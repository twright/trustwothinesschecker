use futures::{stream::LocalBoxStream, StreamExt};
// mod ast;
// mod async_runtime;
// mod constraint_based_runtime;
// mod constraint_solver;
// mod core;
// mod queuing_runtime;
// mod monitoring_semantics;
// mod parser;
// mod ring_buffer;
// mod untimed_monitoring_combinators;
use trustworthiness_checker::{
    self as tc, file_input_provider, parse_file, InputProvider, Monitor, VarName,
};

use clap::{Parser, ValueEnum};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Language {
    /// LOLA + Eval language
    Lola,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Semantics {
    Untimed,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Runtime {
    Async,
    Queuing,
    Constraints,
}

#[derive(Parser)]
struct Cli {
    model: String,
    input_file: String,

    #[arg(long)]
    language: Option<Language>,
    #[arg(long)]
    semantics: Option<Semantics>,
    #[arg(long)]
    runtime: Option<Runtime>,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    // let model = std::fs::read_to_string(cli.model).expect("Model file could not be read");
    let input_file = cli.input_file;

    let language = cli.language.unwrap_or(Language::Lola);
    let semantics = cli.semantics.unwrap_or(Semantics::Untimed);
    let runtime = cli.runtime.unwrap_or(Runtime::Async);

    let model_parser = match language {
        Language::Lola => tc::parser::lola_specification,
    };
    let input_file_parser = match language {
        Language::Lola => tc::parser::lola_input_file,
    };
    let input_streams = tc::parse_file(input_file_parser, &input_file)
        .await
        .expect("Input file could not be parsed");

    let model = parse_file(model_parser, cli.model.as_str())
        .await
        .expect("Model file could not be parsed");

    // println!("Outputs: {:?}", model.output_vars);
    // println!("Inputs: {:?}", model.input_vars);
    // println!("Model: {:?}", model);

    // TODO: This code is rather repetitive, but currently cannot be refactored
    // in the obvious way because of the types involved
    // That is, the type of the runner is not object safe so cannot be stored
    // outside of the scope of a match clause, and the lifetime of
    // the tokio threads feeding the output_streams is tied to the lifetime of
    // the runner object. This should be refactorable after some adjustment to
    // the types involved or the lifetimes of the
    let mut enumerated_outputs = match (runtime, semantics) {
        (Runtime::Async, Semantics::Untimed) => {
            let mut runner = tc::AsyncMonitorRunner::<_, tc::UntimedLolaSemantics, _, _>::new(
                model,
                input_streams,
            );
            runner.monitor_outputs().enumerate()
        }
        (Runtime::Queuing, Semantics::Untimed) => {
            let mut runner = tc::queuing_runtime::QueuingMonitorRunner::<
                _,
                tc::UntimedLolaSemantics,
                _,
                _,
            >::new(model, input_streams);
            runner.monitor_outputs().enumerate()
        }
        (Runtime::Constraints, Semantics::Untimed) => {
            let mut runner =
                tc::constraint_based_runtime::ConstraintBasedMonitor::new(model, input_streams);
            runner.monitor_outputs().enumerate()
        }
    };
    while let Some((i, output)) = enumerated_outputs.next().await {
        for (var, data) in output {
            println!("{}[{}] = {:?}", var, i, data);
        }
    }
}
