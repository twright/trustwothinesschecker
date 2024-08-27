use std::collections::BTreeMap;
use std::future::Future;
use std::mem;
use std::pin::Pin;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::Mutex;
use tokio::task::JoinHandle;

use futures::future::join_all;
use futures::stream;
use futures::stream::BoxStream;
use futures::stream::LocalBoxStream;
use futures::StreamExt;
use tokio::sync::broadcast::{channel, Receiver, Sender};

use crate::ast::*;
use crate::monitor_combinators as mc;
use crate::monitor_combinators::{OutputStream, StreamContext};
use crate::SExprConstraintStore;

pub trait Monitorable {
    fn to_async_stream(self, ctx: &impl StreamContext) -> OutputStream;
}

impl Monitorable for SExpr<VarName> {
    fn to_async_stream(self, ctx: &impl StreamContext) -> OutputStream {
        match self {
            SExpr::Val(v) => mc::val(v),
            SExpr::Plus(e1, e2) => {
                let e1 = e1.to_async_stream(ctx);
                let e2 = e2.to_async_stream(ctx);
                mc::plus(e1, e2)
            }
            SExpr::Minus(e1, e2) => {
                let e1 = e1.to_async_stream(ctx);
                let e2 = e2.to_async_stream(ctx);
                mc::minus(e1, e2)
            }
            SExpr::Mult(e1, e2) => {
                let e1 = e1.to_async_stream(ctx);
                let e2 = e2.to_async_stream(ctx);
                mc::mult(e1, e2)
            }
            SExpr::Var(v) => mc::var(ctx, v),
            SExpr::Eval(e) => {
                let e = e.to_async_stream(ctx);
                mc::eval(e)
            }
            SExpr::Index(e, i, c) => {
                let e = e.to_async_stream(ctx);
                mc::index(e, i, c)
            }
            SExpr::If(b, e1, e2) => {
                let b = b.to_async_stream(ctx);
                let e1 = e1.to_async_stream(ctx);
                let e2 = e2.to_async_stream(ctx);
                mc::if_stm(b, e1, e2)
            }
        }
    }
}

impl Monitorable for BExpr<VarName> {
    fn to_async_stream(self, ctx: &impl StreamContext) -> OutputStream {
        match self {
            BExpr::And(b1, b2) => {
                let b1 = b1.to_async_stream(ctx);
                let b2 = b2.to_async_stream(ctx);
                mc::and(b1, b2)
            }
            BExpr::Or(b1, b2) => {
                let b1 = b1.to_async_stream(ctx);
                let b2 = b2.to_async_stream(ctx);
                mc::or(b1, b2)
            }
            BExpr::Not(b) => {
                let b = b.to_async_stream(ctx);
                mc::not(b)
            }
            BExpr::Eq(e1, e2) => {
                let e1 = e1.to_async_stream(ctx);
                let e2 = e2.to_async_stream(ctx);
                mc::eq(e1, e2)
            }
            BExpr::Le(e1, e2) => {
                let e1 = e1.to_async_stream(ctx);
                let e2 = e2.to_async_stream(ctx);
                mc::le(e1, e2)
            }
            BExpr::Val(b) => mc::val(StreamData::Bool(b)),
        }
    }
}

struct AsyncVarExchange {
    senders: BTreeMap<VarName, Arc<Mutex<Sender<StreamData>>>>,
}

impl AsyncVarExchange {
    fn new(vars: Vec<&VarName>) -> Self {
        let mut senders = BTreeMap::new();

        for var in vars {
            let (sender, _) = channel::<StreamData>(100);
            senders.insert(var, Arc::new(Mutex::new(sender)));
        }

        AsyncVarExchange {
            senders: BTreeMap::new(),
        }
    }

    fn publish(&self, var: &VarName, data: StreamData) {
        let sender = self.senders.get(&var).unwrap();
        sender.lock().unwrap().send(data).unwrap();
    }
}

impl StreamContext for AsyncVarExchange {
    fn var(&self, var: &VarName) -> Option<OutputStream> {
        match self.senders.get(&var) {
            Some(sender) => {
                let receiver = sender.lock().unwrap().subscribe();
                Some(Box::pin(stream::unfold(
                    receiver,
                    |mut receiver| async move {
                        if let Ok(res) = receiver.recv().await {
                            Some((res, receiver))
                        } else {
                            None
                        }
                    },
                )))
            }
            None => None,
        }
    }
}

impl<Ctx: StreamContext> StreamContext for Arc<Ctx> {
    fn var(&self, var: &VarName) -> Option<OutputStream> {
        self.as_ref().var(var)
    }
}

pub struct AsyncMonitor {
    input_streams: Arc<Mutex<BTreeMap<VarName, BoxStream<'static, StreamData>>>>,
    input_vars: Vec<VarName>,
    output_vars: Vec<VarName>,
    system: SExprConstraintStore<VarName>,
    var_exchange: Arc<AsyncVarExchange>,
    tasks: Option<Vec<Pin<Box<dyn Future<Output = ()> + Send>>>>,
    output_streams: BTreeMap<VarName, OutputStream>,
}

impl AsyncMonitor {
    pub fn new(
        input_streams: BTreeMap<VarName, BoxStream<'static, StreamData>>,
        input_vars: Vec<VarName>,
        output_vars: Vec<VarName>,
        system: SExprConstraintStore<VarName>,
    ) -> Self {
        let var_names = input_vars.iter().chain(output_vars.iter()).collect();

        let var_exchange = Arc::new(AsyncVarExchange::new(var_names));

        let input_streams = Arc::new(Mutex::new(input_streams));

        let mut output_streams = BTreeMap::new();

        AsyncMonitor {
            input_streams,
            input_vars,
            output_vars,
            system,
            var_exchange,
            tasks: None,
            output_streams,
        }
        .build_tasks()
    }

    fn monitor_input(&self, var: VarName) -> impl Future<Output = ()> {
        let var_exchange = self.var_exchange.clone();
        let mut input = self.input_streams.lock().unwrap().remove(&var).unwrap();

        async move {
            while let Some(data) = input.next().await {
                var_exchange.publish(&var, data);
            }
        }
    }

    fn monitor_output(&self, var: VarName) -> impl Future<Output = ()> {
        let var_exchange = self.var_exchange.clone();

        let mut output = if let Some(data) = self
            .system
            .resolved
            .iter()
            .filter(|(k, _)| k == &var)
            .map(|(_, v)| v)
            .next()
        {
            mc::val(data.clone())
        } else if let Some(expr) = self
            .system
            .unresolved
            .iter()
            .filter(|(k, _)| k == &var)
            .map(|(_, v)| v)
            .next()
        {
            expr.clone().to_async_stream(&var_exchange)
        } else {
            unimplemented!()
        };

        async move {
            while let Some(data) = output.next().await {
                var_exchange.publish(&var, data);
            }
        }
    }

    // Define futures which monitors each of the input and output variables
    // as well as output streams which can be used to subscribe to the output
    // Note that the numbers of subscribers to each broadcast channel is
    // determined when this function is run
    fn build_tasks(mut self) -> Self {
        let mut tasks: Vec<Pin<Box<dyn Future<Output = ()> + Send>>> = vec![];

        for var in self.input_vars.iter() {
            tasks.push(Box::pin(self.monitor_input(var.clone())));

            // Add output steams that just echo the inputs
            // self.output_streams
            // .insert(var.clone(), mc::var(&self.var_exchange, var.clone()));
        }

        for var in self.output_vars.iter() {
            tasks.push(Box::pin(self.monitor_output(var.clone())));

            // Add output steams that subscribe to the inputs
            self.output_streams
                .insert(var.clone(), mc::var(&self.var_exchange, var.clone()));
        }

        self.tasks = Some(tasks);
        self
    }

    pub fn spawn(&mut self) -> Vec<JoinHandle<()>> {
        let mut tasks: Vec<Pin<Box<dyn Future<Output = ()> + Send>>> =
            mem::take(&mut self.tasks).unwrap();

        let mut handles = vec![];

        for task in tasks.into_iter() {
            handles.push(tokio::spawn(task));
        }

        handles
    }

    pub fn monitor_outputs(&mut self) -> BoxStream<'static, BTreeMap<VarName, StreamData>> {
        let mut output_streams = mem::take(&mut self.output_streams);
        let outputs = self.output_vars.clone();

        Box::pin(stream::unfold(
            (output_streams, outputs),
            |(mut output_streams, outputs)| async move {
                let mut futures = vec![];
                for (_, stream) in output_streams.iter_mut() {
                    futures.push(stream.next());
                }

                let next_vals = join_all(futures).await;
                let mut res: BTreeMap<VarName, StreamData> = BTreeMap::new();
                for (var, val) in outputs.clone().iter().zip(next_vals) {
                    res.insert(
                        var.clone(),
                        match val {
                            Some(val) => val,
                            None => return None,
                        },
                    );
                }
                return Some((res, (output_streams, outputs)));
            },
        )) as BoxStream<'static, BTreeMap<VarName, StreamData>>
    }
}
