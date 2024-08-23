use core::panic;

use futures::{
    future,
    stream::{self, BoxStream, LocalBoxStream, Stream},
    StreamExt,
};

use crate::ast::*;

// type InputFn = dyn Fn(VarName, Option<usize>) -> BoxStream<'static, StreamData>;
// type MonitorFn = dyn Fn(&InputFn) -> BoxStream<'static, StreamData>;

// Define a sequence of traits which define the inputs and outputs
// of monitor combinators

pub trait InputFn:
    Fn(VarName) -> BoxStream<'static, StreamData> + Clone + Sync + 'static
{
}
pub trait OutputFn<IF>: Fn(IF) -> BoxStream<'static, StreamData> + Clone + Sync + 'static {
    type IF: InputFn;
}

impl<IF, F> OutputFn<IF> for F
where
    F: Fn(IF) -> BoxStream<'static, StreamData> + Clone + Sync + 'static,
    IF: InputFn,
{
    type IF = IF;
}

// Define traits for monitor combinators
pub trait CompMonitorFn1<A, IF>: Fn(A) -> <Self as CompMonitorFn1<A, IF>>::Ret {
    type Ret: OutputFn<IF>;
}

impl<A, F, RT, IF> CompMonitorFn1<A, IF> for F
where
    F: Fn(A) -> RT,
    RT: OutputFn<IF>,
{
    type Ret = RT;
}

pub trait CompMonitorFn2<A, B, IF>: Fn(A, B) -> <Self as CompMonitorFn2<A, B, IF>>::Ret {
    type Ret: OutputFn<IF>;
}

impl<A, B, F, RT, IF> CompMonitorFn2<A, B, IF> for F
where
    F: Fn(A, B) -> RT,
    RT: OutputFn<IF>,
{
    type Ret = RT;
}

pub trait CompMonitorFn3<A, B, C, IF>:
    Fn(A, B, C) -> <Self as CompMonitorFn3<A, B, C, IF>>::Ret
{
    type Ret: OutputFn<IF>;
}

impl<A, B, C, F, RT, IF> CompMonitorFn3<A, B, C, IF> for F
where
    F: Fn(A, B, C) -> RT,
    RT: OutputFn<IF>,
{
    type Ret = RT;
}

// The type for this needs to be defined indirectly due to needing
// nested impls which rely on the unstable feature
// https://github.com/rust-lang/rust/pull/93582
pub fn lift1<A, F, IF>(f: F) -> impl CompMonitorFn1<A, IF>
where
    F: Fn(StreamData) -> StreamData + Clone + Send + Sync + 'static,
    A: OutputFn<IF>,
    IF: InputFn,
{
    // We need to clone into the closure at each stage to
    // ensure that the closure is Fn
    // see: https://internals.rust-lang.org/t/feature-request-add-clone-closure/15484
    let f = f.clone();
    move |x_mon| {
        let f = f.clone();
        let x_mon = x_mon.clone();
        move |in_fn: IF| {
            Box::pin(x_mon(in_fn.clone()).map(f.clone())) as BoxStream<'static, StreamData>
        }
    }
}

pub fn lift2<A, B, F, IF>(f: F) -> impl CompMonitorFn2<A, B, IF>
where
    F: Fn(StreamData, StreamData) -> StreamData + Copy + Send + Sync + 'static,
    A: OutputFn<IF>,
    B: OutputFn<IF>,
    IF: InputFn,
{
    let f = f.clone();
    move |x_mon, y_mon| {
        let x_mon = x_mon.clone();
        let y_mon = y_mon.clone();
        move |in_fn: IF| {
            Box::pin(
                x_mon(in_fn.clone())
                    .zip(y_mon(in_fn))
                    .map(move |(x, y)| f(x, y)),
            ) as BoxStream<'static, StreamData>
        }
    }
}

pub fn lift3<A, B, C, F, IF>(f: F) -> impl CompMonitorFn3<A, B, C, IF>
where
    F: Fn(StreamData, StreamData, StreamData) -> StreamData + Copy + Send + Sync + 'static,
    A: OutputFn<IF>,
    B: OutputFn<IF>,
    C: OutputFn<IF>,
    IF: InputFn,
{
    let f = f.clone();
    move |x_mon, y_mon, z_mon| {
        let x_mon = x_mon.clone();
        let y_mon = y_mon.clone();
        let z_mon = z_mon.clone();
        move |in_fn: IF| {
            Box::pin(
                x_mon(in_fn.clone())
                    .zip(y_mon(in_fn.clone()))
                    .zip(z_mon(in_fn.clone()))
                    .map(move |((x, y), z)| f(x, y, z)),
            ) as BoxStream<'static, StreamData>
        }
    }
}

pub fn and<IF>(x: impl OutputFn<IF>, y: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift2(|x, y| StreamData::Bool(x == StreamData::Bool(true) && y == StreamData::Bool(true)))(x, y)
}

pub fn or<IF>(x: impl OutputFn<IF>, y: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift2(|x, y| StreamData::Bool(x == StreamData::Bool(true) || y == StreamData::Bool(true)))(x, y)
}

pub fn not<IF>(x: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift1(|x| StreamData::Bool(x == StreamData::Bool(false)))(x)
}

pub fn eq<IF>(x: impl OutputFn<IF>, y: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift2(|x, y| StreamData::Bool(x == y))(x, y)
}

pub fn le<IF>(x: impl OutputFn<IF>, y: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift2(|x, y| match (x, y) {
        (StreamData::Int(x), StreamData::Int(y)) => StreamData::Bool(x <= y),
        (StreamData::Bool(a), StreamData::Bool(b)) => StreamData::Bool(a <= b),
        _ => panic!("Invalid comparison"),
    })(x, y)
}

pub fn val<IF>(x: StreamData) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    move |_: IF| Box::pin(stream::repeat(x.clone())) as BoxStream<'static, StreamData>
}

pub fn if_stm<IF>(x: impl OutputFn<IF>, y: impl OutputFn<IF>, z: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift3(|x, y, z| match x {
        StreamData::Bool(true) => y,
        StreamData::Bool(false) => z,
        _ => panic!("Invalid if condition"),
    })(x, y, z)
}

pub fn index<IF>(x: impl OutputFn<IF>, i: isize, c: StreamData) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    let c = c.clone();
    move |in_fn: IF| {
        let n = -i;
        let c = c.clone();
        Box::pin(if n >= 0 {
            let n: usize = n.try_into().unwrap();
            let cs = stream::repeat(c).take(3);

            cs.chain(x(in_fn.clone()).skip(n))
        } else {
            panic!("Indexing into future")
        }) as BoxStream<'static, StreamData>
    }
}

pub fn plus<IF>(x: impl OutputFn<IF>, y: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift2(|x, y| match (x, y) {
        (StreamData::Int(x), StreamData::Int(y)) => StreamData::Int(x + y),
        _ => panic!("Invalid addition"),
    })(x, y)
}

pub fn minus<IF>(x: impl OutputFn<IF>, y: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift2(|x, y| match (x, y) {
        (StreamData::Int(x), StreamData::Int(y)) => StreamData::Int(x - y),
        _ => panic!("Invalid subtraction"),
    })(x, y)
}

pub fn mult<IF>(x: impl OutputFn<IF>, y: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift2(|x, y| match (x, y) {
        (StreamData::Int(x), StreamData::Int(y)) => StreamData::Int(x * y),
        _ => panic!("Invalid multiplication"),
    })(x, y)
}

pub fn eval<IF>(x: impl OutputFn<IF>) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    // TODO: Figure out how to properly implement eval
    let x = x.clone();
    x
}

pub fn var<IF>(x: VarName) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    move |in_fn: IF| {
        Box::pin(in_fn(x.clone()))
        as BoxStream<'static, StreamData>
    }
}