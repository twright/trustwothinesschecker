use core::panic;
use std::{arch::x86_64, borrow::Borrow, rc::Rc, sync::Arc};

use futures::{
    future,
    stream::{self, BoxStream, LocalBoxStream, Stream},
    StreamExt,
};
use winnow::Str;

use crate::ast::*;
use dyn_clone::DynClone;

// Define a sequence of traits which define the inputs and outputs
// of monitor combinators
pub trait InputFn: Fn(VarName) -> LocalBoxStream<'static, StreamData> + DynClone + 'static {}
dyn_clone::clone_trait_object!(InputFn);

impl<T> InputFn for T where
    T: Fn(VarName) -> LocalBoxStream<'static, StreamData> + DynClone + 'static
{
}

pub trait OutputFn<IF>: Fn(IF) -> LocalBoxStream<'static, StreamData> + DynClone + 'static {
    type IF: InputFn;
}

impl<IF, F> OutputFn<IF> for F
where
    F: Fn(IF) -> LocalBoxStream<'static, StreamData> + DynClone + 'static,
    IF: InputFn,
{
    type IF = IF;
}
dyn_clone::clone_trait_object!(<IF:InputFn>OutputFn<IF,IF=IF>);

pub trait CloneFn1: Fn(StreamData) -> StreamData + Clone + 'static {}
impl<T> CloneFn1 for T where T: Fn(StreamData) -> StreamData + Clone + 'static {}

pub fn lift1<IF>(
    f: impl CloneFn1,
    x_mon: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    let f = f.clone();
    let x_mon = x_mon.clone();

    Box::new(move |in_fn: IF| {
        let f = f.clone();
        let in_fn = *dyn_clone::clone_box(&in_fn);
        Box::pin(x_mon(in_fn).map(move |x| f(x))) as LocalBoxStream<'static, StreamData>
    })
}

pub trait CloneFn2: Fn(StreamData, StreamData) -> StreamData + Clone + 'static {}
impl<T> CloneFn2 for T where T: Fn(StreamData, StreamData) -> StreamData + Clone + 'static {}

pub fn lift2<IF>(
    f: impl CloneFn2,
    x_mon: Box<dyn OutputFn<IF, IF = IF>>,
    y_mon: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    let f = f.clone();
    let x_mon = x_mon.clone();
    let y_mon = y_mon.clone();

    Box::new(move |in_fn: IF| {
        let f = f.clone();
        let in_fn1 = *dyn_clone::clone_box(&in_fn);
        let in_fn2 = *dyn_clone::clone_box(&in_fn);
        Box::pin(x_mon(in_fn1).zip(y_mon(in_fn2)).map(move |(x, y)| f(x, y)))
            as LocalBoxStream<'static, StreamData>
    })
}

pub trait CloneFn3: Fn(StreamData, StreamData, StreamData) -> StreamData + Clone + 'static {}
impl<T> CloneFn3 for T where
    T: Fn(StreamData, StreamData, StreamData) -> StreamData + Clone + 'static
{
}

pub fn lift3<IF>(
    f: impl CloneFn3,
    x_mon: Box<dyn OutputFn<IF, IF = IF>>,
    y_mon: Box<dyn OutputFn<IF, IF = IF>>,
    z_mon: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    let f = f.clone();
    let x_mon = x_mon.clone();
    let y_mon = y_mon.clone();
    let z_mon = z_mon.clone();

    Box::new(move |in_fn: IF| {
        let f = f.clone();
        let in_fn1 = *dyn_clone::clone_box(&in_fn);
        let in_fn2 = *dyn_clone::clone_box(&in_fn);
        let in_fn3 = *dyn_clone::clone_box(&in_fn);
        Box::pin(
            x_mon(in_fn1)
                .zip(y_mon(in_fn2))
                .zip(z_mon(in_fn3))
                .map(move |((x, y), z)| f(x, y, z)),
        ) as LocalBoxStream<'static, StreamData>
    })
}
pub fn and<IF>(
    x: Box<dyn OutputFn<IF, IF = IF>>,
    y: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    lift2(
        |x, y| StreamData::Bool(x == StreamData::Bool(true) && y == StreamData::Bool(true)),
        x,
        y,
    )
}

pub fn or<IF>(
    x: Box<dyn OutputFn<IF, IF = IF>>,
    y: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    lift2(
        |x, y| StreamData::Bool(x == StreamData::Bool(true) || y == StreamData::Bool(true)),
        x,
        y,
    )
}

pub fn not<IF>(x: Box<dyn OutputFn<IF, IF = IF>>) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    lift1(|x| StreamData::Bool(x == StreamData::Bool(true)), x)
}

pub fn eq<IF>(
    x: Box<dyn OutputFn<IF, IF = IF>>,
    y: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    lift2(|x, y| StreamData::Bool(x == y), x, y)
}

pub fn le<IF>(
    x: Box<dyn OutputFn<IF, IF = IF>>,
    y: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    lift2(
        |x, y| match (x, y) {
            (StreamData::Int(x), StreamData::Int(y)) => StreamData::Bool(x <= y),
            (StreamData::Bool(a), StreamData::Bool(b)) => StreamData::Bool(a <= b),
            _ => panic!("Invalid comparison"),
        },
        x,
        y,
    )
}

pub fn val<IF>(x: StreamData) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    Box::new(move |_: IF| Box::pin(stream::repeat(x.clone())) as BoxStream<'static, StreamData>)
}

pub fn if_stm<IF>(
    x: Box<dyn OutputFn<IF, IF = IF>>,
    y: Box<dyn OutputFn<IF, IF = IF>>,
    z: Box<dyn OutputFn<IF, IF = IF>>,
) -> impl OutputFn<IF>
where
    IF: InputFn,
{
    lift3(
        |x, y, z| match x {
            StreamData::Bool(true) => y,
            StreamData::Bool(false) => z,
            _ => panic!("Invalid if condition"),
        },
        x,
        y,
        z,
    )
}

pub fn index<IF>(
    x: Box<dyn OutputFn<IF, IF = IF>>,
    i: isize,
    c: StreamData,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    let c = c.clone();
    Box::new(move |in_fn: IF| {
        let c = c.clone();
        let in_fn = *dyn_clone::clone_box(&in_fn);
        if i < 0 {
            let n: usize = (-i).try_into().unwrap();
            let cs = stream::repeat(c).take(n);
            Box::pin(cs.chain(x(in_fn)))
        } else {
            let n: usize = i.try_into().unwrap();
            Box::pin(x(in_fn).skip(n))
        }
    })
}

pub fn plus<IF>(
    x: Box<dyn OutputFn<IF, IF = IF>>,
    y: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    lift2(
        |x, y| match (x, y) {
            (StreamData::Int(x), StreamData::Int(y)) => StreamData::Int(x + y),
            _ => panic!("Invalid addition"),
        },
        x,
        y,
    )
}

pub fn minus<IF>(
    x: Box<dyn OutputFn<IF, IF = IF>>,
    y: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    lift2(
        |x, y| match (x, y) {
            (StreamData::Int(x), StreamData::Int(y)) => StreamData::Int(x - y),
            _ => panic!("Invalid subtraction"),
        },
        x,
        y,
    )
}

pub fn mult<IF>(
    x: Box<dyn OutputFn<IF, IF = IF>>,
    y: Box<dyn OutputFn<IF, IF = IF>>,
) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    lift2(
        |x, y| match (x, y) {
            (StreamData::Int(x), StreamData::Int(y)) => StreamData::Int(x * y),
            _ => panic!("Invalid multiplication"),
        },
        x,
        y,
    )
}

pub fn eval<IF>(x: Box<dyn OutputFn<IF, IF = IF>>) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    unimplemented!("eval not implemented")
}

pub fn var<IF>(x: VarName) -> Box<dyn OutputFn<IF, IF = IF>>
where
    IF: InputFn,
{
    Box::new(move |in_fn: IF| {
        let in_fn = *dyn_clone::clone_box(&in_fn);
        Box::pin(in_fn(x.clone())) as LocalBoxStream<'static, StreamData>
    })
}
