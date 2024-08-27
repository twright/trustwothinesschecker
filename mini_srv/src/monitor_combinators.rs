use core::panic;
use std::sync::{Arc, Weak};

use futures::{
    stream::{self, BoxStream},
    StreamExt,
};
use winnow::Str;

use crate::ast::*;

pub type OutputStream = BoxStream<'static, StreamData>;

pub trait StreamContext {
    fn var(&self, x: &VarName) -> Option<OutputStream>;
}

pub trait CloneFn1: Fn(StreamData) -> StreamData + Clone + Sync + Send + 'static {}
impl<T> CloneFn1 for T where T: Fn(StreamData) -> StreamData + Sync + Send + Clone + 'static {}

pub fn lift1(f: impl CloneFn1, x_mon: OutputStream) -> OutputStream {
    let f = f.clone();

    Box::pin(x_mon.map(move |x| f(x)))
}

pub trait CloneFn2:
    Fn(StreamData, StreamData) -> StreamData + Clone + Sync + Send + 'static
{
}
impl<T> CloneFn2 for T where
    T: Fn(StreamData, StreamData) -> StreamData + Clone + Sync + Send + 'static
{
}

pub fn lift2(f: impl CloneFn2, x_mon: OutputStream, y_mon: OutputStream) -> OutputStream {
    let f = f.clone();
    Box::pin(x_mon.zip(y_mon).map(move |(x, y)| f(x, y)))
}

pub trait CloneFn3:
    Fn(StreamData, StreamData, StreamData) -> StreamData + Clone + Sync + Send + 'static
{
}
impl<T> CloneFn3 for T where
    T: Fn(StreamData, StreamData, StreamData) -> StreamData + Clone + Sync + Send + 'static
{
}

pub fn lift3(
    f: impl CloneFn3,
    x_mon: OutputStream,
    y_mon: OutputStream,
    z_mon: OutputStream,
) -> OutputStream {
    let f = f.clone();

    Box::pin(
        x_mon
            .zip(y_mon)
            .zip(z_mon)
            .map(move |((x, y), z)| f(x, y, z)),
    ) as BoxStream<'static, StreamData>
}

pub fn and(x: OutputStream, y: OutputStream) -> OutputStream {
    lift2(
        |x, y| StreamData::Bool(x == StreamData::Bool(true) && y == StreamData::Bool(true)),
        x,
        y,
    )
}

pub fn or(x: OutputStream, y: OutputStream) -> OutputStream {
    lift2(
        |x, y| StreamData::Bool(x == StreamData::Bool(true) || y == StreamData::Bool(true)),
        x,
        y,
    )
}

pub fn not(x: OutputStream) -> OutputStream {
    lift1(|x| StreamData::Bool(x == StreamData::Bool(true)), x)
}

pub fn eq(x: OutputStream, y: OutputStream) -> OutputStream {
    lift2(|x, y| StreamData::Bool(x == y), x, y)
}

pub fn le(x: OutputStream, y: OutputStream) -> OutputStream {
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

pub fn val(x: StreamData) -> OutputStream {
    Box::pin(stream::repeat(x.clone()))
}

pub fn if_stm(x: OutputStream, y: OutputStream, z: OutputStream) -> OutputStream {
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

pub fn index(x: OutputStream, i: isize, c: StreamData) -> OutputStream {
    let c = c.clone();
    if i < 0 {
        let n: usize = (-i).try_into().unwrap();
        let cs = stream::repeat(c).take(n);
        Box::pin(cs.chain(x)) as BoxStream<'static, StreamData>
    } else {
        let n: usize = i.try_into().unwrap();
        Box::pin(x.skip(n)) as BoxStream<'static, StreamData>
    }
}

pub fn plus(x: OutputStream, y: OutputStream) -> OutputStream {
    lift2(
        |x, y| match (x, y) {
            (StreamData::Int(x), StreamData::Int(y)) => StreamData::Int(x + y),
            _ => panic!("Invalid addition"),
        },
        x,
        y,
    )
}

pub fn minus(x: OutputStream, y: OutputStream) -> OutputStream {
    lift2(
        |x, y| match (x, y) {
            (StreamData::Int(x), StreamData::Int(y)) => StreamData::Int(x - y),
            _ => panic!("Invalid subtraction"),
        },
        x,
        y,
    )
}

pub fn mult(x: OutputStream, y: OutputStream) -> OutputStream {
    lift2(
        |x, y| match (x, y) {
            (StreamData::Int(x), StreamData::Int(y)) => StreamData::Int(x * y),
            _ => panic!("Invalid multiplication"),
        },
        x,
        y,
    )
}

pub fn eval(x: OutputStream) -> OutputStream {
    unimplemented!("eval not implemented")
}

pub fn var(ctx: &impl StreamContext, x: VarName) -> OutputStream {
    match ctx.var(&x) {
        Some(x) => x,
        None => {
            let VarName(x) = x;
            panic!("Variable {} not found", x)
        }
    }
}
