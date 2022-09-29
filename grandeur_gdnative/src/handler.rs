use std::borrow::Borrow;
use std::fmt::{self, Debug};
use std::marker::PhantomData;
use std::sync::Arc;

use gdnative::prelude::{Node, Ref, Variant};

pub trait SignalHandler<M>: Send + Sync {
    fn handle(&self, node: Ref<Node>, args: Vec<Variant>) -> Result<M, Box<dyn std::error::Error>>;

    fn map_msg<F, G, R>(self, g: G) -> Mapped<Self, F, G, M, R>
    where
        Self: Sized,
        F: Fn(M) -> R,
        G: Borrow<F>,
    {
        Mapped {
            handler: self,
            g,
            _marker: PhantomData,
        }
    }
}

impl<M> SignalHandler<M> for Arc<dyn SignalHandler<M>> {
    fn handle(&self, node: Ref<Node>, args: Vec<Variant>) -> Result<M, Box<dyn std::error::Error>> {
        <dyn SignalHandler<M>>::handle(&**self, node, args)
    }
}

impl<M> Debug for dyn SignalHandler<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "dyn SignalHandler")
    }
}

impl<M> SignalHandler<M> for M
where
    M: Clone + Send + Sync,
{
    fn handle(
        &self,
        _node: Ref<Node>,
        _args: Vec<Variant>,
    ) -> Result<M, Box<dyn std::error::Error>> {
        Ok(self.clone())
    }
}

pub struct Mapped<H, F, G, M, R> {
    handler: H,
    g: G,

    #[allow(clippy::type_complexity)]
    _marker: PhantomData<(F, fn(M) -> R)>,
}

impl<H, F, G, M, R> SignalHandler<R> for Mapped<H, F, G, M, R>
where
    H: SignalHandler<M>,
    F: Send + Sync + Fn(M) -> R,
    G: Borrow<F> + Send + Sync,
{
    fn handle(&self, node: Ref<Node>, args: Vec<Variant>) -> Result<R, Box<dyn std::error::Error>> {
        self.handler.handle(node, args).map(self.g.borrow())
    }
}

pub struct Nullary<F, M> {
    f: F,
    _marker: PhantomData<fn() -> M>,
}

impl<F, M> Nullary<F, M>
where
    F: Fn() -> M,
{
    pub fn new(f: F) -> Self {
        Nullary {
            f,
            _marker: PhantomData,
        }
    }
}

impl<F, M> From<F> for Nullary<F, M>
where
    F: Fn() -> M,
{
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

impl<F, M> SignalHandler<M> for Nullary<F, M>
where
    F: Send + Sync + Fn() -> M,
{
    fn handle(
        &self,
        _node: Ref<Node>,
        _args: Vec<Variant>,
    ) -> Result<M, Box<dyn std::error::Error>> {
        Ok((self.f)())
    }
}

pub struct Ary<F, M> {
    f: F,
    _marker: PhantomData<fn() -> M>,
}

impl<F, M> Ary<F, M>
where
    F: Fn(Vec<Variant>) -> Result<M, Box<dyn std::error::Error>>,
{
    pub fn new(f: F) -> Self {
        Ary {
            f,
            _marker: PhantomData,
        }
    }
}

impl<F, M> From<F> for Ary<F, M>
where
    F: Fn(Vec<Variant>) -> Result<M, Box<dyn std::error::Error>>,
{
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

impl<F, M> SignalHandler<M> for Ary<F, M>
where
    F: Send + Sync + Fn(Vec<Variant>) -> Result<M, Box<dyn std::error::Error>>,
{
    fn handle(
        &self,
        _node: Ref<Node>,
        args: Vec<Variant>,
    ) -> Result<M, Box<dyn std::error::Error>> {
        (self.f)(args)
    }
}

pub struct SourceAware<F, M> {
    f: F,
    _marker: PhantomData<fn() -> M>,
}

impl<F, M> SourceAware<F, M>
where
    F: Send + Sync + Fn(Ref<Node>, Vec<Variant>) -> Result<M, Box<dyn std::error::Error>>,
{
    pub fn new(f: F) -> Self {
        SourceAware {
            f,
            _marker: PhantomData,
        }
    }
}

impl<F, M> From<F> for SourceAware<F, M>
where
    F: Send + Sync + Fn(Ref<Node>, Vec<Variant>) -> Result<M, Box<dyn std::error::Error>>,
{
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

impl<F, M> SignalHandler<M> for SourceAware<F, M>
where
    F: Send + Sync + Fn(Ref<Node>, Vec<Variant>) -> Result<M, Box<dyn std::error::Error>>,
{
    fn handle(&self, node: Ref<Node>, args: Vec<Variant>) -> Result<M, Box<dyn std::error::Error>> {
        (self.f)(node, args)
    }
}
