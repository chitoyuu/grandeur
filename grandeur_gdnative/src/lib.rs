#![cfg_attr(
    feature = "nightly",
    feature(type_alias_impl_trait, never_type)
)]

use std::fmt::Display;

pub use crossbeam_channel as channel;
pub use grandeur;

#[cfg(feature = "macros")]
pub use grandeur_gdnative_macros as macros;

pub mod handler;
pub mod prop;
pub mod renderer;
pub mod repo;
pub mod vnode;

mod symbols;

/// Specialized alias for virtual scene tree nodes.
pub type VTreeNode<M> = grandeur::Node<vnode::VNode<M>>;

/// Alias for a "fragment", a.k.a. a vector of nodes to be positioned under a common parent.
pub type Fragment<M> = Vec<VTreeNode<M>>;

use gdnative::prelude::{GodotObject, InitHandle, Node, SubClass};

pub trait Msg: 'static + Send {}
impl<M> Msg for M where M: 'static + Send {}

pub struct RegisterBuilder<'a> {
    handle: &'a InitHandle,
    tool: bool,
}

impl<'a> RegisterBuilder<'a> {
    pub fn with_renderer<N, M: Msg>(&self, name: impl Display) -> &Self
    where
        N: GodotObject + SubClass<Node> + 'static,
    {
        renderer::Renderer::<N, M>::register_as(self.handle, name, self.tool);
        self
    }
}

pub fn register(handle: &InitHandle, tool: bool) -> RegisterBuilder<'_> {
    if tool {
        handle.add_tool_class::<renderer::NodeMeta>();
    } else {
        handle.add_class::<renderer::NodeMeta>();
    }

    RegisterBuilder { handle, tool }
}
