use std::any::Any;
use std::borrow::Cow;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use derivative::Derivative;
use gdnative::api::packed_scene::GenEditState;
use gdnative::api::Node;
use gdnative::object::ownership::Unique;
use gdnative::object::GodotObject;
use gdnative::object::Ref;
use gdnative::prelude::{GodotString, OwnedToVariant, TRef, Variant};
use hashbrown::HashMap;
use indexmap::IndexMap;
use thiserror::Error;

use crate::handler::{Mapped, SignalHandler};
use crate::prop::Prop;
use crate::repo::{self, SceneRepo};
use crate::symbols::Symbols;
use crate::Msg;

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct VNode<M> {
    pub(crate) kind: Arc<dyn VNodeKind>,
    pub(crate) prefer_as_is: bool,
    pub(crate) key: Option<Arc<dyn AnyEqHash>>,
    pub(crate) props: IndexMap<Prop, Variant, ahash::RandomState>,
    pub(crate) subs: HashMap<String, Arc<dyn SignalHandler<M>>, ahash::RandomState>,
}

impl<M> Debug for VNode<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VNode")
            .field("kind", &self.kind)
            .field("key", &self.key)
            .finish_non_exhaustive()
    }
}

impl<M: Msg> VNode<M> {
    pub fn new<K: VNodeKind + 'static>(kind: K) -> Self {
        VNode {
            kind: Arc::new(kind),
            prefer_as_is: false,
            key: None,
            props: IndexMap::default(),
            subs: HashMap::default(),
        }
    }

    /// Sets a flag that instructs the conciliator to ignore this node and the subtree
    /// in favor of previously rendered nodes in the scene, if any matching ones are found.
    ///
    /// Defaults to `false`. When set to `true`, the subtree with this node as the root will
    /// only be rendered if no such matching trees are found in the scene.
    ///
    /// This should usually only be used when it is necessary to keep some part of the scene
    /// for which the Rust state is gone for some reason. For example, in a multiplayer game
    /// while waiting for network state to be brought back into sync.
    pub fn prefer_as_is(mut self, value: bool) -> Self {
        self.prefer_as_is = value;
        self
    }

    pub fn key<K: 'static + AnyEqHash>(mut self, key: K) -> Self {
        self.key = Some(Arc::new(key));
        self
    }

    pub fn prop<P, V>(mut self, prop: P, value: V) -> Self
    where
        P: Into<Prop>,
        V: OwnedToVariant,
    {
        self.props.insert(prop.into(), value.owned_to_variant());
        self
    }

    pub fn props<I, P, V>(mut self, it: I) -> Self
    where
        I: IntoIterator<Item = (P, V)>,
        P: Into<Prop>,
        V: OwnedToVariant,
    {
        self.props.extend(
            it.into_iter()
                .map(|(k, v)| (k.into(), v.owned_to_variant())),
        );
        self
    }

    pub fn connect<H>(mut self, signal: &str, handler: H) -> Self
    where
        H: SignalHandler<M> + 'static,
    {
        self.subs.insert(signal.to_owned(), Arc::new(handler));
        self
    }

    pub fn map<F, R>(self, f: &Arc<F>) -> VNode<R>
    where
        R: Msg,
        F: 'static + Send + Sync + Fn(M) -> R,
    {
        let VNode {
            key,
            prefer_as_is,
            kind,
            props,
            subs,
        } = self;

        let subs = subs
            .into_iter()
            .map(|(key, handler)| {
                let handler: Mapped<_, F, _, M, R> = handler.map_msg(Arc::clone(f));
                let handler: Arc<dyn SignalHandler<R>> = Arc::new(handler);
                (key, handler)
            })
            .collect();

        VNode {
            key,
            prefer_as_is,
            kind,
            props,
            subs,
        }
    }
}

/// Trait for types that can be used as the "kind" value of [`VNode`]s. This is a sealed trait
/// with no public API.
pub trait VNodeKind: Debug + Send + Sync + private::Sealed {
    #[doc(hidden)]
    fn expand(
        &self,
        symbols: &Symbols,
        repo: &mut SceneRepo,
    ) -> Result<Ref<Node, Unique>, ExpandError>;

    #[doc(hidden)]
    fn matches(&self, symbols: &Symbols, node: TRef<'_, gdnative::api::Node>) -> bool;
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum ExpandError {
    #[error("`{0}` is not a valid class name")]
    InvalidClassName(Cow<'static, str>),
    #[error("scene `{0}` cannot be load: {1}")]
    SceneUnavailable(String, repo::Error),
    #[error("cannot instance scene")]
    CannotInstance,
    #[error("cannot expand virtual root node for top-level fragments")]
    CannotExpandRootNode,
}

/// Used internally to represent the renderer node, under which top-level fragments are placed.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) struct Root;

impl private::Sealed for Root {}
impl VNodeKind for Root {
    fn expand(
        &self,
        _symbols: &Symbols,
        _repo: &mut SceneRepo,
    ) -> Result<Ref<Node, Unique>, ExpandError> {
        Err(ExpandError::CannotExpandRootNode)
    }

    fn matches(&self, _symbols: &Symbols, _node: TRef<'_, gdnative::api::Node>) -> bool {
        // Assuming that the `Root` type is used correctly by the rest of the library
        true
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Native {
    name: Cow<'static, str>,
}

impl Native {
    pub fn new<T: GodotObject>() -> Self {
        Native {
            name: Cow::Borrowed(T::class_name()),
        }
    }

    /// Tries to create a native node by class name. This will fail at expand time if the name is invalid.
    pub fn by_name(name: String) -> Self {
        Native {
            name: Cow::Owned(name),
        }
    }
}

impl private::Sealed for Native {}
impl VNodeKind for Native {
    fn expand(
        &self,
        _symbols: &Symbols,
        _repo: &mut SceneRepo,
    ) -> Result<Ref<Node, Unique>, ExpandError> {
        Ref::<Node, _>::by_class_name(self.name.as_ref())
            .ok_or_else(|| ExpandError::InvalidClassName(self.name.clone()))
    }

    fn matches(&self, _symbols: &Symbols, node: TRef<'_, gdnative::api::Node>) -> bool {
        node.is_class(self.name.as_ref())
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Scene {
    path: GodotString,
}

impl Scene {
    /// Creates a new node from a resource path.
    ///
    /// # Safety
    ///
    /// The scene must point to a "well-behaving, self-contained" component.
    ///
    /// - Must not send references to `self` or any contained nodes in the subtree to another thread.
    /// - Must not modify the value of the internal meta `_grandeur_src_scene`.
    ///
    /// TODO: define what this actually means.
    pub fn new<S: Into<GodotString>>(path: S) -> Self {
        Scene { path: path.into() }
    }
}

impl private::Sealed for Scene {}
impl VNodeKind for Scene {
    fn expand(
        &self,
        symbols: &Symbols,
        repo: &mut SceneRepo,
    ) -> Result<Ref<Node, Unique>, ExpandError> {
        let res = repo
            .load(self.path.clone())
            .map_err(|e| ExpandError::SceneUnavailable(self.path.to_string(), e))?;

        let node = res
            .instance(GenEditState::DISABLED.into())
            .ok_or(ExpandError::CannotInstance)?;

        // SAFETY: PackedScene::instance should return fresh nodes
        let node = unsafe { node.assume_unique() };
        node.set_meta(symbols.scene_meta.clone(), self.path.clone());

        Ok(node)
    }

    fn matches(&self, symbols: &Symbols, node: TRef<'_, gdnative::api::Node>) -> bool {
        node.has_meta(symbols.scene_meta.clone())
            && node
                .get_meta(symbols.scene_meta.clone(), Variant::nil())
                .try_to::<GodotString>()
                .map_or(false, |path| path == self.path)
    }
}

pub trait AnyEqHash: 'static + Send + Sync + Debug {
    fn as_any(&self) -> &dyn Any;
    fn any_eq(&self, other: &dyn AnyEqHash) -> bool;
    fn any_hash(&self, hasher: &mut dyn Hasher);
}

impl<T> AnyEqHash for T
where
    T: 'static + Eq + Hash + Send + Sync + Debug,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn any_eq(&self, other: &dyn AnyEqHash) -> bool {
        other
            .as_any()
            .downcast_ref::<Self>()
            .map_or(false, |other| self == other)
    }

    fn any_hash(&self, mut hasher: &mut dyn Hasher) {
        self.hash(&mut hasher);
    }
}

impl PartialEq for dyn AnyEqHash {
    fn eq(&self, other: &Self) -> bool {
        self.any_eq(other)
    }
}

impl Eq for dyn AnyEqHash {}

impl Hash for dyn AnyEqHash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.any_hash(state)
    }
}

mod private {
    pub trait Sealed {}
}
