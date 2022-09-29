use std::collections::VecDeque;
use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::sync::atomic::{self, AtomicUsize};
use std::sync::Arc;
use std::time::{Duration, Instant};

use crossbeam_channel::Sender;
use derivative::Derivative;
use gdnative::api::object::ConnectFlags;
use gdnative::api::Node;
use gdnative::export::user_data::{Map, MapMut};
use gdnative::export::Varargs;
use gdnative::export::{StaticArgs, StaticArgsMethod};
use gdnative::log::godot_site;
use gdnative::prelude::user_data::LocalCellData;
use gdnative::prelude::*;
use grandeur::conciliation::{Conciliate, DryState, Update, Updates, WetState};
use grandeur::fiber::{Chunk, Estimate};
use grandeur::Node as VTreeNode;
use hashbrown::{HashMap, HashSet};
use indexmap::IndexMap;
use thiserror::Error;

use crate::handler::SignalHandler;
use crate::prop::Prop;
use crate::repo::SceneRepo;
use crate::symbols::Symbols;
use crate::vnode::{AnyEqHash, Root, VNode};
use crate::{Fragment, Msg};

static RENDERER_SIGNAL_HANDLER_NAME: &str = "_on_rendered_node_signal";

static INSTANCE_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub struct Renderer<N, M> {
    instance_id: usize,
    symbols: Symbols,

    subs: RendererSubsMap<M>,
    chunks: VecDeque<Chunk<NodeUpdate<M>>>,
    options: Options<M>,
    repo: SceneRepo,
    poll: Vec<PollFn<M>>,
    _marker: PhantomData<N>,
}

type PollFn<M> = Box<dyn Fn() -> Option<Fragment<M>>>;
type RendererSubsMap<M> = HashMap<Ref<Node>, HashMap<String, Arc<dyn SignalHandler<M>>>>;

impl<N, M: Msg> Renderer<N, M>
where
    N: GodotObject + SubClass<Node> + 'static,
{
    pub fn new(options: Options<M>) -> Self {
        Self::with_repo(options, SceneRepo::new())
    }

    /// Create a renderer with a scene repository, possibly pre-loaded on another thread.
    pub fn with_repo(options: Options<M>, repo: SceneRepo) -> Self {
        let instance_id = INSTANCE_COUNTER.fetch_add(1, atomic::Ordering::AcqRel);

        Renderer {
            instance_id,
            symbols: Symbols::new(),

            subs: HashMap::default(),
            chunks: VecDeque::default(),
            options,
            repo,
            poll: Vec::new(),
            _marker: PhantomData,
        }
    }

    /// Add a new source to poll trees from on start of frame. Only the last tree polled is
    /// rendered, if any.
    pub fn poll<F>(&mut self, f: F)
    where
        F: 'static + Fn() -> Option<Fragment<M>>,
    {
        self.poll.push(Box::new(f))
    }

    pub(super) fn register_as(init: &InitHandle, name: impl Display, tool: bool) {
        if tool {
            init.add_tool_class_as::<Self>(name.to_string());
        } else {
            init.add_class_as::<Self>(name.to_string());
        }
    }
}

#[derive(Clone, Debug)]
pub struct Options<M> {
    pub fiberize: grandeur::fiber::Options,
    pub frame_time: Duration,
    pub msg_send: Sender<M>,
    pub handle_signals_while_dirty: bool,
}

impl<N, M: Msg> NativeClass for Renderer<N, M>
where
    N: GodotObject + SubClass<Node> + 'static,
{
    type Base = N;
    type UserData = LocalCellData<Self>;
}

impl<N, M: Msg> NativeClassMethods for Renderer<N, M>
where
    N: GodotObject + SubClass<Node> + 'static,
{
    fn register(builder: &ClassBuilder<Self>) {
        builder.signal("up_to_date").done();
        register_handler(builder);
        register_process(builder);
    }
}

fn register_handler<N, M: Msg>(builder: &ClassBuilder<Renderer<N, M>>)
where
    N: GodotObject + SubClass<Node> + 'static,
{
    #[derive(Derivative)]
    #[derivative(Copy, Clone, Default)]
    struct HandlerMethod<M> {
        _marker: PhantomData<M>,
    }

    unsafe impl<M> Send for HandlerMethod<M> {}
    unsafe impl<M> Sync for HandlerMethod<M> {}

    impl<N, M: Msg> Method<Renderer<N, M>> for HandlerMethod<M>
    where
        N: GodotObject + SubClass<Node> + 'static,
    {
        fn site() -> Option<gdnative::log::Site<'static>> {
            Some(godot_site!(Renderer::HandlerMethod))
        }

        fn call(
            &self,
            this: TInstance<'_, Renderer<N, M>, Shared>,
            mut args: Varargs<'_>,
        ) -> Variant {
            this.script().map(|script| {
                if script.options.handle_signals_while_dirty ^ !script.chunks.is_empty() {
                    return Variant::nil();
                }

                let signal_args = if args.len() > 2 {
                    let amount = args.len() - 2;
                    (&mut args).take(amount).cloned().collect()
                } else {
                    Vec::new()
                };

                let node = args.read::<Ref<Node>>()
                    .with_name("node")
                    .with_type_name("Node")
                    .get();

                let node = match node {
                    Ok(node) => node,
                    Err(e) => {
                        godot_error!("{}", e);
                        return Variant::nil();
                    }
                };

                let signal = args.read::<String>()
                    .with_name("signal_name")
                    .with_type_name("String").
                    get();

                let signal = match signal {
                    Ok(signal) => signal,
                    Err(e) => {
                        godot_error!("{}", e);
                        return Variant::nil();
                    }
                };

                if let Err(e) = args.done() {
                    godot_error!("{}", e);
                    return Variant::nil();
                }

                if let Some(handler) = script.subs.get(&node).and_then(|map| map.get(&signal)) {
                    let msg = match SignalHandler::<M>::handle(&**handler, node, signal_args) {
                        Ok(msg) => msg,
                        Err(e) => {
                            godot_error!("error handling signal `{}` on `{:?}`: {}", signal, node, e);
                            return Variant::nil();
                        }
                    };

                    if let Err(e) = script.options.msg_send.send(msg) {
                        godot_error!("error sending message: {}", e);
                    }
                } else {
                    godot_error!("signal `{}` on `{:?}` is redirected to the renderer, but no handler for it is found", signal, node);
                }

                Variant::nil()
            }).unwrap()
        }
    }

    builder
        .method(RENDERER_SIGNAL_HANDLER_NAME, HandlerMethod::<M>::default())
        .done_stateless();
}

fn register_process<N, M: Msg>(builder: &ClassBuilder<Renderer<N, M>>)
where
    N: GodotObject + SubClass<Node> + 'static,
{
    #[derive(FromVarargs)]
    struct ProcessArgs {
        _delta: f64,
    }

    #[derive(Derivative)]
    #[derivative(Copy, Clone, Default)]
    struct ProcessMethod<M> {
        _marker: PhantomData<M>,
    }

    unsafe impl<M> Send for ProcessMethod<M> {}
    unsafe impl<M> Sync for ProcessMethod<M> {}

    impl<M> ProcessMethod<M> {
        fn new() -> StaticArgs<Self> {
            StaticArgs::new(Self::default())
        }
    }

    impl<N, M: Msg> StaticArgsMethod<Renderer<N, M>> for ProcessMethod<M>
    where
        N: GodotObject + SubClass<Node> + 'static,
    {
        type Args = ProcessArgs;

        fn site() -> Option<gdnative::log::Site<'static>> {
            Some(godot_site!(Renderer::ProcessMethod))
        }

        fn call(&self, this: TInstance<'_, Renderer<N, M>, Shared>, _args: Self::Args) -> Variant {
            this.script()
                .map_mut(|script| {
                    let mut new_fragment = None;
                    for poll_fn in &script.poll {
                        if let Some(tree) = poll_fn() {
                            new_fragment = Some(tree);
                        }
                    }

                    let renderer_node = this.base().upcast();

                    if let Some(fragment) = new_fragment {
                        let tree = VTreeNode::with_children(VNode::new(Root), fragment);

                        if !renderer_node.has_meta(script.symbols.node_meta.clone()) {
                            renderer_node.set_meta(script.symbols.node_meta.clone(), NodeMeta {
                                instance_id: script.instance_id,
                                key: None,
                                subs: HashSet::default(),
                            }.emplace());
                        }

                        match build_wet_state_from_scene(script.instance_id, &script.symbols, renderer_node) {
                            Ok(wet) => {
                                let updates = grandeur::conciliation::conciliate::<ConciliateImpl<M>>(wet, tree);
                                let chunks = grandeur::fiber::fiberize(script.options.fiberize.clone(), updates);

                                script.chunks = VecDeque::from(chunks);
                            },
                            Err(e) => {
                                godot_error!("error reading state from scene tree: {}", e);
                            }
                        }
                    }

                    let deadline = Instant::now() + script.options.frame_time;

                    let mut count = 0;

                    while let Some(chunk) = if count == 0 || Instant::now() < deadline {
                        script.chunks.pop_front()
                    } else {
                        None
                    } {
                        count += 1;

                        let chunk_start = Instant::now();
                        let mut total_updates = 0;

                        for (path, updates) in chunk.updates() {
                            total_updates += updates.len();

                            let node = if let Some(node) = get_node_by_vpath(renderer_node, script.instance_id, &script.symbols, &path) {
                                node
                            } else {
                                godot_error!("cannot find node at {:?}", path);
                                continue;
                            };

                            apply_chunk(ApplyChunkEnv {
                                instance_id: script.instance_id,
                                symbols: &script.symbols,
                                renderer_node,
                                subs: &mut script.subs,
                                repo: &mut script.repo,
                                path: &path,
                            }, node, updates);
                        }

                        let duration = Instant::now() - chunk_start;
                        log::trace!("processed chunk of {} updates in {}s", total_updates, duration.as_secs_f32());
                    }

                    if count > 0 {
                        log::trace!("{} chunks processed in a frame", count);
                        if script.chunks.is_empty() {
                            renderer_node.emit_signal("up_to_date", &[]);
                        }
                    }

                    Variant::nil()
                })
                .unwrap()
        }
    }

    builder
        .method("_process", ProcessMethod::<M>::new())
        .done_stateless();
}

fn get_node_by_vpath<'a>(
    renderer: TRef<'a, Node>,
    instance_id: usize,
    symbols: &'a Symbols,
    path: &[u32],
) -> Option<TRef<'a, Node>> {
    let mut node = renderer;
    for &idx in path {
        if let Some((_, child_node)) =
            ManagedChildren::new(node, instance_id, symbols).nth(idx as usize)
        {
            node = child_node;
        } else {
            return None;
        }
    }

    Some(node)
}

struct ManagedChildren<'a> {
    node: TRef<'a, Node>,
    instance_id: usize,
    symbols: &'a Symbols,
    child_idx: std::ops::Range<i64>,
}

fn get_mount_point<'a>(node: TRef<'a, Node>, symbols: &Symbols) -> TRef<'a, Node> {
    if !node.has_method(symbols.mount_point_fn.clone()) {
        return node;
    }

    let mount_point = unsafe { node.call(symbols.mount_point_fn.clone(), &[]) };
    if mount_point.is_nil() {
        return node;
    }

    let mount_point = if let Ok(mount_point) = mount_point.try_to_object::<Node>() {
        mount_point
    } else {
        log::error!(
            "`{}` on {:?} returned a non-nil value that is not a node: `{:?}`",
            symbols.mount_point_fn,
            node,
            mount_point
        );

        return node;
    };

    let mount_point = if let Some(mount_point) = unsafe { mount_point.assume_safe_if_sane() } {
        mount_point
    } else {
        log::error!(
            "`{}` on {:?} returned an invalid node pointer",
            symbols.mount_point_fn,
            node,
        );

        return node;
    };

    if !node.is_a_parent_of(mount_point) {
        log::warn!(
            "`{}` on {:?} returned a node that is not a descendent of itself: {:?}",
            symbols.mount_point_fn,
            node,
            mount_point,
        );
    }

    mount_point
}

impl<'a> ManagedChildren<'a> {
    fn new(node: TRef<'a, Node>, instance_id: usize, symbols: &'a Symbols) -> Self {
        let node = get_mount_point(node, symbols);

        ManagedChildren {
            node,
            instance_id,
            symbols,
            child_idx: 0..node.get_child_count(),
        }
    }
}

impl<'a> Iterator for ManagedChildren<'a> {
    type Item = (i64, TRef<'a, Node>);

    fn next(&mut self) -> Option<Self::Item> {
        for child_idx in &mut self.child_idx {
            let child = unsafe { self.node.get_child(child_idx).unwrap().assume_safe() };
            if child.has_meta(self.symbols.node_meta.clone()) {
                if let Ok(meta) = Instance::<NodeMeta, _>::from_variant(
                    &child.get_meta(self.symbols.node_meta.clone(), Variant::nil()),
                ) {
                    match meta
                        .script()
                        .map(|meta| meta.instance_id == self.instance_id)
                    {
                        Ok(true) => return Some((child_idx, child)),
                        Ok(false) => {}
                        Err(e) => {
                            log::warn!(
                                "meta key `{}` used for irrelevant data type: {}",
                                self.symbols.node_meta,
                                e
                            );
                        }
                    }
                }
            }
        }

        None
    }
}

impl<'a> DoubleEndedIterator for ManagedChildren<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        while let Some(child_idx) = self.child_idx.next_back() {
            let child = unsafe { self.node.get_child(child_idx).unwrap().assume_safe() };
            if child.has_meta(self.symbols.node_meta.clone()) {
                return Some((child_idx, child));
            }
        }

        None
    }
}

struct ApplyChunkEnv<'a, M> {
    instance_id: usize,
    symbols: &'a Symbols,
    renderer_node: TRef<'a, Node>,
    subs: &'a mut RendererSubsMap<M>,
    repo: &'a mut SceneRepo,
    path: &'a [u32],
}

fn apply_chunk<'a, M: Msg>(
    mut env: ApplyChunkEnv<'a, M>,
    mut node: TRef<'a, Node>,
    updates: Vec<NodeUpdate<M>>,
) {
    for u in updates {
        match u {
            NodeUpdate::RemoveChildrenAt(xs) => {
                apply_remove_children_at(&env, node, xs);
            }
            NodeUpdate::SwapChildren(a, b) => {
                apply_swap_children(&env, node, a, b);
            }
            NodeUpdate::AddChildrenAt(x, n) => {
                apply_add_children_at(&env, node, x, n);
            }
            NodeUpdate::ReplaceWith(vnode) => {
                apply_replace_with(&mut env, node, vnode);
                node = get_node_by_vpath(env.renderer_node, env.instance_id, env.symbols, env.path)
                    .unwrap();
            }
            NodeUpdate::SyncProps { set_props } => {
                patch_props(node, set_props);
            }
            NodeUpdate::SyncSubs { unsubs, subs } => {
                let meta = match Instance::<NodeMeta, _>::from_variant(
                    &node.get_meta(env.symbols.node_meta.clone(), Variant::nil()),
                ) {
                    Ok(meta) => meta,
                    Err(e) => {
                        godot_error!("failed to fetch meta for node {:?}: {}", node.get_path(), e);
                        return;
                    }
                };

                meta.script()
                    .map_mut(|meta| {
                        patch_subs(&mut env, node, meta, unsubs, subs);
                    })
                    .unwrap();
            }
        }
    }
}

fn apply_replace_with<'a, M: Msg>(
    env: &mut ApplyChunkEnv<M>,
    node: TRef<'a, Node>,
    vnode: VNode<M>,
) {
    let VNode {
        key,
        kind,
        props,
        subs,
        ..
    } = vnode;

    let parent_node = node.get_parent().expect("this node is under a `Renderer`");
    // SAFETY: parent is also part of the rendered tree
    let parent_node = unsafe { parent_node.assume_safe() };

    let node_index = node.get_index();

    let new_node = match kind.expand(env.symbols, env.repo) {
        Ok(node) => unsafe { node.into_shared().assume_unique() },
        Err(e) => {
            godot_error!("failed to create new node: {}", e);
            return;
        }
    };

    let new_meta = Instance::emplace(NodeMeta {
        instance_id: env.instance_id,
        key,
        subs: HashSet::default(),
    });

    let new_meta = new_meta.into_shared();
    let new_meta = unsafe { new_meta.assume_safe() };

    new_node.set_meta(env.symbols.node_meta.clone(), new_meta.base());

    let new_node = unsafe { new_node.into_shared().assume_safe() };

    new_meta
        .script()
        .map_mut(|meta| {
            patch_subs(env, new_node, meta, HashSet::default(), subs);
            patch_props(new_node, props);
        })
        .unwrap();

    parent_node.remove_child(node);
    parent_node.add_child(new_node, false);
    parent_node.move_child(new_node, node_index);

    // SAFETY: removed from the scene tree
    let old_node = unsafe { node.assume_unique() };
    old_node.queue_free();
}

fn patch_props(node: TRef<Node>, set_props: IndexMap<Prop, Variant, ahash::RandomState>) {
    for (prop, value) in set_props {
        prop.set(node.upcast(), value.clone());
    }
}

fn patch_subs<M: Msg>(
    env: &mut ApplyChunkEnv<M>,
    node: TRef<Node>,
    meta: &mut NodeMeta,
    unsubs: HashSet<String, ahash::RandomState>,
    subs: HashMap<String, Arc<dyn SignalHandler<M>>, ahash::RandomState>,
) {
    let renderer_subs = env.subs.entry(node.claim()).or_default();

    meta.subs.retain(|signal| {
        if unsubs.contains(signal) {
            node.disconnect(signal, env.renderer_node, RENDERER_SIGNAL_HANDLER_NAME);
            renderer_subs.remove(signal);
            false
        } else {
            true
        }
    });

    for (signal, handler) in subs {
        if !meta.subs.contains(&signal) {
            let binds = VariantArray::new();

            binds.push(node);
            binds.push(&signal);

            if let Err(e) = node.connect(
                &signal,
                env.renderer_node,
                RENDERER_SIGNAL_HANDLER_NAME,
                binds.into_shared(),
                ConnectFlags::DEFERRED.into(),
            ) {
                godot_error!(
                    "error connecting to signal {} on new node under `{:?}`: {}",
                    signal,
                    node.get_path(),
                    e
                );

                continue;
            }

            meta.subs.insert(signal.clone());
        }

        renderer_subs.insert(signal, handler);
    }
}

// FIXME: Extremely inefficient! Avoid creating real temporary nodes for this.
fn apply_add_children_at<M: Msg>(env: &ApplyChunkEnv<M>, node: TRef<Node>, x: usize, n: usize) {
    let base_idx = if x == 0 {
        0
    } else {
        ManagedChildren::new(node, env.instance_id, env.symbols)
            .nth(x)
            .map_or_else(|| node.get_child_count(), |(idx, _)| idx)
    };

    let mount_point = get_mount_point(node, env.symbols);

    for _ in 0..n {
        let child = Node::new();
        child.set_meta(
            env.symbols.node_meta.clone(),
            Instance::emplace(NodeMeta {
                instance_id: env.instance_id,
                key: None,
                subs: HashSet::default(),
            }),
        );

        let child = child.into_shared();
        mount_point.add_child(child, false);
        mount_point.move_child(child, base_idx as i64);
    }
}

fn apply_swap_children<M>(env: &ApplyChunkEnv<'_, M>, node: TRef<Node>, a: usize, b: usize) {
    let (a, b) = (a.min(b), a.max(b));
    if a == b {
        return;
    }

    let mut managed_children = ManagedChildren::new(node, env.instance_id, env.symbols);
    let (a_idx, child_a) = if let Some(pair) = managed_children.nth(a) {
        pair
    } else {
        godot_error!("child indices are out of range: `{}`", a);
        return;
    };

    let (b_idx, child_b) = if let Some(pair) = managed_children.nth(b - a - 1) {
        pair
    } else {
        godot_error!("child indices are out of range: `{}`", b);
        return;
    };

    let mount_point = get_mount_point(node, env.symbols);

    mount_point.move_child(child_b, a_idx);
    if b_idx > a_idx + 1 {
        mount_point.move_child(child_a, b_idx);
    }
}

fn apply_remove_children_at<M>(
    env: &ApplyChunkEnv<'_, M>,
    node: TRef<'_, Node>,
    mut xs: Vec<usize>,
) {
    xs.sort_unstable();

    let mount_point = get_mount_point(node, env.symbols);

    // TODO: Just cheaping out here -- trying to get a working build ASAP
    let mut managed_children =
        ManagedChildren::new(node, env.instance_id, env.symbols).collect::<Vec<_>>();

    for idx in xs.iter().rev() {
        let (_, child) = managed_children.remove(*idx);

        child.remove_meta(env.symbols.node_meta.clone());
        if child.has_method(env.symbols.unmount_fn.clone()) {
            unsafe {
                child.call(env.symbols.unmount_fn.clone(), &[]);
            }
        } else {
            mount_point.remove_child(child);
            unsafe { child.assume_unique().queue_free() };
        }
    }
}

struct ConciliateImpl<M> {
    _marker: PhantomData<M>,
}

impl<M> Conciliate for ConciliateImpl<M> {
    type Wet<'a> = Wet<'a>
    where
        Self: 'a;

    type Dry<'a> = VNode<M>
    where
        Self: 'a;

    type Key<'a> = Arc<dyn AnyEqHash>
    where
        Self: 'a;

    type Update<'a> = NodeUpdate<M>
    where
        Self: 'a;
}

struct Wet<'a> {
    node: Option<TRef<'a, Node>>,
    key: Option<Arc<dyn AnyEqHash>>,
    symbols: Option<&'a Symbols>,
}

fn get_meta(node: TRef<'_, Node>, symbols: &'_ Symbols) -> Result<NodeMeta, BuildWetStateError> {
    let meta = Instance::<NodeMeta, Shared>::from_variant(
        &node.get_meta(symbols.node_meta.clone(), Variant::nil()),
    )
    .map_err(|e| BuildWetStateError {
        kind: BuildWetStateErrorKind::InvalidMeta(e),
        path: node.get_path(),
    })?;

    Ok(meta.script().map(NodeMeta::clone).unwrap())
}

fn build_wet_state_from_scene<'a>(
    instance_id: usize,
    symbols: &'a Symbols,
    root: TRef<'a, Node>,
) -> Result<VTreeNode<Wet<'a>>, BuildWetStateError> {
    struct Frame<'a> {
        node: TRef<'a, Node>,
        meta: NodeMeta,
        managed_children: ManagedChildren<'a>,
        vnodes: Vec<VTreeNode<Wet<'a>>>,
    }

    let meta = get_meta(root, symbols)?;

    let mut stack = vec![Frame {
        node: root,
        meta,
        managed_children: ManagedChildren::new(root, instance_id, symbols),
        vnodes: Vec::new(),
    }];

    while let Some(mut frame) = stack.pop() {
        if frame.meta.instance_id != instance_id {
            return Err(BuildWetStateError {
                kind: BuildWetStateErrorKind::InstanceMismatch,
                path: frame.node.get_path(),
            });
        }

        if let Some((_, node)) = frame.managed_children.next() {
            let meta = get_meta(node, symbols)?;

            stack.push(frame);
            stack.push(Frame {
                node,
                meta,
                managed_children: ManagedChildren::new(node, instance_id, symbols),
                vnodes: Vec::new(),
            });
        } else {
            let wet = Wet {
                node: Some(frame.node),
                key: frame.meta.key,
                symbols: Some(symbols),
            };

            let vnode = VTreeNode::with_children(wet, frame.vnodes);

            if let Some(parent) = stack.last_mut() {
                parent.vnodes.push(vnode);
                continue;
            } else {
                return Ok(vnode);
            }
        }
    }

    unreachable!()
}

#[derive(Debug, Error)]
#[error("at node `{path:?}`: {kind}")]
struct BuildWetStateError {
    kind: BuildWetStateErrorKind,
    path: NodePath,
}

#[derive(Debug, Error)]
enum BuildWetStateErrorKind {
    #[error("instance id does not match that of the scene tree node")]
    InstanceMismatch,
    #[error("invalid meta value: {0}")]
    InvalidMeta(FromVariantError),
}

impl<'a, M: 'a> WetState<'a, ConciliateImpl<M>> for Wet<'a> {
    fn nil() -> Self {
        Wet {
            node: None,
            key: None,
            symbols: None,
        }
    }

    fn key(&self) -> Option<Arc<dyn AnyEqHash>> {
        self.key.clone()
    }

    fn updates(&self, target: &VNode<M>) -> Updates<'a, ConciliateImpl<M>> {
        let node = if let Some(node) = self.node {
            node
        } else {
            return Updates::Replace;
        };

        let symbols = if let Some(symbols) = self.symbols {
            symbols
        } else {
            return Updates::Replace;
        };

        if target.prefer_as_is {
            return Updates::AsIs;
        }

        if !target.kind.matches(symbols, node) {
            return Updates::Replace;
        }

        let meta = match Instance::<NodeMeta, _>::from_variant(
            &node.get_meta(symbols.node_meta.clone(), Variant::nil()),
        ) {
            Ok(meta) => meta,
            Err(_) => return Updates::Replace,
        };

        let mut updates = Vec::new();

        meta.script()
            .map(|meta| {
                let sync_props = {
                    let mut set_props = IndexMap::default();

                    for (prop, value) in &target.props {
                        let old = prop.get(node.upcast());
                        if &old != value {
                            set_props.insert(prop.clone(), value.clone());
                        }
                    }

                    if set_props.is_empty() {
                        None
                    } else {
                        Some(NodeUpdate::SyncProps { set_props })
                    }
                };

                updates.extend(sync_props);

                let sync_subs = {
                    let mut unsubs = HashSet::default();
                    let mut subs = HashMap::default();

                    for signal in &meta.subs {
                        if !target.subs.contains_key(signal) {
                            unsubs.insert(signal.clone());
                        }
                    }

                    for (signal, handler) in &target.subs {
                        subs.insert(signal.clone(), handler.clone());
                    }

                    if unsubs.is_empty() && subs.is_empty() {
                        None
                    } else {
                        Some(NodeUpdate::SyncSubs { unsubs, subs })
                    }
                };

                updates.extend(sync_subs);
            })
            .unwrap();

        Updates::Updates(updates)
    }
}

impl<'a, M> DryState<'a, ConciliateImpl<M>> for VNode<M> {
    fn key(&self) -> Option<Arc<dyn AnyEqHash>> {
        self.key.clone()
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
enum NodeUpdate<M> {
    ReplaceWith(VNode<M>),
    AddChildrenAt(usize, usize),
    RemoveChildrenAt(Vec<usize>),
    SwapChildren(usize, usize),
    SyncProps {
        set_props: IndexMap<Prop, Variant, ahash::RandomState>,
    },
    SyncSubs {
        unsubs: HashSet<String, ahash::RandomState>,
        subs: HashMap<String, Arc<dyn SignalHandler<M>>, ahash::RandomState>,
    },
}

impl<'a, M> Update<'a, ConciliateImpl<M>> for NodeUpdate<M> {
    fn replace_with(dry: VNode<M>) -> Self {
        Self::ReplaceWith(dry)
    }
    fn add_children_at(idx: usize, n: usize) -> Self {
        Self::AddChildrenAt(idx, n)
    }
    fn remove_children_at(xs: Vec<usize>) -> Self {
        Self::RemoveChildrenAt(xs)
    }
    fn swap_children(a: usize, b: usize) -> Self {
        Self::SwapChildren(a, b)
    }
}

impl<M> Estimate for NodeUpdate<M> {
    fn cost(&self) -> usize {
        // TODO: better estimation
        1
    }
}

#[derive(Clone, NativeClass)]
#[inherit(Reference)]
#[no_constructor]
#[user_data(LocalCellData<Self>)]
pub struct NodeMeta {
    instance_id: usize,
    key: Option<Arc<dyn AnyEqHash>>,
    subs: HashSet<String>,
}

#[methods]
impl NodeMeta {}
