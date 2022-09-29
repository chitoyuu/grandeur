use std::time::Duration;

use gdnative::api::{HBoxContainer, LineEdit, VBoxContainer};
use gdnative::prelude::*;
use grandeur_gdnative::channel::{unbounded, Receiver, Sender};
use grandeur_gdnative::handler::Ary;
use grandeur_gdnative::macros::{vfrag, vtree};
use grandeur_gdnative::renderer::{Options, Renderer};
use grandeur_gdnative::vnode::Scene;
use grandeur_gdnative::Fragment;

#[derive(NativeClass, Default)]
#[inherit(Node2D)]
struct TodoApp {
    msg_recv: Option<Receiver<TodoMsg>>,

    tree_send: Option<Sender<Fragment<TodoMsg>>>,

    counter: usize,
    items: Vec<(usize, GodotString)>,
    current: GodotString,
}

impl TodoApp {
    fn new(_owner: &Node2D) -> Self {
        Self::default()
    }

    fn update(&mut self, msg: TodoMsg) {
        match msg {
            TodoMsg::Delete(n) => {
                self.items.remove(n);
            }
            TodoMsg::Add => {
                if !self.current.is_empty() {
                    let id = self.counter;
                    self.counter += 1;
                    self.items.push((id, std::mem::take(&mut self.current)));
                }
            }
            TodoMsg::SetCurrent(s) => {
                self.current = s;
            }
        }
    }

    fn render(&self) -> Fragment<TodoMsg> {
        let fade_scene = Scene::new("res://fade.tscn");

        vfrag! {
            <VBoxContainer>
                <Label text="Obligatory Todo App" />
                {
                    self.items.iter().enumerate().map(|(n, (id, text))| {
                        vtree! {
                            <fade_scene x:key=*id>
                                <HBoxContainer>
                                    <Button
                                        text="x"
                                        on:pressed=TodoMsg::Delete(n) />
                                    <Label text />
                                </HBoxContainer>
                            </fade_scene>
                        }
                    })
                }
                <HBoxContainer>
                    <LineEdit
                        text = self.current.clone()
                        rect_min_size = Vector2::new(100.0, 20.0)
                        on:text_changed = Ary::new(|args| {
                            let text = GodotString::from_variant(args.get(0).unwrap())?;
                            Ok(TodoMsg::SetCurrent(text))
                        }) />
                    <Button text="Add" on:pressed=TodoMsg::Add />
                </HBoxContainer>
            </VBoxContainer>
        }
    }
}

#[methods]
impl TodoApp {
    #[method]
    fn _ready(&mut self, #[base] owner: TRef<'_, Node2D>) -> bool {
        let (msg_send, msg_recv) = grandeur_gdnative::channel::unbounded();
        self.msg_recv = Some(msg_recv);

        let mut renderer = Renderer::<Node2D, TodoMsg>::new(Options {
            fiberize: grandeur::fiber::Options {
                chunk_cost: 128,
                break_cost: 16,
            },
            frame_time: Duration::from_millis(9),
            msg_send,
            handle_signals_while_dirty: false,
        });

        let (tree_send, tree_recv) = unbounded();

        tree_send.send(self.render()).ok();
        self.tree_send = Some(tree_send);

        renderer.poll(move || tree_recv.try_recv().ok());

        owner.add_child(Instance::emplace(renderer).into_base(), false);

        true
    }

    #[method]
    fn _process(&mut self, _delta: f64) {
        let mut any = false;
        while let Some(msg) = self.msg_recv.as_ref().and_then(|r| r.try_recv().ok()) {
            any = true;
            self.update(msg);
        }

        if any {
            if let Some(send) = self.tree_send.as_ref() {
                send.send(self.render()).expect("it works");
            }
        }
    }
}

#[derive(Clone, Debug)]
enum TodoMsg {
    Delete(usize),
    SetCurrent(GodotString),
    Add,
}

fn init(handle: InitHandle) {
    handle.add_class::<TodoApp>();
    grandeur_gdnative::register(&handle, true)
        .with_renderer::<Node2D, TodoMsg>("Node2DTodoMsgRenderer");
}

godot_init!(init);
