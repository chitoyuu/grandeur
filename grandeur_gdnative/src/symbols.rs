use gdnative::prelude::GodotString;

#[derive(Clone, Debug)]
pub struct Symbols {
    pub node_meta: GodotString,
    pub scene_meta: GodotString,

    pub unmount_fn: GodotString,
    pub mount_point_fn: GodotString,
}

impl Default for Symbols {
    fn default() -> Self {
        Self::with_prefix("_grandeur")
    }
}

impl Symbols {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_prefix<S: AsRef<str>>(prefix: S) -> Self {
        let prefix = prefix.as_ref();

        let node_meta = GodotString::from(format!("{}_internal", prefix));
        let scene_meta = GodotString::from(format!("{}_internal_src_scene", prefix));

        let unmount_fn = GodotString::from(format!("{}_unmount", prefix));
        let mount_point_fn = GodotString::from(format!("{}_mount_point", prefix));

        Symbols {
            node_meta,
            scene_meta,

            unmount_fn,
            mount_point_fn,
        }
    }
}
