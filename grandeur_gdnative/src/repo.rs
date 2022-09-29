use gdnative::api::PackedScene;
use gdnative::api::ResourceLoader;
use gdnative::core_types::GodotString;
use gdnative::object::ownership::ThreadLocal;
use gdnative::object::Ref;
use gdnative::prelude::Unique;
use hashbrown::hash_map::Entry;
use hashbrown::HashMap;
use thiserror::Error;

/// Safe thread-local repository of packed scenes.
#[derive(Clone, Default)]
pub struct SceneRepo {
    cache: HashMap<GodotString, Ref<PackedScene, ThreadLocal>>,
}

impl SceneRepo {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn load(
        &mut self,
        path: impl Into<GodotString>,
    ) -> Result<Ref<PackedScene, ThreadLocal>, Error> {
        let path = path.into();

        match self.cache.entry(path.clone()) {
            Entry::Occupied(entry) => Ok(entry.get().clone()),
            Entry::Vacant(entry) => {
                let loader = ResourceLoader::godot_singleton();
                let scene = loader
                    .load(path, "PackedScene", true)
                    .ok_or(Error::NotExist)?;
                let scene = scene.cast::<PackedScene>().ok_or(Error::NotPackedScene)?;

                // SAFETY: load with no_cache = true should fetch a fresh unique copy of the
                // resource
                let scene = unsafe { scene.assume_thread_local() };

                entry.insert(scene.clone());
                Ok(scene)
            }
        }
    }
}

impl From<SceneRepoBuilder> for SceneRepo {
    fn from(b: SceneRepoBuilder) -> Self {
        b.done()
    }
}

/// Builder for pre-loaded [`SceneRepo`]s off the main thread.
#[derive(Default)]
pub struct SceneRepoBuilder {
    cache: HashMap<GodotString, Ref<PackedScene, Unique>>,
}

impl SceneRepoBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Loads a scene into the repo being built. Returns whether the operation is successful
    /// *without* returning it.
    pub fn load(&mut self, path: impl Into<GodotString>) -> Result<(), Error> {
        let path = path.into();

        match self.cache.entry(path.clone()) {
            Entry::Occupied(_) => Ok(()),
            Entry::Vacant(entry) => {
                let loader = ResourceLoader::godot_singleton();
                let scene = loader
                    .load(path, "PackedScene", true)
                    .ok_or(Error::NotExist)?;
                let scene = scene.cast::<PackedScene>().ok_or(Error::NotPackedScene)?;

                // SAFETY: load with no_cache = true should fetch a fresh unique copy of the
                // resource
                let scene = unsafe { scene.assume_unique() };

                entry.insert(scene);
                Ok(())
            }
        }
    }

    pub fn done(self) -> SceneRepo {
        let cache = self
            .cache
            .into_iter()
            .map(|(k, scene)| (k, scene.into_thread_local()))
            .collect();
        SceneRepo { cache }
    }
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum Error {
    #[error("the requested path does not exist")]
    NotExist,
    #[error("the resource at the requested path is not a PackedScene")]
    NotPackedScene,
}
