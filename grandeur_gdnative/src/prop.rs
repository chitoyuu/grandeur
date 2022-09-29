use gdnative::core_types::{GodotString, Variant};
use gdnative::prelude::{Object, TRef};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Prop {
    name: GodotString,
    index: Option<i32>,
}

impl Prop {
    pub fn new<S>(name: S) -> Self
    where
        S: Into<GodotString>,
    {
        Prop::with_index(name, None)
    }

    pub fn with_index<S>(name: S, index: Option<i32>) -> Self
    where
        S: Into<GodotString>,
    {
        Prop {
            name: name.into(),
            index,
        }
    }

    pub(crate) fn set(&self, object: TRef<'_, Object>, value: Variant) {
        if let Some(index) = self.index {
            object.set_indexed(format!("{}:{}", self.name, index), value);
        } else {
            object.set(self.name.clone(), value);
        }
    }

    pub(crate) fn get(&self, object: TRef<'_, Object>) -> Variant {
        if let Some(index) = self.index {
            object.get_indexed(format!("{}:{}", self.name, index))
        } else {
            object.get(self.name.clone())
        }
    }
}

impl<S> From<S> for Prop
where
    S: Into<GodotString>,
{
    fn from(name: S) -> Self {
        Self::new(name)
    }
}
