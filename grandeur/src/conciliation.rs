use alloc::vec;
use alloc::vec::Vec;
use core::fmt::Debug;
use core::hash::Hash;
use hashbrown::HashMap;

use crate::{
    node::{OwnedNodePath, VisitEntry},
    Node,
};

pub trait Conciliate {
    type Wet<'a>: WetState<'a, Self>
    where
        Self: 'a;

    type Dry<'a>: DryState<'a, Self>
    where
        Self: 'a;

    type Key<'a>: Eq + Hash
    where
        Self: 'a;

    type Update<'a>: Update<'a, Self>
    where
        Self: 'a;
}

pub trait WetState<'a, Family: 'a + Conciliate + ?Sized> {
    /// Returns a "nil" instance, which cannot be updated. Used to represent new nodes.
    fn nil() -> Self;

    fn key(&self) -> Option<Family::Key<'a>>;

    /// Try to generate a list of update operations that can be performed on this node to bring
    /// it to the state of `target`. Return [`Updates::None`] if the node must be replaced.
    ///
    /// The default implementation returns [`Updates::Replace`].
    fn updates(&self, _target: &Family::Dry<'a>) -> Updates<'a, Family> {
        Updates::Replace
    }
}

/// Represents the actions that may be taken to bring a node to an updated state.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Updates<'a, Family: 'a + Conciliate + ?Sized> {
    /// Keep the existing wet tree as-is, and terminate the conciliation algorithm.
    AsIs,
    /// The node cannot be updated to reflect the dry state, and must be replaced.
    Replace,
    /// The node can be re-used with the given array of updates.
    Updates(Vec<Family::Update<'a>>),
}

impl<'a, F: 'a + Conciliate + ?Sized> Default for Updates<'a, F> {
    fn default() -> Self {
        Updates::Replace
    }
}

pub trait DryState<'a, Family: Conciliate + ?Sized> {
    fn key(&self) -> Option<Family::Key<'a>>;
}

pub trait Update<'a, Family: Conciliate + ?Sized> {
    fn replace_with(dry: Family::Dry<'a>) -> Self;
    fn add_children_at(idx: usize, n: usize) -> Self;
    fn remove_children_at(xs: Vec<usize>) -> Self;
    fn swap_children(a: usize, b: usize) -> Self;
}

/// Update entry that can be optionally fibered.
pub struct UpdateEntry<'a, Family: 'a + Conciliate + ?Sized> {
    pub path: OwnedNodePath,
    pub op: Family::Update<'a>,
}

pub fn conciliate<'a, Family: Conciliate + ?Sized>(
    wet: Node<Family::Wet<'a>>,
    dry: Node<Family::Dry<'a>>,
) -> Node<Vec<Family::Update<'a>>> {
    wet.visit_apply(dry, |wet, dry| {
        let mut updates = match wet.data.updates(&dry.data) {
            Updates::AsIs => {
                dry.children.clear();
                return Vec::new();
            }
            Updates::Updates(updates) => updates,
            Updates::Replace => {
                wet.children.clear();
                wet.children.extend(
                    core::iter::repeat_with(|| Node::new(WetState::nil())).take(dry.children.len()),
                );

                let mut updates = vec![Update::replace_with(dry.data)];

                if !dry.children.is_empty() {
                    updates.push(Update::add_children_at(0, dry.children.len()));
                }

                updates
            }
        };

        let mut old_keyed =
            HashMap::with_capacity_and_hasher(wet.children.len(), ahash::RandomState::default());
        let mut new_keyed =
            HashMap::with_capacity_and_hasher(dry.children.len(), ahash::RandomState::default());
        let mut old_unkeyed = Vec::with_capacity(wet.children.len());
        let mut new_unkeyed = Vec::with_capacity(dry.children.len());

        for (n, child) in wet.children.iter().enumerate() {
            if let Some(key) = child.data().key() {
                old_keyed.insert(key, n);
            } else {
                old_unkeyed.push(n);
            }
        }

        for (n, child) in dry.children.iter().enumerate() {
            if let Some(key) = child.data().key() {
                new_keyed.insert(key, n);
            } else {
                new_unkeyed.push(n);
            }
        }

        let mut to_remove = Vec::new();

        for (k, n) in &old_keyed {
            if !new_keyed.contains_key(k) {
                to_remove.push(*n);
            }
        }

        if old_unkeyed.len() > new_unkeyed.len() {
            to_remove.extend(old_unkeyed.drain(new_unkeyed.len()..));
        }

        to_remove.sort_unstable();
        if !to_remove.is_empty() {
            for n in to_remove.iter().rev() {
                wet.children.remove(*n);
            }

            old_keyed.retain(|_, n| {
                if let Err(offset) = to_remove.binary_search(n) {
                    *n -= offset;
                    true
                } else {
                    false
                }
            });

            for n in &mut old_unkeyed {
                *n -= to_remove.binary_search(n).unwrap_err();
            }

            updates.push(Update::remove_children_at(to_remove.clone()));
        }

        let count_delta = dry.children.len().saturating_sub(wet.children.len());
        if count_delta > 0 {
            updates.push(Update::add_children_at(wet.children.len(), count_delta));
            for _ in 0..count_delta {
                wet.children.push(Node::new(WetState::nil()));
            }
        }

        let mut unkeyed_count = 0;
        for (n, dry) in dry.children.iter().enumerate() {
            let old_n = if let Some(key) = dry.data().key() {
                old_keyed.get(&key).copied()
            } else {
                let old_n = old_unkeyed.get(unkeyed_count).copied();
                unkeyed_count += 1;
                old_n
            };

            if let Some(old_n) = old_n {
                if old_n != n {
                    updates.push(Update::swap_children(n, old_n));
                    if let Some(swapped_key) = wet.children[n].data().key() {
                        old_keyed.insert(swapped_key, old_n);
                    }
                    wet.children.swap(n, old_n);
                }
            } else {
                updates.push(Update::add_children_at(n, 1));
                wet.children.insert(n, Node::new(WetState::nil()));
                for r in old_keyed.values_mut().chain(&mut old_unkeyed) {
                    if *r >= n {
                        *r += 1;
                    }
                }
            }
        }

        updates
    })
}

pub trait Apply<'a>: Conciliate {
    fn apply<I>(wet: &mut VisitEntry<'_, Self::Wet<'a>>, updates: I)
    where
        I: IntoIterator<Item = Self::Update<'a>>;
}

pub trait Dehydrate<'a>: Conciliate {
    fn dehydrate(wet: Self::Wet<'a>) -> Self::Dry<'a>;
}

pub fn test_conciliate<'a, Family: Conciliate + ?Sized>(
    wet: Node<Family::Wet<'a>>,
    dry: Node<Family::Dry<'a>>,
) where
    Family: Apply<'a> + Dehydrate<'a>,
    Family::Wet<'a>: Clone,
    Family::Dry<'a>: Debug + Clone + Eq,
{
    let updates = conciliate::<'a, Family>(wet.clone(), dry.clone());
    let result = wet.visit_apply(updates, |mut wet, updates| {
        Family::apply(&mut wet, updates.data);
        Family::dehydrate(wet.data)
    });

    dry.visit_apply(result, |a, b| {
        assert_eq!(
            a.data, b.data,
            "unexpected difference in data at {:?}",
            a.path
        );
        assert_eq!(
            a.children.len(),
            b.children.len(),
            "unexpected difference in child count at {:?}",
            a.path
        );
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
    struct Wrap {
        key: Option<i32>,
        value: Option<i32>,
        allow_update: AllowUpdate,
    }

    #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
    enum AllowUpdate {
        Yes,
        No,
        AsIs,
    }

    impl Default for AllowUpdate {
        fn default() -> Self {
            AllowUpdate::No
        }
    }

    impl Wrap {
        fn new(value: i32) -> Self {
            Wrap {
                key: None,
                value: Some(value),
                allow_update: AllowUpdate::Yes,
            }
        }

        fn key(mut self, key: i32) -> Self {
            self.key = Some(key);
            self
        }

        fn no_update(mut self) -> Self {
            self.allow_update = AllowUpdate::No;
            self
        }

        fn as_is(mut self) -> Self {
            self.allow_update = AllowUpdate::AsIs;
            self
        }
    }

    struct Stub;
    impl Conciliate for Stub {
        type Wet<'a> = Wrap
        where
            Self: 'a;

        type Dry<'a> = Wrap
        where
            Self: 'a;

        type Key<'a> = i32
        where
            Self: 'a;

        type Update<'a> = NumUpdate
        where
            Self: 'a;
    }

    impl<'a> WetState<'a, Stub> for Wrap {
        fn nil() -> Self {
            Self::default()
        }

        fn key(&self) -> Option<i32> {
            self.key
        }

        fn updates(&self, target: &Self) -> Updates<'a, Stub> {
            match self.allow_update {
                AllowUpdate::Yes => {
                    if let (Some(target), Some(value)) = (target.value, self.value) {
                        if target != value {
                            Updates::Updates(vec![NumUpdate::Delta(target - value)])
                        } else {
                            Updates::Updates(vec![])
                        }
                    } else {
                        Updates::Replace
                    }
                }
                AllowUpdate::No => Updates::Replace,
                AllowUpdate::AsIs => Updates::AsIs,
            }
        }
    }

    impl<'a> DryState<'a, Stub> for Wrap {
        fn key(&self) -> Option<i32> {
            self.key
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    enum NumUpdate {
        Delta(i32),
        ReplaceWith(Wrap),
        AddChildrenAt(usize, usize),
        RemoveChildrenAt(Vec<usize>),
        SwapChildren(usize, usize),
    }

    impl<'a> Update<'a, Stub> for NumUpdate {
        fn replace_with(dry: Wrap) -> Self {
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

    impl<'a> Apply<'a> for Stub {
        fn apply<I>(wet: &mut VisitEntry<'_, Wrap>, updates: I)
        where
            I: IntoIterator<Item = NumUpdate>,
        {
            for u in updates {
                match u {
                    NumUpdate::Delta(d) => *wet.data.value.as_mut().unwrap() += d,
                    NumUpdate::AddChildrenAt(at, n) => {
                        wet.children
                            .splice(at..at, core::iter::repeat_with(Node::default).take(n));
                    }
                    NumUpdate::RemoveChildrenAt(mut xs) => {
                        xs.sort_unstable();
                        for n in xs.iter().rev() {
                            wet.children.remove(*n);
                        }
                    }
                    NumUpdate::ReplaceWith(data) => wet.data = data,
                    NumUpdate::SwapChildren(a, b) => wet.children.swap(a, b),
                }
            }
        }
    }

    impl<'a> Dehydrate<'a> for Stub {
        fn dehydrate(wet: Wrap) -> Wrap {
            wet
        }
    }

    #[test]
    fn it_works() {
        let left = Node::with_children(
            Wrap::new(42),
            vec![
                Node::new(Wrap::new(1).key(1)),
                Node::with_children(
                    Wrap::new(2).no_update(),
                    vec![Node::new(Wrap::new(3)), Node::new(Wrap::new(4))],
                ),
                Node::new(Wrap::new(3).key(3)),
            ],
        );

        let right = Node::with_children(
            Wrap::new(42),
            vec![
                Node::new(Wrap::new(30).key(3)),
                Node::new(Wrap::new(20).no_update()),
                Node::with_children(
                    Wrap::new(10).key(1),
                    vec![Node::new(Wrap::new(3)), Node::new(Wrap::new(4))],
                ),
                Node::new(Wrap::new(40)),
            ],
        );

        test_conciliate::<'_, Stub>(left, right);
    }

    #[test]
    fn deals_with_zero_child_replacement() {
        let left = Node::with_children(
            Wrap::new(42).no_update(),
            vec![Node::new(Wrap::new(1).key(1))],
        );

        let right = Node::new(Wrap::new(42));

        let expected = Node::new(vec![NumUpdate::ReplaceWith(Wrap::new(42))]);

        assert_eq!(expected, conciliate::<'_, Stub>(left, right));
    }

    #[test]
    fn deals_with_keyed_elements() {
        let left = Node::with_children(
            Wrap::new(42),
            vec![
                Node::new(Wrap::new(1).key(1)),
                Node::new(Wrap::new(2).key(2)),
            ],
        );

        let right = Node::with_children(
            Wrap::new(42),
            vec![
                Node::new(Wrap::new(1).key(2)),
                Node::new(Wrap::new(2).key(1)),
            ],
        );

        let expected = Node::with_children(
            vec![NumUpdate::SwapChildren(0, 1)],
            vec![
                Node::new(vec![NumUpdate::Delta(-1)]),
                Node::new(vec![NumUpdate::Delta(1)]),
            ],
        );

        assert_eq!(expected, conciliate::<'_, Stub>(left, right));
    }

    #[test]
    fn deals_with_as_is_termination() {
        let left = Node::with_children(
            Wrap::new(42).as_is(),
            vec![
                Node::new(Wrap::new(1).key(1)),
                Node::new(Wrap::new(2).key(2)),
            ],
        );

        let right = Node::with_children(
            Wrap::new(42),
            vec![
                Node::new(Wrap::new(1).key(2)),
                Node::new(Wrap::new(2).key(1)),
            ],
        );

        let expected = Node::new(vec![]);

        assert_eq!(expected, conciliate::<'_, Stub>(left, right));
    }
}
