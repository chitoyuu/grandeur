use alloc::vec;
use alloc::vec::Vec;
use core::fmt::Debug;
use core::hash::Hash;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Node<T> {
    data: T,
    children: Vec<Self>,
}

impl<T> Node<T> {
    pub fn new(data: T) -> Self {
        Node {
            data,
            children: Vec::new(),
        }
    }

    pub fn with_children(data: T, children: Vec<Self>) -> Self {
        Node { data, children }
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut T {
        &mut self.data
    }

    pub fn children(&self) -> &[Self] {
        &self.children
    }

    pub fn children_mut(&mut self) -> &mut Vec<Self> {
        &mut self.children
    }

    pub fn get(&self, path: NodePath<'_>) -> Option<&Self> {
        let mut cur = self;
        for &idx in path {
            let idx = usize::try_from(idx).ok()?;
            cur = cur.children.get(idx)?;
        }
        Some(cur)
    }

    pub fn get_mut(&mut self, path: NodePath<'_>) -> Option<&mut Self> {
        let mut cur = self;
        for &idx in path {
            let idx = usize::try_from(idx).ok()?;
            cur = cur.children.get_mut(idx)?;
        }
        Some(cur)
    }
}

impl<T> Node<T> {
    pub fn map<F, R>(self, mut f: F) -> Node<R>
    where
        F: FnMut(T) -> R,
    {
        self.try_map::<_, _, !>(|d| Ok(f(d))).unwrap()
    }

    pub fn try_map<F, R, E>(self, mut f: F) -> Result<Node<R>, E>
    where
        F: FnMut(T) -> Result<R, E>,
    {
        self.try_visit_map(|entry| f(entry.data))
    }

    pub fn cast<R>(self) -> Node<R>
    where
        R: From<T>,
    {
        self.map(From::from)
    }

    pub fn try_cast<R>(self) -> Result<Node<R>, <R as TryFrom<T>>::Error>
    where
        R: TryFrom<T>,
    {
        self.try_map(TryFrom::try_from)
    }

    pub fn to_ref(&self) -> Node<&'_ T> {
        let data = &self.data;
        let children = self.children.iter().map(Node::to_ref).collect::<Vec<_>>();

        Node { data, children }
    }

    pub fn to_mut(&mut self) -> Node<&'_ mut T> {
        let data = &mut self.data;
        let children = self
            .children
            .iter_mut()
            .map(Node::to_mut)
            .collect::<Vec<_>>();

        Node { data, children }
    }

    pub fn to_unit(&self) -> Node<()> {
        let children = self.children.iter().map(Node::to_unit).collect::<Vec<_>>();
        Node { data: (), children }
    }
}

#[non_exhaustive]
pub struct VisitEntry<'a, T> {
    pub data: T,
    pub path: NodePath<'a>,
    pub children: &'a mut Vec<Node<T>>,
}

impl<T> Node<T> {
    /// Visit and transform the node tree in depth-first order. The callback may mutate
    /// each visited node before recurring into it.
    ///
    /// See [`Node::visit_apply`] for a more powerful traversal function.
    pub fn visit_map<F, R>(self, mut f: F) -> Node<R>
    where
        F: FnMut(VisitEntry<'_, T>) -> R,
    {
        self.try_visit_map::<_, _, !>(|e| Ok(f(e))).unwrap()
    }

    /// Visit and transform the node tree in depth-first order. The callback may mutate
    /// each visited node before recurring into it.
    ///
    /// See [`Node::try_visit_apply`] for a more powerful traversal function.
    pub fn try_visit_map<F, R, E>(self, mut f: F) -> Result<Node<R>, E>
    where
        F: FnMut(VisitEntry<'_, T>) -> Result<R, E>,
    {
        let unit = self.to_unit();
        self.try_visit_apply(unit, |entry, _| f(entry))
    }

    /// Visit and transform two node trees, zipped, in depth-first order. The callback may
    /// mutate each pair of visited nodes before recurring into them.
    pub fn visit_apply<F, R, C>(self, other: Node<R>, mut f: F) -> Node<C>
    where
        F: FnMut(VisitEntry<'_, T>, VisitEntry<'_, R>) -> C,
    {
        self.try_visit_apply::<_, _, _, !>(other, |a, b| Ok(f(a, b)))
            .unwrap()
    }

    /// Visit and transform two node trees, zipped, in depth-first order. The callback may
    /// mutate each pair of visited nodes before recurring into them.
    pub fn try_visit_apply<F, R, C, E>(self, other: Node<R>, mut f: F) -> Result<Node<C>, E>
    where
        F: FnMut(VisitEntry<'_, T>, VisitEntry<'_, R>) -> Result<C, E>,
    {
        enum Frame<T, R, C> {
            Pre(Node<T>, Node<R>),
            Post(C, Vec<Node<T>>, Vec<Node<R>>, Vec<Node<C>>),
        }

        let mut path = Vec::<u32>::new();
        let mut stack = vec![Frame::Pre(self, other)];

        while let Some(frame) = stack.pop() {
            match frame {
                Frame::Pre(mut left, mut right) => {
                    let left_entry = VisitEntry {
                        data: left.data,
                        path: &path,
                        children: &mut left.children,
                    };

                    let right_entry = VisitEntry {
                        data: right.data,
                        path: &path,
                        children: &mut right.children,
                    };

                    let data = f(left_entry, right_entry)?;

                    let mut left = left.children;
                    let mut right = right.children;
                    left.reverse();
                    right.reverse();

                    stack.push(Frame::Post(data, left, right, Vec::new()));
                }
                Frame::Post(data, mut left, mut right, children) => {
                    if let (Some(left_c), Some(right_c)) = (left.pop(), right.pop()) {
                        path.push(u32::try_from(children.len()).unwrap());
                        stack.push(Frame::Post(data, left, right, children));
                        stack.push(Frame::Pre(left_c, right_c));
                    } else {
                        let node = Node { data, children };

                        if let Some(Frame::Post(.., ref mut children)) = stack.last_mut() {
                            children.push(node);
                            path.pop();
                        } else {
                            return Ok(node);
                        }
                    }
                }
            }
        }

        unreachable!()
    }
}

impl<T> Node<T> {
    pub fn apply_prod<F, R, C>(&self, other: &Node<R>, mut f: F) -> Node<C>
    where
        F: FnMut(&T, &R) -> C,
    {
        self.try_apply_prod::<_, _, _, !>(other, |a, b| Ok(f(a, b)))
            .unwrap()
    }

    pub fn try_apply_prod<F, R, C, E>(&self, other: &Node<R>, mut f: F) -> Result<Node<C>, E>
    where
        F: FnMut(&T, &R) -> Result<C, E>,
    {
        let data = f(&self.data, &other.data)?;
        let mut children = Vec::with_capacity(self.children.len() * other.children.len());

        for a in &self.children {
            for b in &other.children {
                children.push(a.try_apply_prod(b, &mut f)?);
            }
        }

        Ok(Node { data, children })
    }
}

pub type NodePath<'a> = &'a [u32];
pub type OwnedNodePath = Vec<u32>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_visit_apply() {
        let left = Node::with_children(
            1,
            vec![
                Node::new(2),
                Node::with_children(3, vec![Node::new(4), Node::new(5)]),
                Node::new(6),
            ],
        );

        let right = Node::with_children(
            1,
            vec![
                Node::new(2),
                Node::with_children(3, vec![Node::new(4), Node::new(5)]),
                Node::new(6),
            ],
        );

        let expected = Node::with_children(
            2,
            vec![
                Node::new(4),
                Node::with_children(6, vec![Node::new(8), Node::new(10)]),
                Node::new(12),
            ],
        );

        assert_eq!(expected, left.visit_apply(right, |a, b| a.data + b.data));
    }
}
