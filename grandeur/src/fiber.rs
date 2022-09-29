use alloc::vec::Vec;

use crate::node::{Node, OwnedNodePath};

pub trait Estimate {
    fn cost(&self) -> usize;
}

#[derive(Clone, Debug)]
pub struct Options {
    pub chunk_cost: usize,
    pub break_cost: usize,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Chunk<T> {
    min_path_length: usize,
    cost: usize,
    updates: Vec<(OwnedNodePath, Vec<T>)>,
}

impl<T> Chunk<T> {
    fn new() -> Self {
        Chunk {
            min_path_length: usize::MAX,
            cost: 0,
            updates: Vec::new(),
        }
    }

    pub fn min_path_length(&self) -> usize {
        self.min_path_length
    }

    pub fn cost(&self) -> usize {
        self.cost
    }

    pub fn updates(self) -> Vec<(OwnedNodePath, Vec<T>)> {
        self.updates
    }
}

pub fn fiberize<T: Estimate>(options: Options, tree: Node<Vec<T>>) -> Vec<Chunk<T>> {
    let Options {
        chunk_cost,
        break_cost,
    } = options;

    let mut node_updates = Vec::new();
    tree.visit_map(|entry| {
        let updates = entry.data;
        if updates.is_empty() {
            return;
        }

        let mut path = Vec::with_capacity(entry.path.len());
        path.extend_from_slice(entry.path);
        node_updates.push((path, updates));
    });

    // Sort by path length, and then lexically, essentially re-arranging in breadth-first order.
    // FIXME: Implement breadth-first traversal for `Node` directly.
    node_updates.sort_unstable_by(|a, b| a.0.len().cmp(&b.0.len()).then_with(|| a.0.cmp(&b.0)));

    // Re-group into chunks

    let mut chunks = Vec::new();
    let mut current_chunk = Chunk::new();

    for (path, updates) in node_updates {
        let mut cost = 0usize;
        let mut cost_till = Vec::with_capacity(updates.len());

        for u in &updates {
            cost = cost.saturating_add(u.cost());
            cost_till.push(cost);
        }

        let mut rest_cost = chunk_cost.saturating_sub(current_chunk.cost);

        if rest_cost < cost_till[0].saturating_add(break_cost) {
            chunks.push(core::mem::replace(&mut current_chunk, Chunk::new()));
            rest_cost = chunk_cost;
        }

        let mut updates = updates;
        let mut cost_till = cost_till;
        while !updates.is_empty() && rest_cost < cost {
            let break_idx = cost_till
                .binary_search(&(rest_cost - break_cost))
                .map_or_else(|x| x, |x| x + 1)
                .max(1);

            let cost_till_break = cost_till[break_idx - 1];
            cost -= cost_till_break;
            current_chunk.cost = current_chunk
                .cost
                .saturating_add(cost_till_break)
                .saturating_add(break_cost);
            cost_till = cost_till.split_off(break_idx);
            for c in &mut cost_till {
                *c -= cost_till_break;
            }

            let next_updates = updates.split_off(break_idx);
            current_chunk.min_path_length = current_chunk.min_path_length.min(path.len());
            current_chunk.updates.push((path.clone(), updates));
            updates = next_updates;

            chunks.push(core::mem::replace(&mut current_chunk, Chunk::new()));
            rest_cost = chunk_cost;
        }

        if !updates.is_empty() {
            current_chunk.min_path_length = current_chunk.min_path_length.min(path.len());
            current_chunk.updates.push((path, updates));
            current_chunk.cost += cost;
        }
    }

    if !current_chunk.updates.is_empty() {
        chunks.push(current_chunk);
    }

    chunks
}

#[cfg(test)]
mod tests {
    use super::*;

    use alloc::vec;

    #[derive(PartialEq, Eq, Debug)]
    struct Wrap(usize);

    impl Estimate for Wrap {
        fn cost(&self) -> usize {
            self.0
        }
    }

    #[test]
    fn it_works() {
        let tree = Node::with_children(
            vec![],
            vec![
                Node::with_children(
                    vec![Wrap(1), Wrap(2), Wrap(3), Wrap(4), Wrap(5)],
                    vec![
                        Node::new(vec![Wrap(1), Wrap(2), Wrap(3)]),
                        Node::new(vec![Wrap(3), Wrap(2), Wrap(1)]),
                    ],
                ),
                Node::with_children(
                    vec![Wrap(5), Wrap(4), Wrap(3), Wrap(2), Wrap(1)],
                    vec![
                        Node::new(vec![Wrap(1), Wrap(2), Wrap(3)]),
                        Node::new(vec![Wrap(3), Wrap(2), Wrap(1)]),
                        Node::new(vec![Wrap(99), Wrap(99), Wrap(99)]),
                    ],
                ),
            ],
        );

        let options = Options {
            chunk_cost: 20,
            break_cost: 5,
        };

        let expected = vec![
            Chunk {
                min_path_length: 1,
                cost: 15,
                updates: vec![(vec![0], vec![Wrap(1), Wrap(2), Wrap(3), Wrap(4), Wrap(5)])],
            },
            Chunk {
                min_path_length: 1,
                cost: 15,
                updates: vec![(vec![1], vec![Wrap(5), Wrap(4), Wrap(3), Wrap(2), Wrap(1)])],
            },
            Chunk {
                min_path_length: 2,
                cost: 18,
                updates: vec![
                    (vec![0, 0], vec![Wrap(1), Wrap(2), Wrap(3)]),
                    (vec![0, 1], vec![Wrap(3), Wrap(2), Wrap(1)]),
                    (vec![1, 0], vec![Wrap(1), Wrap(2), Wrap(3)]),
                ],
            },
            Chunk {
                min_path_length: 2,
                cost: 6,
                updates: vec![(vec![1, 1], vec![Wrap(3), Wrap(2), Wrap(1)])],
            },
            Chunk {
                min_path_length: 2,
                cost: 104,
                updates: vec![(vec![1, 2], vec![Wrap(99)])],
            },
            Chunk {
                min_path_length: 2,
                cost: 104,
                updates: vec![(vec![1, 2], vec![Wrap(99)])],
            },
            Chunk {
                min_path_length: 2,
                cost: 104,
                updates: vec![(vec![1, 2], vec![Wrap(99)])],
            },
        ];

        assert_eq!(expected, fiberize(options, tree));
    }
}
