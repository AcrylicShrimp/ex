use std::num::NonZeroU64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(NonZeroU64);

impl NodeId {
    pub fn new(id: u64) -> Self {
        NodeId(NonZeroU64::new(id).unwrap())
    }
}
