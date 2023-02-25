use crate::NodeId;

#[derive(Default, Debug, Clone, Hash)]
pub struct NodeIdAllocator {
    next_id: u64,
}

impl NodeIdAllocator {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn allocate(&mut self) -> NodeId {
        self.next_id += 1;
        NodeId::new(self.next_id)
    }
}
