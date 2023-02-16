use std::ptr::null_mut;

pub struct Chunk {
    data: Vec<u8>,
    ptr: *mut u8,
}

impl Chunk {
    pub fn with_capacity(capacity: usize) -> Self {
        let mut chunk = Self {
            data: vec![0; capacity],
            ptr: null_mut(),
        };
        chunk.ptr = chunk.data.as_mut_ptr();
        chunk
    }

    pub fn len(&self) -> usize {
        self.ptr as usize - self.data.as_ptr() as usize
    }

    pub fn capacity(&self) -> usize {
        self.data.len()
    }

    pub fn remain(&self) -> usize {
        self.capacity() - self.len()
    }

    pub fn alloc(&mut self, size: usize) -> Option<*mut u8> {
        if self.remain() < size {
            return None;
        }

        let ptr = self.ptr;
        self.ptr = unsafe { self.ptr.add(size) };
        Some(ptr)
    }
}

// NOTE: It is safe to implement "Send" for the "Chunk", because the "ptr" is pointing memory location allocated by the "data".
unsafe impl Send for Chunk {}
