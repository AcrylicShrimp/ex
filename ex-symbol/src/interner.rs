use crate::{Chunk, Symbol};
use rustc_hash::FxHashMap;
use std::{cmp::max, ptr::copy_nonoverlapping, slice::from_raw_parts, str::from_utf8_unchecked};

#[derive(Default)]
pub struct Interner {
    strs: Vec<&'static str>,
    reversed: FxHashMap<&'static str, Symbol>,
    chunks: Vec<Chunk>,
}

impl Interner {
    pub const CHUNK_SIZE: usize = 4096;

    pub fn new() -> Self {
        Self::default()
    }

    pub fn str(&self, symbol: Symbol) -> &'static str {
        self.strs[u32::from(symbol) as usize]
    }

    pub fn intern(&mut self, str: impl AsRef<str>) -> Symbol {
        let str = str.as_ref();

        if let Some(&reverse) = self.reversed.get(str) {
            return reverse;
        }

        let ptr = self
            .chunks
            .last_mut()
            .and_then(|chunk| chunk.alloc(str.len()))
            .unwrap_or_else(|| {
                let mut chunk = Chunk::with_capacity(max(Self::CHUNK_SIZE, str.len()));
                let ptr = chunk.alloc(str.len()).unwrap();
                self.chunks.push(chunk);
                ptr
            });

        unsafe {
            copy_nonoverlapping(str.as_ptr(), ptr, str.len());
        }

        let str = unsafe { from_utf8_unchecked(from_raw_parts(ptr, str.len())) };
        let symbol = Symbol::new(self.strs.len() as _);
        self.strs.push(str);
        self.reversed.insert(str, symbol);
        symbol
    }
}
