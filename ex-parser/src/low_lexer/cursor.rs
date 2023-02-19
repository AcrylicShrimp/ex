use std::str::Chars;

#[derive(Debug)]
pub struct Cursor<'s> {
    chars: Chars<'s>,
    initial_length: u32,
}

impl<'s> Cursor<'s> {
    pub fn new(src: impl Into<&'s str>) -> Self {
        let src = src.into();
        debug_assert!(src.len() <= u32::MAX as usize);

        Self {
            chars: src.chars(),
            initial_length: src.len() as u32,
        }
    }

    pub fn len_consumed(&self) -> u32 {
        self.initial_length - self.chars.as_str().len() as u32
    }

    pub fn first(&self) -> char {
        self.lookup(0)
    }

    pub fn second(&self) -> char {
        self.lookup(1)
    }

    pub fn lookup(&self, offset: u32) -> char {
        self.chars.clone().nth(offset as usize).unwrap_or('\0')
    }

    pub fn consume(&mut self) -> Option<char> {
        self.chars.next()
    }
}
