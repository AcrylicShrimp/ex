mod pos;
mod source_file;
mod source_map;
mod span;

pub use pos::*;
pub use source_file::*;
pub use source_map::*;
pub use span::*;

#[cfg(test)]
mod tests {
    use super::*;
    use rand::{distributions::Uniform, thread_rng, Rng};
    use std::{path::PathBuf, sync::Arc};

    #[test]
    fn ex_span() {
        let mut source_map = SourceMap::new();

        fn add_source_file(source_map: &mut SourceMap) -> Arc<SourceFile> {
            let content = thread_rng()
                .sample_iter(&Uniform::new_inclusive(
                    char::from_u32(20).unwrap(),
                    char::MAX,
                ))
                .map(char::from)
                .take(thread_rng().gen_range(0..4096))
                .collect::<String>();
            let name = thread_rng()
                .sample_iter(&Uniform::new_inclusive(
                    char::from_u32(20).unwrap(),
                    char::MAX,
                ))
                .map(char::from)
                .take(thread_rng().gen_range(0..32))
                .collect::<String>();
            let path = thread_rng()
                .sample_iter(&Uniform::new_inclusive(
                    char::from_u32(20).unwrap(),
                    char::MAX,
                ))
                .map(char::from)
                .take(thread_rng().gen_range(0..128))
                .collect::<String>();
            return source_map.add_source_file(content, name, Some(PathBuf::from(path)));
        }

        let source_file_1 = add_source_file(&mut source_map);
        let source_file_2 = add_source_file(&mut source_map);

        assert_eq!(source_file_1.span().low, Pos::from(0));
        assert_eq!(
            source_file_1.span().high,
            Pos::from(source_file_1.content().len() as u32)
        );

        assert_eq!(
            source_file_2.span().low,
            Pos::from(source_file_1.content().len() as u32)
        );
        assert_eq!(
            source_file_2.span().high,
            Pos::from((source_file_1.content().len() + source_file_2.content().len()) as u32)
        );
    }
}
