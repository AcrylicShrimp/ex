use crate::{Function, FunctionId, TypeIdTable, UserStruct, UserStructId};
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct Program {
    pub type_id_table: TypeIdTable,
    pub functions: HashMap<FunctionId, Function>,
    pub user_structs: HashMap<UserStructId, UserStruct>,
}

impl Program {
    pub fn new() -> Self {
        Default::default()
    }
}
