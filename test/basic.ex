# Comment.

# Hello, world!

# Docstring.

###
### Hello, world!
### This is a struct `Foo`.
### It has no field.
###
struct Foo { }

# Module statement.

mod module_name {
  mod inner_module_name {

  }
}

# Use statement.

use self::self_module_name;
use super::super_module_name;

use module_name;
use module_name::inner_module;
use module_name::inner_module::StructName;
use module_name::inner_module::function_name;

use module_name::*;
use module_name::inner_module::*;

use module_name::inner_module::{StructName, function_name};

use {
  self::self_module_name,
  super::super_module_name,
  module_name,
  module_name::inner_module,
  module_name::inner_module::StructName,
  module_name::inner_module::function_name,
  module_name::*,
  module_name::inner_module::*,
  module_name::inner_module::{StructName, function_name},
};

# Use statement with rename.

use module_name as new_module_name;
use module_name::inner_module as new_inner_module_name;

# Use statement for re-export. They can be used in current module(file).

pub use module_name;
pub use module_name::inner_module;
pub use module_name::inner_module::StructName;
pub use module_name::inner_module::function_name;

pub use module_name::*;
pub use module_name::inner_module::*;

pub use module_name as new_module_name;
pub use module_name::inner_module as new_inner_module_name;

# Type definition.

pub type NewTypeName = OldTypeName;

# Extern block for FFI.

extern {
  # Importing.
  fn imported_function_name_a();
  pub fn imported_function_name_b(param: i32) -> i32;

  # Exporting.
  fn exported_function_name_a() {

  }

  pub fn exported_function_name_b(param: i32) -> i32 {
    return param;
  }
}

# Function definition.

fn function_name(param_name: ParamType) -> ReturnType {
  return ReturnType {
    value: param_name.value,
  };
}

# Struct definition.

struct StructName {
  field_name_0: FieldType;
  field_name_1: FieldType;
}

# Struct method implementation.

impl StructName {
  pub fn new(field_name_0: FieldType, field_name_1: FieldType) -> Self {
    return Self {
      field_name_0: field_name_0,
      field_name_1: field_name_1,
    };
  }
}

# Struct literal.

fn new_struct_name(field_name_0: FieldType, field_name_1: FieldType) -> StructName {
  return StructName {
    field_name_0: field_name_0,
    field_name_1: field_name_1,
  };
}

# Primitive type.

fn primitive_types() {
  let primitive: bool;
  let primitive: char;
  let primitive: i8;
  let primitive: i16;
  let primitive: i32;
  let primitive: i64;
  let primitive: i128;
  let primitive: u8;
  let primitive: u16;
  let primitive: u32;
  let primitive: u64;
  let primitive: u128;
  let primitive: isize;
  let primitive: usize;
  let primitive: f32;
  let primitive: f64;
  let primitive: core::range::Range<usize>;
  let primitive: core::range::RangeInclusive<usize>;
}

# Attribute-like tag.
# Tags must be followed by any top-level items.

![tag_name, tag_name, tag_param=tag_arg, tag_param=tag_arg]
struct TaggedStruct { }

![tag_name, tag_name, tag_param=tag_arg, tag_param=tag_arg]
fn tagged_function() { }

# Generics.

struct GenericStruct<T0, T1, T2> {
  a: T0;
  b: T1;
  c: T2;
}

fn generic_func<T0, T1, T2>(a: T0, b: T1, c: T2) -> GenericStruct<T0, T1, T2> {
  return GenericStruct {
    a: a,
    b: b,
    c: c,
  };
}

# Operator overloading.

pub fn operator + (lhs: i32, rhs: i32) -> i32 {
  return core::intrinsics::add_i32_i32(lhs, rhs);
}

pub fn operator as (lhs: i32) -> i64 {
  return core::intrinsics::convert_i32_i64(lhs);
}

# Interface.

pub interface InterfaceName {
  fn static_method_name();
  fn move_method_name(self);
  fn ref_method_name(self ref);
}

![impl clone, copy]
struct Foo {}

impl InterfaceName for Foo {
  fn static_method_name() {

  }

  fn move_method_name(self) {

  }

  fn ref_method_name(self ref) {

  }
}
