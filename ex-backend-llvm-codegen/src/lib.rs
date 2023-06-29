mod binary_op_dispatchers;
mod unary_op_dispatchers;

use proc_macro::TokenStream;
use proc_macro_error::*;

#[proc_macro]
#[proc_macro_error]
pub fn register_binary_op_dispatchers(item: TokenStream) -> TokenStream {
    binary_op_dispatchers::register_binary_op_dispatchers(item)
}

#[proc_macro]
#[proc_macro_error]
pub fn register_unary_op_dispatchers(item: TokenStream) -> TokenStream {
    unary_op_dispatchers::register_unary_op_dispatchers(item)
}
