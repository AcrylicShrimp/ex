use proc_macro::TokenStream;
use proc_macro_error::abort;
use quote::{format_ident, quote};
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token::Bracket,
    Ident, Path, Result as SynResult, Token,
};

pub struct Input {
    pub dispatcher: Ident,
    pub bracket_token: Bracket,
    pub accepters: Punctuated<Accepter, Token![,]>,
}

impl Parse for Input {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let content;
        Ok(Self {
            dispatcher: input.parse()?,
            bracket_token: bracketed!(content in input),
            accepters: content.parse_terminated(Accepter::parse, Token![,])?,
        })
    }
}

pub struct Accepter {
    pub operator: Path,
    pub arrow_token: Token![=>],
    pub name: Ident,
}

impl Parse for Accepter {
    fn parse(input: ParseStream) -> SynResult<Self> {
        Ok(Self {
            operator: input.parse()?,
            arrow_token: input.parse()?,
            name: input.parse()?,
        })
    }
}

pub fn register_unary_op_dispatchers(item: TokenStream) -> TokenStream {
    let Input {
        dispatcher,
        accepters,
        ..
    } = parse_macro_input!(item as Input);
    let mut statements = Vec::with_capacity(accepters.len());

    for accepter in accepters {
        let name = accepter.name.to_string();
        let pieces = name.split('_').collect::<Vec<_>>();
        let mut pieces_iter = pieces.iter().cloned().rev();
        let lhs = if let Some(lhs) = pieces_iter.next() {
            lhs
        } else {
            abort!("invalid unary operator accepter name: {}", accepter.name);
        };

        let mut lhs = lhs.to_string();
        lhs[..1].make_ascii_uppercase();
        let lhs = format_ident!("LLVM{}", lhs);

        let name = accepter.name;
        let operator = accepter.operator;

        statements.push(quote! {
            #dispatcher.register_unary_op_dispatcher::<#lhs>(#operator, #name);
        });
    }

    TokenStream::from(quote! {
        #(#statements)*
    })
}
