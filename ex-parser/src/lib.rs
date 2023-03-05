mod ast;
mod lexer;
mod low_lexer;
mod parser;

pub use ast::*;
pub use lexer::*;
pub use low_lexer::*;
pub use parser::*;

use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin, SubDiagnostics};
use ex_span::SourceFile;
use ex_symbol::Symbol;
use std::sync::{mpsc::Sender, Arc};

// How about to include token delimiter for error recovery?

pub fn parse_ast(file: Arc<SourceFile>, diagnostics: Arc<Sender<Diagnostics>>) -> ASTProgram {
    let mut id_alloc = NodeIdAllocator::new();
    let token_iter = token_iter(&file);
    let mut parser = Parser::new(token_iter, file.clone(), diagnostics.clone());
    parse_ast_program(&mut id_alloc, &mut parser, &file, &diagnostics)
}

fn parse_ast_program(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> ASTProgram {
    let mut top_levels = Vec::new();

    while parser.is_exists() {
        if let Ok(top_level) = parse_ast_toplevel(id_alloc, parser, file, diagnostics) {
            top_levels.push(top_level);
        }
    }

    let span = match (top_levels.first(), top_levels.last()) {
        (Some(first), Some(last)) => first.span.to(last.span),
        _ => parser.span(),
    };

    ASTProgram { top_levels, span }
}

fn parse_ast_toplevel(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTTopLevel, ()> {
    if parser.first().is_keyword(*crate::KEYWORD_FN) {
        let item = parse_function(id_alloc, parser, file, diagnostics)?;
        Ok(ASTTopLevel {
            id: id_alloc.allocate(),
            span: item.span,
            kind: ASTTopLevelKind::Function(item),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_STRUCT) {
        let item = parse_struct(id_alloc, parser, file, diagnostics)?;
        Ok(ASTTopLevel {
            id: id_alloc.allocate(),
            span: item.span,
            kind: ASTTopLevelKind::Struct(item),
        })
    } else {
        unexpected_token(
            parser.first(),
            &[*crate::KEYWORD_FN, *crate::KEYWORD_STRUCT],
            file,
            diagnostics,
        );

        // Skip to next top level.
        while parser.is_exists()
            && !parser.first().is_keyword(*crate::KEYWORD_FN)
            && !parser.first().is_keyword(*crate::KEYWORD_STRUCT)
        {
            parser.consume();
        }

        Err(())
    }
}

fn parse_function(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTFunction, ()> {
    let signature = parse_function_signature(id_alloc, parser, file, diagnostics)?;

    let body_block = parse_block(id_alloc, parser, file, diagnostics)?;

    let span = signature.span.to(body_block.span);

    Ok(ASTFunction {
        signature,
        body_block,
        span,
    })
}

fn parse_function_signature(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTFunctionSignature, ()> {
    let keyword_fn = if let Some(id) = parser.first().keyword(*crate::KEYWORD_FN) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_FN], file, diagnostics);
        return Err(());
    };

    let function_name = if let Some(id) = parser.first().id() {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
        return Err(());
    };

    let paren_open = if let Some(id) = parser.first().kind(TokenKind::OpenParen) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::OPEN_PAREN], file, diagnostics);
        return Err(());
    };

    let parameters = parse_function_parameters(id_alloc, parser, file, diagnostics)?;

    let paren_close = if let Some(id) = parser.first().kind(TokenKind::CloseParen) {
        parser.consume();
        id
    } else {
        unexpected_token(
            parser.first(),
            &[*crate::COMMA, *crate::CLOSE_PAREN],
            file,
            diagnostics,
        );
        return Err(());
    };

    let return_type = if parser.first().is_kind(TokenKind::Arrow) {
        Some(parse_function_return_type(
            id_alloc,
            parser,
            file,
            diagnostics,
        )?)
    } else {
        None
    };

    let span = keyword_fn.span.to(return_type
        .as_ref()
        .map_or_else(|| paren_close.span, |return_type| return_type.span));

    Ok(ASTFunctionSignature {
        keyword_fn,
        name: function_name,
        paren_open,
        parameters,
        paren_close,
        return_type,
        span,
    })
}

fn parse_function_parameters(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<Vec<ASTFunctionParameter>, ()> {
    let mut parameters = Vec::new();

    while parser.is_exists() && !parser.first().is_kind(TokenKind::CloseParen) {
        let parameter_name = if let Some(id) = parser.first().id() {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
            return Err(());
        };

        let colon = if let Some(id) = parser.first().kind(TokenKind::Colon) {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::COLON], file, diagnostics);
            return Err(());
        };

        let typename = parse_typename(id_alloc, parser, file, diagnostics)?;

        let comma = parser.first().kind(TokenKind::Comma);
        if comma.is_some() {
            parser.consume();
        }

        let span = parameter_name
            .span
            .to(comma.map_or_else(|| typename.span, |comma| comma.span));

        parameters.push(ASTFunctionParameter {
            name: parameter_name,
            colon,
            typename,
            comma,
            span,
        })
    }

    Ok(parameters)
}

fn parse_function_return_type(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTFunctionReturnType, ()> {
    let arrow = if let Some(id) = parser.first().kind(TokenKind::Arrow) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::ARROW], file, diagnostics);
        return Err(());
    };

    let typename = parse_typename(id_alloc, parser, file, diagnostics)?;

    let span = arrow.span.to(typename.span);

    Ok(ASTFunctionReturnType {
        arrow,
        typename,
        span,
    })
}

fn parse_struct(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTStruct, ()> {
    let keyword_struct = if let Some(id) = parser.first().keyword(*crate::KEYWORD_STRUCT) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_STRUCT], file, diagnostics);
        return Err(());
    };

    let struct_name = if let Some(id) = parser.first().id() {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
        return Err(());
    };

    let brace_open = if let Some(id) = parser.first().kind(TokenKind::OpenBrace) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::OPEN_PAREN], file, diagnostics);
        return Err(());
    };

    let fields = parse_struct_fields(id_alloc, parser, file, diagnostics)?;

    let brace_close = if let Some(id) = parser.first().kind(TokenKind::CloseBrace) {
        parser.consume();
        id
    } else {
        unexpected_token(
            parser.first(),
            &[*crate::COMMA, *crate::CLOSE_PAREN],
            file,
            diagnostics,
        );
        return Err(());
    };

    let span = keyword_struct.span.to(brace_close.span);

    Ok(ASTStruct {
        keyword_struct,
        name: struct_name,
        brace_open,
        fields,
        brace_close,
        span,
    })
}

fn parse_struct_fields(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<Vec<ASTStructField>, ()> {
    let mut fields = Vec::new();

    while parser.is_exists() && !parser.first().is_kind(TokenKind::CloseBrace) {
        let field_name = if let Some(id) = parser.first().id() {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
            return Err(());
        };

        let colon = if let Some(id) = parser.first().kind(TokenKind::Colon) {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::COLON], file, diagnostics);
            return Err(());
        };

        let typename = parse_typename(id_alloc, parser, file, diagnostics)?;

        let semicolon = if let Some(id) = parser.first().kind(TokenKind::Semicolon) {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::SEMICOLON], file, diagnostics);
            return Err(());
        };

        let span = field_name.span.to(semicolon.span);

        fields.push(ASTStructField {
            name: field_name,
            colon,
            typename,
            semicolon,
            span,
        })
    }

    Ok(fields)
}

fn parse_statement(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTStatement, ()> {
    if parser.first().is_kind(TokenKind::OpenBrace) {
        let block_statement = parse_block(id_alloc, parser, file, diagnostics)?;
        Ok(ASTStatement {
            id: id_alloc.allocate(),
            span: block_statement.span,
            kind: ASTStatementKind::Block(block_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_LET) {
        let let_statement = parse_let(id_alloc, parser, file, diagnostics)?;
        Ok(ASTStatement {
            id: id_alloc.allocate(),
            span: let_statement.span,
            kind: ASTStatementKind::Let(let_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_IF) {
        let if_statement = parse_if(id_alloc, parser, file, diagnostics)?;
        Ok(ASTStatement {
            id: id_alloc.allocate(),
            span: if_statement.span,
            kind: ASTStatementKind::If(if_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_LOOP) {
        let loop_statement = parse_loop(id_alloc, parser, file, diagnostics)?;
        Ok(ASTStatement {
            id: id_alloc.allocate(),
            span: loop_statement.span,
            kind: ASTStatementKind::Loop(loop_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_WHILE) {
        let while_statement = parse_while(id_alloc, parser, file, diagnostics)?;
        Ok(ASTStatement {
            id: id_alloc.allocate(),
            span: while_statement.span,
            kind: ASTStatementKind::While(while_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_BREAK) {
        let break_statement = parse_break(parser, file, diagnostics)?;
        Ok(ASTStatement {
            id: id_alloc.allocate(),
            span: break_statement.span,
            kind: ASTStatementKind::Break(break_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_CONTINUE) {
        let continue_statement = parse_continue(parser, file, diagnostics)?;
        Ok(ASTStatement {
            id: id_alloc.allocate(),
            span: continue_statement.span,
            kind: ASTStatementKind::Continue(continue_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_RETURN) {
        let return_statement = parse_return(id_alloc, parser, file, diagnostics)?;
        Ok(ASTStatement {
            id: id_alloc.allocate(),
            span: return_statement.span,
            kind: ASTStatementKind::Return(return_statement),
        })
    } else {
        // TODO: We need to report error that contains above context, in case that the statement is not a row statement.
        parse_assignment_or_row(id_alloc, parser, file, diagnostics)
    }
}

fn parse_block(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTBlock, ()> {
    let brace_open = if let Some(id) = parser.first().kind(TokenKind::OpenBrace) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::OPEN_BRACE], file, diagnostics);
        return Err(());
    };

    let mut statements = Vec::new();

    while parser.is_exists() && !parser.first().is_kind(TokenKind::CloseBrace) {
        let statement = parse_statement(id_alloc, parser, file, diagnostics)?;
        statements.push(statement);
    }

    let brace_close = if let Some(id) = parser.first().kind(TokenKind::CloseBrace) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::CLOSE_BRACE], file, diagnostics);
        return Err(());
    };

    let span = brace_open.span.to(brace_close.span);

    Ok(ASTBlock {
        brace_open,
        statements,
        brace_close,
        span,
    })
}

fn parse_let(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTLet, ()> {
    let keyword_let = if let Some(id) = parser.first().keyword(*crate::KEYWORD_LET) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_LET], file, diagnostics);
        return Err(());
    };

    let name = if let Some(id) = parser.first().id() {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
        return Err(());
    };

    let let_type = if parser.first().is_kind(TokenKind::Colon) {
        Some(parse_let_type(id_alloc, parser, file, diagnostics)?)
    } else {
        None
    };

    let let_assignment = if parser.first().is_kind(TokenKind::Assign) {
        Some(parse_let_assignment(id_alloc, parser, file, diagnostics)?)
    } else {
        None
    };

    let semicolon = if let Some(id) = parser.first().kind(TokenKind::Semicolon) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::SEMICOLON], file, diagnostics);
        return Err(());
    };

    let span = keyword_let.span.to(semicolon.span);

    Ok(ASTLet {
        keyword_let,
        name,
        let_type,
        let_assignment,
        semicolon,
        span,
    })
}

fn parse_let_type(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTLetType, ()> {
    let colon = if let Some(id) = parser.first().kind(TokenKind::Colon) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::COLON], file, diagnostics);
        return Err(());
    };

    let typename = parse_typename(id_alloc, parser, file, diagnostics)?;

    let span = colon.span.to(typename.span);

    Ok(ASTLetType {
        colon,
        typename,
        span,
    })
}

fn parse_let_assignment(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTLetAssignment, ()> {
    let assignment = if let Some(id) = parser.first().kind(TokenKind::Assign) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::ASSIGN], file, diagnostics);
        return Err(());
    };

    let expression = parse_expression(true, id_alloc, parser, file, diagnostics)?;

    let span = assignment.span.to(expression.span);

    Ok(ASTLetAssignment {
        assignment,
        expression,
        span,
    })
}

fn parse_if(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTIf, ()> {
    let keyword_if = if let Some(id) = parser.first().keyword(*crate::KEYWORD_IF) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_IF], file, diagnostics);
        return Err(());
    };

    let condition = parse_expression(false, id_alloc, parser, file, diagnostics)?;

    let body_block = parse_block(id_alloc, parser, file, diagnostics)?;

    let single_else_ifs = parse_single_else_ifs(id_alloc, parser, file, diagnostics)?;

    let single_else = if parser.first().is_keyword(*crate::KEYWORD_ELSE) {
        Some(parse_single_else(id_alloc, parser, file, diagnostics)?)
    } else {
        None
    };

    let span = keyword_if.span.to(single_else
        .as_ref()
        .map(|single_else| single_else.span)
        .unwrap_or(body_block.span));

    Ok(ASTIf {
        keyword_if,
        expression: condition,
        body_block,
        single_else_ifs,
        single_else,
        span,
    })
}

fn parse_single_else_ifs(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<Vec<ASTSingleElseIf>, ()> {
    let mut single_else_ifs = Vec::new();

    while parser.is_exists() {
        let (keyword_else, keyword_if) = match (
            parser.first().keyword(*crate::KEYWORD_ELSE),
            parser.second().keyword(*crate::KEYWORD_IF),
        ) {
            (Some(keyword_else), Some(keyword_if)) => {
                parser.consume();
                parser.consume();
                (keyword_else, keyword_if)
            }
            _ => break,
        };

        let condition = parse_expression(false, id_alloc, parser, file, diagnostics)?;

        let body_block = parse_block(id_alloc, parser, file, diagnostics)?;

        let span = keyword_else.span.to(body_block.span);

        single_else_ifs.push(ASTSingleElseIf {
            keyword_else,
            keyword_if,
            expression: condition,
            body_block,
            span,
        });
    }

    Ok(single_else_ifs)
}

fn parse_single_else(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTSingleElse, ()> {
    let keyword_else = if let Some(id) = parser.first().keyword(*crate::KEYWORD_ELSE) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_ELSE], file, diagnostics);
        return Err(());
    };

    let body_block = parse_block(id_alloc, parser, file, diagnostics)?;

    let span = keyword_else.span.to(body_block.span);

    Ok(ASTSingleElse {
        keyword_else,
        body_block,
        span,
    })
}

fn parse_loop(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTLoop, ()> {
    let keyword_loop = if let Some(id) = parser.first().keyword(*crate::KEYWORD_LOOP) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_LOOP], file, diagnostics);
        return Err(());
    };

    let body_block = parse_block(id_alloc, parser, file, diagnostics)?;

    let span = keyword_loop.span.to(body_block.span);

    Ok(ASTLoop {
        keyword_loop,
        body_block,
        span,
    })
}

fn parse_while(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTWhile, ()> {
    let keyword_while = if let Some(id) = parser.first().keyword(*crate::KEYWORD_WHILE) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_WHILE], file, diagnostics);
        return Err(());
    };

    let expression = parse_expression(false, id_alloc, parser, file, diagnostics)?;

    let body_block = parse_block(id_alloc, parser, file, diagnostics)?;

    let span = keyword_while.span.to(body_block.span);

    Ok(ASTWhile {
        keyword_while,
        expression,
        body_block,
        span,
    })
}

fn parse_break(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTBreak, ()> {
    let keyword_break = if let Some(id) = parser.first().keyword(*crate::KEYWORD_BREAK) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_BREAK], file, diagnostics);
        return Err(());
    };

    let semicolon = if let Some(id) = parser.first().kind(TokenKind::Semicolon) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::SEMICOLON], file, diagnostics);
        return Err(());
    };

    let span = keyword_break.span.to(semicolon.span);

    Ok(ASTBreak {
        keyword_break,
        semicolon,
        span,
    })
}

fn parse_continue(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTContinue, ()> {
    let keyword_continue = if let Some(id) = parser.first().keyword(*crate::KEYWORD_CONTINUE) {
        parser.consume();
        id
    } else {
        unexpected_token(
            parser.first(),
            &[*crate::KEYWORD_CONTINUE],
            file,
            diagnostics,
        );
        return Err(());
    };

    let semicolon = if let Some(id) = parser.first().kind(TokenKind::Semicolon) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::SEMICOLON], file, diagnostics);
        return Err(());
    };

    let span = keyword_continue.span.to(semicolon.span);

    Ok(ASTContinue {
        keyword_continue,
        semicolon,
        span,
    })
}

fn parse_return(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTReturn, ()> {
    let keyword_return = if let Some(id) = parser.first().keyword(*crate::KEYWORD_RETURN) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_RETURN], file, diagnostics);
        return Err(());
    };

    let expression = if parser.is_exists() && !parser.first().is_kind(TokenKind::Semicolon) {
        Some(parse_expression(true, id_alloc, parser, file, diagnostics)?)
    } else {
        None
    };

    let semicolon = if let Some(id) = parser.first().kind(TokenKind::Semicolon) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::SEMICOLON], file, diagnostics);
        return Err(());
    };

    let span = keyword_return.span.to(semicolon.span);

    Ok(ASTReturn {
        keyword_return,
        expression,
        semicolon,
        span,
    })
}

fn parse_assignment_or_row(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTStatement, ()> {
    let left = parse_expression(true, id_alloc, parser, file, diagnostics)?;

    let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::Assign) {
        parser.consume();
        (id, None)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignAdd) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::Add))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignSub) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::Sub))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignMul) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::Mul))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignDiv) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::Div))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignMod) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::Mod))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignPow) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::Pow))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignBitOr) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::BitOr))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignBitAnd) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::BitAnd))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignBitXor) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::BitXor))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignShl) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::Shl))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignShr) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::Shr))
    } else if let Some(id) = parser.first().kind(TokenKind::AssignBitNot) {
        parser.consume();
        (id, Some(ASTAssignmentOperatorKind::BitNot))
    } else {
        let semicolon = if let Some(id) = parser.first().kind(TokenKind::Semicolon) {
            parser.consume();
            id
        } else {
            unexpected_token(
                parser.first(),
                &[
                    *crate::ASSIGN,
                    *crate::ASSIGN_ADD,
                    *crate::ASSIGN_SUB,
                    *crate::ASSIGN_MUL,
                    *crate::ASSIGN_DIV,
                    *crate::ASSIGN_MOD,
                    *crate::ASSIGN_POW,
                    *crate::ASSIGN_BIT_OR,
                    *crate::ASSIGN_BIT_AND,
                    *crate::ASSIGN_BIT_XOR,
                    *crate::ASSIGN_SHL,
                    *crate::ASSIGN_SHR,
                    *crate::SEMICOLON,
                ],
                file,
                diagnostics,
            );
            return Err(());
        };

        let span = left.span.to(semicolon.span);

        return Ok(ASTStatement {
            id: id_alloc.allocate(),
            kind: ASTStatementKind::Row(ASTRow {
                expression: left,
                semicolon,
                span,
            }),
            span,
        });
    };

    let right = parse_expression(true, id_alloc, parser, file, diagnostics)?;

    let semicolon = if let Some(id) = parser.first().kind(TokenKind::Semicolon) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::SEMICOLON], file, diagnostics);
        return Err(());
    };

    let span = left.span.to(semicolon.span);

    Ok(ASTStatement {
        id: id_alloc.allocate(),
        kind: ASTStatementKind::Assignment(ASTAssignment {
            left,
            operator,
            operator_kind,
            right,
            semicolon,
            span,
        }),
        span,
    })
}

fn parse_expression(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    parse_binary_expression_compare(allow_struct_literal, id_alloc, parser, file, diagnostics)
}

fn parse_binary_expression_compare(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_logical_or_and(
        allow_struct_literal,
        id_alloc,
        parser,
        file,
        diagnostics,
    )?;

    while parser.is_exists() {
        let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::Eq) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Eq)
        } else if let Some(id) = parser.first().kind(TokenKind::Ne) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Ne)
        } else if let Some(id) = parser.first().kind(TokenKind::Lt) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Lt)
        } else if let Some(id) = parser.first().kind(TokenKind::Gt) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Gt)
        } else if let Some(id) = parser.first().kind(TokenKind::Le) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Le)
        } else if let Some(id) = parser.first().kind(TokenKind::Ge) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Ge)
        } else {
            break;
        };

        let right =
            parse_binary_expression_logical_or_and(true, id_alloc, parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
            id: id_alloc.allocate(),
            kind: ASTExpressionKind::Binary(ASTBinaryExpression {
                left: Box::new(left),
                operator,
                operator_kind,
                right: Box::new(right),
                span,
            }),
            span,
        }
    }

    Ok(left)
}

fn parse_binary_expression_logical_or_and(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_arithmetic_add_sub(
        allow_struct_literal,
        id_alloc,
        parser,
        file,
        diagnostics,
    )?;

    while parser.is_exists() {
        let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::LogOr) {
            parser.consume();
            (id, ASTBinaryOperatorKind::LogOr)
        } else if let Some(id) = parser.first().kind(TokenKind::LogAnd) {
            parser.consume();
            (id, ASTBinaryOperatorKind::LogAnd)
        } else {
            break;
        };

        let right =
            parse_binary_expression_arithmetic_add_sub(true, id_alloc, parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
            id: id_alloc.allocate(),
            kind: ASTExpressionKind::Binary(ASTBinaryExpression {
                left: Box::new(left),
                operator,
                operator_kind,
                right: Box::new(right),
                span,
            }),
            span,
        }
    }

    Ok(left)
}

fn parse_binary_expression_arithmetic_add_sub(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_arithmetic_mul_div_mod(
        allow_struct_literal,
        id_alloc,
        parser,
        file,
        diagnostics,
    )?;

    while parser.is_exists() {
        let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::Add) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Add)
        } else if let Some(id) = parser.first().kind(TokenKind::Sub) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Sub)
        } else {
            break;
        };

        let right = parse_binary_expression_arithmetic_mul_div_mod(
            true,
            id_alloc,
            parser,
            file,
            diagnostics,
        )?;

        let span = left.span.to(right.span);

        left = ASTExpression {
            id: id_alloc.allocate(),
            kind: ASTExpressionKind::Binary(ASTBinaryExpression {
                left: Box::new(left),
                operator,
                operator_kind,
                right: Box::new(right),
                span,
            }),
            span,
        }
    }

    Ok(left)
}

fn parse_binary_expression_arithmetic_mul_div_mod(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_arithmetic_pow(
        allow_struct_literal,
        id_alloc,
        parser,
        file,
        diagnostics,
    )?;

    while parser.is_exists() {
        let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::Mul) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Mul)
        } else if let Some(id) = parser.first().kind(TokenKind::Div) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Div)
        } else if let Some(id) = parser.first().kind(TokenKind::Mod) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Mod)
        } else {
            break;
        };

        let right =
            parse_binary_expression_arithmetic_pow(true, id_alloc, parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
            id: id_alloc.allocate(),
            kind: ASTExpressionKind::Binary(ASTBinaryExpression {
                left: Box::new(left),
                operator,
                operator_kind,
                right: Box::new(right),
                span,
            }),
            span,
        }
    }

    Ok(left)
}

fn parse_binary_expression_arithmetic_pow(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_bit_shift(
        allow_struct_literal,
        id_alloc,
        parser,
        file,
        diagnostics,
    )?;

    while parser.is_exists() {
        let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::Pow) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Pow)
        } else {
            break;
        };

        let right = parse_binary_expression_bit_shift(true, id_alloc, parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
            id: id_alloc.allocate(),
            kind: ASTExpressionKind::Binary(ASTBinaryExpression {
                left: Box::new(left),
                operator,
                operator_kind,
                right: Box::new(right),
                span,
            }),
            span,
        }
    }

    Ok(left)
}

fn parse_binary_expression_bit_shift(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_bit_or_and_xor(
        allow_struct_literal,
        id_alloc,
        parser,
        file,
        diagnostics,
    )?;

    while parser.is_exists() {
        let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::Shl) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Shl)
        } else if let Some(id) = parser.first().kind(TokenKind::Shr) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Shr)
        } else {
            break;
        };

        let right =
            parse_binary_expression_bit_or_and_xor(true, id_alloc, parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
            id: id_alloc.allocate(),
            kind: ASTExpressionKind::Binary(ASTBinaryExpression {
                left: Box::new(left),
                operator,
                operator_kind,
                right: Box::new(right),
                span,
            }),
            span,
        }
    }

    Ok(left)
}

fn parse_binary_expression_bit_or_and_xor(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left =
        parse_unary_expression(allow_struct_literal, id_alloc, parser, file, diagnostics)?;

    while parser.is_exists() {
        let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::BitOr) {
            parser.consume();
            (id, ASTBinaryOperatorKind::BitOr)
        } else if let Some(id) = parser.first().kind(TokenKind::BitAnd) {
            parser.consume();
            (id, ASTBinaryOperatorKind::BitAnd)
        } else if let Some(id) = parser.first().kind(TokenKind::BitXor) {
            parser.consume();
            (id, ASTBinaryOperatorKind::BitXor)
        } else {
            break;
        };

        let right = parse_unary_expression(true, id_alloc, parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
            id: id_alloc.allocate(),
            kind: ASTExpressionKind::Binary(ASTBinaryExpression {
                left: Box::new(left),
                operator,
                operator_kind,
                right: Box::new(right),
                span,
            }),
            span,
        }
    }

    Ok(left)
}

fn parse_unary_expression(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::Add) {
        parser.consume();
        (id, ASTUnaryOperatorKind::Plus)
    } else if let Some(id) = parser.first().kind(TokenKind::Sub) {
        parser.consume();
        (id, ASTUnaryOperatorKind::Minus)
    } else if let Some(id) = parser.first().kind(TokenKind::BitNot) {
        parser.consume();
        (id, ASTUnaryOperatorKind::BitNot)
    } else if let Some(id) = parser.first().kind(TokenKind::LogNot) {
        parser.consume();
        (id, ASTUnaryOperatorKind::LogNot)
    } else {
        return parse_as_expression(allow_struct_literal, id_alloc, parser, file, diagnostics);
    };

    let right = parse_unary_expression(true, id_alloc, parser, file, diagnostics)?;

    let span = operator.span.to(right.span);

    Ok(ASTExpression {
        id: id_alloc.allocate(),
        kind: ASTExpressionKind::Unary(ASTUnaryExpression {
            operator,
            operator_kind,
            right: Box::new(right),
            span,
        }),
        span,
    })
}

fn parse_as_expression(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_call_expression_or_member_access(
        allow_struct_literal,
        id_alloc,
        parser,
        file,
        diagnostics,
    )?;

    while parser.is_exists() {
        let keyword_as = if let Some(id) = parser.first().keyword(*crate::KEYWORD_AS) {
            parser.consume();
            id
        } else {
            break;
        };

        let typename = parse_typename(id_alloc, parser, file, diagnostics)?;

        let span = left.span.to(typename.span);

        left = ASTExpression {
            id: id_alloc.allocate(),
            kind: ASTExpressionKind::As(ASTAsExpression {
                expression: Box::new(left),
                keyword_as,
                typename,
                span,
            }),
            span,
        }
    }

    Ok(left)
}

fn parse_call_expression_or_member_access(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left =
        parse_single_expression(allow_struct_literal, id_alloc, parser, file, diagnostics)?;

    while parser.is_exists() {
        if parser.first().is_kind(TokenKind::Dot) && parser.second().is_id() {
            let dot = if let Some(id) = parser.first().kind(TokenKind::Dot) {
                parser.consume();
                id
            } else {
                unexpected_token(parser.first(), &[*crate::DOT], file, diagnostics);
                return Err(());
            };

            let member = if let Some(id) = parser.first().id() {
                parser.consume();
                id
            } else {
                unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
                return Err(());
            };

            let span = dot.span.to(member.span);

            left = ASTExpression {
                id: id_alloc.allocate(),
                kind: ASTExpressionKind::Member(ASTMemberExpression {
                    expression: Box::new(left),
                    dot,
                    member,
                    span,
                }),
                span,
            };
        } else if parser.first().is_kind(TokenKind::OpenParen) {
            let paren_open = if let Some(id) = parser.first().kind(TokenKind::OpenParen) {
                parser.consume();
                id
            } else {
                break;
            };

            let arguments = parse_call_arguments(id_alloc, parser, file, diagnostics)?;

            let paren_close = if let Some(id) = parser.first().kind(TokenKind::CloseParen) {
                parser.consume();
                id
            } else {
                unexpected_token(parser.first(), &[*crate::CLOSE_PAREN], file, diagnostics);
                return Err(());
            };

            let span = left.span.to(paren_close.span);

            left = ASTExpression {
                id: id_alloc.allocate(),
                kind: ASTExpressionKind::Call(ASTCallExpression {
                    expression: Box::new(left),
                    paren_open,
                    arguments,
                    paren_close,
                    span,
                }),
                span,
            };
        } else {
            break;
        }
    }

    Ok(left)
}

fn parse_call_arguments(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<Vec<ASTArgumentExpression>, ()> {
    let mut arguments = Vec::new();

    while parser.is_exists() && !parser.first().is_kind(TokenKind::CloseParen) {
        let expression = parse_expression(true, id_alloc, parser, file, diagnostics)?;

        let comma = if let Some(id) = parser.first().kind(TokenKind::Comma) {
            parser.consume();
            Some(id)
        } else {
            None
        };

        let span = expression
            .span
            .to(comma.map(|c| c.span).unwrap_or(expression.span));

        arguments.push(ASTArgumentExpression {
            expression,
            comma,
            span,
        });
    }

    Ok(arguments)
}

fn parse_single_expression(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    if parser.first().is_kind(TokenKind::OpenParen) {
        let paren = parse_paren_expression(id_alloc, parser, file, diagnostics)?;
        Ok(ASTExpression {
            id: id_alloc.allocate(),
            span: paren.span,
            kind: ASTExpressionKind::Paren(paren),
        })
    } else if let Some(literal) = parser.first().literal() {
        parser.consume();
        Ok(ASTExpression {
            id: id_alloc.allocate(),
            span: literal.span,
            kind: ASTExpressionKind::Literal(literal),
        })
    } else if parser.first().is_id() {
        parse_id_reference_or_struct_literal(
            allow_struct_literal,
            id_alloc,
            parser,
            file,
            diagnostics,
        )
    } else {
        unexpected_token(
            parser.first(),
            &[*crate::OPEN_PAREN, *crate::LITERAL, *crate::ID],
            file,
            diagnostics,
        );
        Err(())
    }
}

fn parse_paren_expression(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTParenExpression, ()> {
    let paren_open = if let Some(id) = parser.first().kind(TokenKind::OpenParen) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::OPEN_PAREN], file, diagnostics);
        return Err(());
    };

    let expression = parse_expression(true, id_alloc, parser, file, diagnostics)?;

    let paren_close = if let Some(id) = parser.first().kind(TokenKind::CloseParen) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::CLOSE_PAREN], file, diagnostics);
        return Err(());
    };

    let span = paren_open.span.to(paren_close.span);

    Ok(ASTParenExpression {
        paren_open,
        expression: Box::new(expression),
        paren_close,
        span,
    })
}

fn parse_id_reference_or_struct_literal(
    allow_struct_literal: bool,
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    if allow_struct_literal
        && parser.first().is_id()
        && parser.second().is_kind(TokenKind::OpenBrace)
    {
        let struct_literal = parse_struct_literal(id_alloc, parser, file, diagnostics)?;
        return Ok(ASTExpression {
            id: id_alloc.allocate(),
            span: struct_literal.span,
            kind: ASTExpressionKind::StructLiteral(struct_literal),
        });
    }

    let id = if let Some(id) = parser.first().id() {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
        return Err(());
    };

    return Ok(ASTExpression {
        id: id_alloc.allocate(),
        span: id.span,
        kind: ASTExpressionKind::IdReference(ASTIdReference {
            id: id_alloc.allocate(),
            reference: id,
            span: id.span,
        }),
    });
}

fn parse_struct_literal(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTStructLiteral, ()> {
    let typename = if let Some(id) = parser.first().id() {
        parser.consume();
        Typename {
            span: id.span,
            id: id_alloc.allocate(),
            kind: TypenameKind::Id(id),
        }
    } else {
        unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
        return Err(());
    };

    let brace_open = if let Some(id) = parser.first().kind(TokenKind::OpenBrace) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::OPEN_BRACE], file, diagnostics);
        return Err(());
    };

    let fields = parse_struct_literal_fields(id_alloc, parser, file, diagnostics)?;

    let brace_close = if let Some(id) = parser.first().kind(TokenKind::CloseBrace) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::CLOSE_BRACE], file, diagnostics);
        return Err(());
    };

    let span = typename.span.to(brace_close.span);

    Ok(ASTStructLiteral {
        typename,
        brace_open,
        fields,
        brace_close,
        span,
    })
}

fn parse_struct_literal_fields(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<Vec<ASTStructLiteralField>, ()> {
    let mut fields = Vec::new();

    while parser.is_exists() && !parser.first().is_kind(TokenKind::CloseBrace) {
        let name = if let Some(id) = parser.first().id() {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
            return Err(());
        };

        let colon = if let Some(id) = parser.first().kind(TokenKind::Colon) {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::COLON], file, diagnostics);
            return Err(());
        };

        let expression = parse_expression(true, id_alloc, parser, file, diagnostics)?;

        let comma = if let Some(id) = parser.first().kind(TokenKind::Comma) {
            parser.consume();
            Some(id)
        } else {
            None
        };

        let span = name
            .span
            .to(comma.map(|c| c.span).unwrap_or_else(|| expression.span));

        fields.push(ASTStructLiteralField {
            name,
            colon,
            expression,
            comma,
            span,
        });
    }

    Ok(fields)
}

fn parse_typename(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<Typename, ()> {
    if parser.first().is_keyword(*crate::KEYWORD_FN) {
        let function = parse_typename_function(id_alloc, parser, file, diagnostics)?;
        Ok(Typename {
            span: function.span,
            id: id_alloc.allocate(),
            kind: TypenameKind::Function(function),
        })
    } else if let Some(id) = parser.first().id() {
        parser.consume();
        Ok(Typename {
            span: id.span,
            id: id_alloc.allocate(),
            kind: TypenameKind::Id(id),
        })
    } else {
        unexpected_token(
            parser.first(),
            &[*crate::KEYWORD_FN, *crate::ID],
            file,
            diagnostics,
        );
        return Err(());
    }
}

fn parse_typename_function(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<TypenameFunction, ()> {
    let keyword_fn = if let Some(id) = parser.first().keyword(*crate::KEYWORD_FN) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_FN], file, diagnostics);
        return Err(());
    };

    let paren_open = if let Some(id) = parser.first().kind(TokenKind::OpenParen) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::OPEN_PAREN], file, diagnostics);
        return Err(());
    };

    let parameters = parse_typename_function_parameters(id_alloc, parser, file, diagnostics)?;

    let paren_close = if let Some(id) = parser.first().kind(TokenKind::CloseParen) {
        parser.consume();
        id
    } else {
        unexpected_token(
            parser.first(),
            &[*crate::COMMA, *crate::CLOSE_PAREN],
            file,
            diagnostics,
        );
        return Err(());
    };

    let return_type = if parser.first().is_kind(TokenKind::Arrow) {
        Some(parse_typename_function_return_type(
            id_alloc,
            parser,
            file,
            diagnostics,
        )?)
    } else {
        None
    };

    let span = keyword_fn.span.to(return_type
        .as_ref()
        .map_or_else(|| paren_close.span, |return_type| return_type.span));

    Ok(TypenameFunction {
        keyword_fn,
        paren_open,
        parameters,
        paren_close,
        return_type,
        span,
    })
}

fn parse_typename_function_parameters(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<Vec<TypenameFunctionParameter>, ()> {
    let mut parameters = Vec::new();

    while parser.is_exists() && !parser.first().is_kind(TokenKind::CloseParen) {
        let typename = parse_typename(id_alloc, parser, file, diagnostics)?;

        let comma = parser.first().kind(TokenKind::Comma);
        if comma.is_some() {
            parser.consume();
        }

        let span = typename
            .span
            .to(comma.map_or_else(|| typename.span, |comma| comma.span));

        parameters.push(TypenameFunctionParameter {
            typename: Box::new(typename),
            comma,
            span,
        })
    }

    Ok(parameters)
}

fn parse_typename_function_return_type(
    id_alloc: &mut NodeIdAllocator,
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<TypenameFunctionReturnType, ()> {
    let arrow = if let Some(id) = parser.first().kind(TokenKind::Arrow) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::ARROW], file, diagnostics);
        return Err(());
    };

    let typename = parse_typename(id_alloc, parser, file, diagnostics)?;

    let span = arrow.span.to(typename.span);

    Ok(TypenameFunctionReturnType {
        arrow,
        typename: Box::new(typename),
        span,
    })
}

fn unexpected_token(
    lookup: &Lookup,
    expected_symbols: &[Symbol],
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) {
    let (message, span) = if let Some(token) = lookup.token() {
        (
            format!("unexpected token '{}'", token.kind.into_symbol().to_str()),
            token.span,
        )
    } else {
        (format!("unexpected end of file"), file.span_end())
    };

    diagnostics
        .send(Diagnostics {
            level: DiagnosticsLevel::Error,
            message,
            origin: Some(DiagnosticsOrigin {
                file: file.clone(),
                span,
            }),
            sub_diagnostics: vec![SubDiagnostics {
                level: DiagnosticsLevel::Hint,
                message: format!(
                    "expected one of: {}",
                    expected_symbols
                        .iter()
                        .map(|symbol| symbol.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                origin: None,
            }],
        })
        .unwrap();
}
