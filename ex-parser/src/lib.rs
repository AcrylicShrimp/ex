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
    let token_iter = token_iter(&file);
    let mut parser = Parser::new(token_iter, file.clone(), diagnostics.clone());
    parse_ast_program(&mut parser, &file, &diagnostics)
}

fn parse_ast_program(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> ASTProgram {
    let mut top_levels = Vec::new();

    while parser.is_exists() {
        if let Ok(top_level) = parse_ast_toplevel(parser, file, diagnostics) {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTTopLevel, ()> {
    if parser.first().is_keyword(*crate::KEYWORD_FN) {
        let function = parse_function(parser, file, diagnostics)?;
        Ok(ASTTopLevel {
            span: function.span,
            kind: ASTTopLevelKind::Function(function),
        })
    } else {
        unexpected_token(parser.first(), &[*crate::KEYWORD_FN], file, diagnostics);

        // Skip to next top level.
        while parser.is_exists() && !parser.first().is_keyword(*crate::KEYWORD_FN) {
            parser.consume();
        }

        Err(())
    }
}

fn parse_function(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTFunction, ()> {
    let signature = parse_function_signature(parser, file, diagnostics)?;

    let body_block = parse_block(parser, file, diagnostics)?;

    let span = signature.span.to(body_block.span);

    Ok(ASTFunction {
        signature,
        body_block,
        span,
    })
}

fn parse_function_signature(
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

    let parameters = parse_function_parameters(parser, file, diagnostics)?;

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

    let return_type = if parser.first().is_kind(TokenKind::Colon) {
        Some(parse_function_return_type(parser, file, diagnostics)?)
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

        let parameter_typename = if let Some(id) = parser.first().id() {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
            return Err(());
        };

        let comma = parser.first().kind(TokenKind::Comma);
        if comma.is_some() {
            parser.consume();
        }

        let span = parameter_name
            .span
            .to(comma.map_or_else(|| parameter_typename.span, |comma| comma.span));

        parameters.push(ASTFunctionParameter {
            name: parameter_name,
            colon,
            typename: parameter_typename,
            comma,
            span,
        })
    }

    Ok(parameters)
}

fn parse_function_return_type(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTFunctionReturnType, ()> {
    let colon = if let Some(id) = parser.first().kind(TokenKind::Colon) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::COLON], file, diagnostics);
        return Err(());
    };

    let typename = if let Some(id) = parser.first().id() {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
        return Err(());
    };

    let span = colon.span.to(typename.span);

    Ok(ASTFunctionReturnType {
        colon,
        typename,
        span,
    })
}

fn parse_statement(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTStatement, ()> {
    if parser.first().is_kind(TokenKind::OpenBrace) {
        let block_statement = parse_block(parser, file, diagnostics)?;
        Ok(ASTStatement {
            span: block_statement.span,
            kind: ASTStatementKind::Block(block_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_LET) {
        let let_statement = parse_let(parser, file, diagnostics)?;
        Ok(ASTStatement {
            span: let_statement.span,
            kind: ASTStatementKind::Let(let_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_IF) {
        let if_statement = parse_if(parser, file, diagnostics)?;
        Ok(ASTStatement {
            span: if_statement.span,
            kind: ASTStatementKind::If(if_statement),
        })
    } else if parser.first().is_keyword(*crate::KEYWORD_RETURN) {
        let return_statement = parse_return(parser, file, diagnostics)?;
        Ok(ASTStatement {
            span: return_statement.span,
            kind: ASTStatementKind::Return(return_statement),
        })
    } else {
        // TODO: We need to report error that contains above context, in case that the statement is not a row statement.
        parse_assignment_or_row(parser, file, diagnostics)
    }
}

fn parse_block(
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
        let statement = parse_statement(parser, file, diagnostics)?;
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
        Some(parse_let_type(parser, file, diagnostics)?)
    } else {
        None
    };

    let let_assignment = if parser.first().is_kind(TokenKind::Assign) {
        Some(parse_let_assignment(parser, file, diagnostics)?)
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

    let typename = if let Some(id) = parser.first().id() {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::ID], file, diagnostics);
        return Err(());
    };

    let span = colon.span.to(typename.span);

    Ok(ASTLetType {
        colon,
        typename,
        span,
    })
}

fn parse_let_assignment(
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

    let expression = parse_expression(parser, file, diagnostics)?;

    let span = assignment.span.to(expression.span);

    Ok(ASTLetAssignment {
        assignment,
        expression,
        span,
    })
}

fn parse_if(
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

    let condition = parse_expression(parser, file, diagnostics)?;

    let body_block = parse_block(parser, file, diagnostics)?;

    let single_else_ifs = parse_single_else_ifs(parser, file, diagnostics)?;

    let single_else = if parser.first().is_keyword(*crate::KEYWORD_ELSE) {
        Some(parse_single_else(parser, file, diagnostics)?)
    } else {
        None
    };

    let span = keyword_if.span.to(single_else
        .as_ref()
        .map(|single_else| single_else.span)
        .unwrap_or(body_block.span));

    Ok(ASTIf {
        keyword_if,
        condition,
        body_block,
        single_else_ifs,
        single_else,
        span,
    })
}

fn parse_single_else_ifs(
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

        let condition = parse_expression(parser, file, diagnostics)?;

        let body_block = parse_block(parser, file, diagnostics)?;

        let span = keyword_else.span.to(body_block.span);

        single_else_ifs.push(ASTSingleElseIf {
            keyword_else,
            keyword_if,
            condition,
            body_block,
            span,
        });
    }

    Ok(single_else_ifs)
}

fn parse_single_else(
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

    let body_block = parse_block(parser, file, diagnostics)?;

    let span = keyword_else.span.to(body_block.span);

    Ok(ASTSingleElse {
        keyword_else,
        body_block,
        span,
    })
}

fn parse_return(
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
        Some(parse_expression(parser, file, diagnostics)?)
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTStatement, ()> {
    let left = parse_expression(parser, file, diagnostics)?;

    let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::Assign) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::Assign)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignAdd) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::Add)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignSub) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::Sub)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignMul) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::Mul)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignDiv) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::Div)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignMod) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::Mod)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignPow) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::Pow)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignBitOr) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::BitOr)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignBitAnd) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::BitAnd)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignBitXor) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::BitXor)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignShl) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::Shl)
    } else if let Some(id) = parser.first().kind(TokenKind::AssignShr) {
        parser.consume();
        (id, ASTAssignmentOperatorKind::Shr)
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
            kind: ASTStatementKind::Row(ASTRow {
                expression: left,
                semicolon,
                span,
            }),
            span,
        });
    };

    let right = parse_expression(parser, file, diagnostics)?;

    let semicolon = if let Some(id) = parser.first().kind(TokenKind::Semicolon) {
        parser.consume();
        id
    } else {
        unexpected_token(parser.first(), &[*crate::SEMICOLON], file, diagnostics);
        return Err(());
    };

    let span = left.span.to(semicolon.span);

    Ok(ASTStatement {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    parse_binary_expression_compare(parser, file, diagnostics)
}

fn parse_binary_expression_compare(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_logical_or_and(parser, file, diagnostics)?;

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

        let right = parse_binary_expression_logical_or_and(parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_arithmetic_add_sub(parser, file, diagnostics)?;

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

        let right = parse_binary_expression_arithmetic_add_sub(parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_arithmetic_mul_div_mod(parser, file, diagnostics)?;

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

        let right = parse_binary_expression_arithmetic_mul_div_mod(parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_arithmetic_pow(parser, file, diagnostics)?;

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

        let right = parse_binary_expression_arithmetic_pow(parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_bit_shift(parser, file, diagnostics)?;

    while parser.is_exists() {
        let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::Pow) {
            parser.consume();
            (id, ASTBinaryOperatorKind::Pow)
        } else {
            break;
        };

        let right = parse_binary_expression_bit_shift(parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_binary_expression_bit_or_and_xor(parser, file, diagnostics)?;

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

        let right = parse_binary_expression_bit_or_and_xor(parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_unary_expression(parser, file, diagnostics)?;

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

        let right = parse_unary_expression(parser, file, diagnostics)?;

        let span = left.span.to(right.span);

        left = ASTExpression {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let (operator, operator_kind) = if let Some(id) = parser.first().kind(TokenKind::BitNot) {
        parser.consume();
        (id, ASTUnaryOperatorKind::BitNot)
    } else if let Some(id) = parser.first().kind(TokenKind::LogNot) {
        parser.consume();
        (id, ASTUnaryOperatorKind::LogNot)
    } else {
        return parse_as_expression(parser, file, diagnostics);
    };

    let right = parse_unary_expression(parser, file, diagnostics)?;

    let span = operator.span.to(right.span);

    Ok(ASTExpression {
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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_call_expression(parser, file, diagnostics)?;

    while parser.is_exists() {
        let keyword_as = if let Some(id) = parser.first().keyword(*crate::KEYWORD_AS) {
            parser.consume();
            id
        } else {
            break;
        };

        let typename = if let Some(id) = parser.first().id() {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::KEYWORD_AS], file, diagnostics);
            return Err(());
        };

        let span = left.span.to(typename.span);

        left = ASTExpression {
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

fn parse_call_expression(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    let mut left = parse_single_expression(parser, file, diagnostics)?;

    while parser.is_exists() {
        let paren_open = if let Some(id) = parser.first().kind(TokenKind::OpenParen) {
            parser.consume();
            id
        } else {
            break;
        };

        let arguments = parse_call_arguments(parser, file, diagnostics)?;

        let paren_close = if let Some(id) = parser.first().kind(TokenKind::CloseParen) {
            parser.consume();
            id
        } else {
            unexpected_token(parser.first(), &[*crate::CLOSE_PAREN], file, diagnostics);
            return Err(());
        };

        let span = left.span.to(paren_close.span);

        left = ASTExpression {
            kind: ASTExpressionKind::Call(ASTCallExpression {
                expression: Box::new(left),
                paren_open,
                arguments,
                paren_close,
                span,
            }),
            span,
        };
    }

    Ok(left)
}

fn parse_call_arguments(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<Vec<ASTArgumentExpression>, ()> {
    let mut arguments = Vec::new();

    while parser.is_exists() && !parser.first().is_kind(TokenKind::CloseParen) {
        let expression = parse_expression(parser, file, diagnostics)?;

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
    parser: &mut Parser<impl Iterator<Item = Token>>,
    file: &Arc<SourceFile>,
    diagnostics: &Sender<Diagnostics>,
) -> Result<ASTExpression, ()> {
    if parser.first().is_kind(TokenKind::OpenParen) {
        let paren = parse_paren_expression(parser, file, diagnostics)?;
        Ok(ASTExpression {
            span: paren.span,
            kind: ASTExpressionKind::Paren(paren),
        })
    } else if let Some(literal) = parser.first().literal() {
        parser.consume();
        Ok(ASTExpression {
            span: literal.span,
            kind: ASTExpressionKind::Literal(literal),
        })
    } else if let Some(id) = parser.first().id() {
        parser.consume();
        Ok(ASTExpression {
            span: id.span,
            kind: ASTExpressionKind::Id(id),
        })
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

    let expression = parse_expression(parser, file, diagnostics)?;

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
