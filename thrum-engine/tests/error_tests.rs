use thrum_engine::{ErrType, lexing::tokens::{AssignOp, TokenKind}, typing::{Type, TypeInferId}, vm_compiling::RuntimeValue};
mod common;



#[test]
fn lexer_errors() {
    test_err!("1 + ~", ErrType::LexerUnexpectedCharacter { c: '~' });
    test_err!("unterminated \"string", ErrType::LexerUnterminatedString);
}

#[test]
fn parser_errors() {
    test_err!("{", ErrType::ParserExpectToken { .. });
    test_err!("fn 1", ErrType::ParserExpectToken { .. });
    test_err!("if", ErrType::ParserExpectedAnExpression { found: TokenKind::EndOfFile });
    test_err!("1 - / 1", ErrType::ParserExpectedAnExpression { found: TokenKind::Op(AssignOp::Slash) });
    test_err!("1 + 1  15", ErrType::ParserUnexpectedExpression);
    test_eval!("{ #label }", RuntimeValue::Void);
    test_err!("{ \n #label }", ErrType::ParserLabelsHaveToBeOnSameLine);
    test_err!("if true => \n 5", ErrType::ParserArrowExprsHaveToBeOnSameLine);
}


#[test]
fn typecheck_var_errors() {
    test_err!("x = 10", ErrType::TyperUndefinedIdentifier { .. });
    test_err!("let x = 10; x = 20", ErrType::TyperVarIsntDeclaredMut { .. });

    test_err!("let x; x + 5", ErrType::TyperCantUseUninitializedVar { .. });
    test_err!("let s = \"hello\"; s^; s^", ErrType::TyperCantUseUninitializedVar { .. });
    test_err!("let s = \"hello\"; s^; s^", ErrType::TyperCantUseUninitializedVar { .. });
    test_err!("let x; if true { x = 2 }; x^", ErrType::TyperCantUseMaybeInitializedVar { .. });
    test_err!("let s = \"blub\"; if true { s^ }; s^", ErrType::TyperCantUseMaybeInitializedVar { .. });
}

#[test]
fn typecheck_mismatch_errors() {
    test_err!("if true { 1 } else { (1,) }", ErrType::TyperMismatch { .. });
    test_err!("let x: num = true", ErrType::TyperMismatch { expected: Type::Num, found: Type::Bool });
}

#[test]
fn typecheck_misc_errors() {
    test_err!("loop { break #outer }", ErrType::TyperUndefinedLoopLabel { .. });

    test_err!("if (1, 2) is let (x, 0) | (0, y) { }", ErrType::TyperOrPatternDoesntBindVars { .. });
    test_err!("let (x, 0) = (1, 2)", ErrType::TyperFailableAssignPattern { .. });

    test_err!("let x", ErrType::TyperCantInferType { typ: Type::Infer(TypeInferId(0)) })
}


#[test]
fn array_out_of_bounds() {
    test_err!("let x = (1, 2, 3); x[3]^", ErrType::RuntimeError { .. });
    test_err!("let x = (); x[0]^", ErrType::TyperCantIndexEmptyTuple { .. });
    test_err!("let x = (1, true); x[0]^", ErrType::TyperCantIndexHeterogenousTuple { .. });
}


#[test]
fn new_types() {
    test_eval!("type Number = num; Number{ 320 }", RuntimeValue::Num(320.0));
    test_err!("type Number = num; Number{ 320 } == 320", ErrType::TyperMismatch { .. });
    test_err!("type Number = num; let x: Number = 5", ErrType::TyperMismatch { .. });
}


#[test]
fn consts() {
    test_eval!("const (x, _) = (5, 3); x^", RuntimeValue::Num(5.0));
    test_eval!("const X = 2 * Y; const Z = 20; const Y = 3 * Z; X^", RuntimeValue::Num(120.0));
    test_err!("const X = 2 * Y; const Y = 2 * X", ErrType::TyperConstResolvingCycle);
}


#[test]
fn typecheck_functions() {
    test_err!("fn square(x: num) {}; square(2, 3)", ErrType::TyperWrongNumberOfArguments { .. });
    test_err!("fn square(x: num) {}; square()", ErrType::TyperWrongNumberOfArguments { .. });
    test_err!("fn square(x) {}", ErrType::TyperRequiresTypeAnnotation);
}