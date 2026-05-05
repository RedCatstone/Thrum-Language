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
    test_err!("let = 5", ErrType::ParserExpectedABindingPattern { .. });
    test!("{ #label }", RuntimeValue::Void);
    test_err!("{ \n #label }", ErrType::ParserLabelsHaveToBeOnSameLine);
    test_err!("if true => \n 5", ErrType::ParserArrowExprsHaveToBeOnSameLine);
}


#[test]
fn typecheck_var_errors() {
    test_err!("x = 10", ErrType::TyperUndefinedIdentifier { .. });
    test_err!("let x = 10; x = 20", ErrType::TyperVarIsntDeclaredMut { .. });
    test_err!("let tup = (1, 2); tup[0] = 99", ErrType::TyperVarIsntDeclaredMut { .. });

    test_err!("let x; x + 5", ErrType::TyperCantUseUninitializedVar { .. });
    test_err!("let x; if true { x = 2 }; x^", ErrType::TyperCantUseMaybeInitializedVar { .. });

    test_err!("let s = \"hello\"; s^; s^", ErrType::TyperCantUseMovedVar { .. });
    test_err!("let s = \"blub\"; if true { s^ }; s^", ErrType::TyperCantUseMaybeMovedVar { .. });
}

#[test]
fn type_mismatch_errors() {
    test_err!("if true { 1 } else { (1,) }", ErrType::TyperMismatch { .. });
    test_err!("let x: num = true", ErrType::TyperMismatch { expected: Type::Num, found: Type::Bool });
    test_err!("(1, 2) is (1, 2, 3)", ErrType::TyperMismatch { .. });

    // operators
    test_err!("1 + true", ErrType::TyperMismatch { .. });
    test_err!("1 % true", ErrType::TyperMismatch { .. });
    test_err!("!5", ErrType::TyperMismatch { .. });
    test_err!("-true", ErrType::TyperMismatch { .. });

    // patterns
    test_err!("1 is \"hello\"", ErrType::TyperMismatch { .. });
    test_err!("(1, 2) is (1, \"two\")", ErrType::TyperMismatch { .. });
    test_err!("(1, 2) is 1 | 2", ErrType::TyperMismatch { .. });
    
    test_err!("let mut x = 5; x += true", ErrType::TyperMismatch { .. });
}

#[test]
fn typecheck_misc_errors() {
    test_err!("loop { break #outer }", ErrType::TyperUndefinedLoopLabel { .. });
    test_err!("break", ErrType::TyperBreakOutsideLoop);
    test_err!("return", ErrType::TyperReturnOutsideFunction);
    test_err!("Self", ErrType::TyperSelfOutsideImplBlock);

    test_err!("let x", ErrType::TyperCantInferType { typ: Type::Infer(TypeInferId(0)) });
}

#[test]
fn invalid_op_types() {
    test_err!("let x = 5; x()", ErrType::TyperCantCallNonFnType { .. });
    test_err!("let x = true; x[0]", ErrType::TyperCantIndexNonArrType { .. });

    test_err!("let x = 5; x^^", ErrType::TyperCantDerefNonPointerType { .. });
    test_err!("5^", ErrType::TyperCantDerefNonPointerType { .. });

    test_err!("let x = 5; x.hello", ErrType::TyperTypeDoesntHaveMember { .. });
    test_err!("true.hello", ErrType::TyperTypeDoesntHaveMember { .. });
}

#[test]
fn exhaustive_pattern_matching() {
    test_err!("match 5 is 1 => 2", ErrType::TyperPatternDoesntCoverAllCases { .. });
    // TODO: test_err!("match 5 is _ => 1 \n is 5 => 2", ErrType::TyperPatternCantBeReached);

    test_err!("let a = (5 is let x)", ErrType::TyperInvalidBindingCaseExpr);
    test_err!("if (5 is let x) == true {}", ErrType::TyperInvalidBindingCaseExpr);

    test_err!("5 is !(let x)", ErrType::TyperNotPatternCantBindVars);

    test_err!("if (1, 2) is let (x, y) | (x, _) {}", ErrType::TyperOrPatternDoesntBindVars { .. });
    test_err!("if (1, 2) is let (x, _) | (x, y) {}", ErrType::TyperOrPatternBindsVarsTooMuch { .. });
    
    test_err!("if (1, 2) is let (x, 0) | (0, y) { }", ErrType::TyperOrPatternDoesntBindVars { .. });
    test_err!("let (x, 0) = (1, 2)", ErrType::TyperFailableAssignPattern { .. });
}

#[test]
fn custom_type() {
    test_err!("num{ 5 }", ErrType::TyperMustBeCustomtypeType { .. });
    test_err!("type X = num; X{ 1, 2 }", ErrType::TyperNewTypesExpectOneUnlabeledExpr);
    test_err!("type X = 5", ErrType::TyperMismatch { .. });

    test_err!("let x: num = .Foo", ErrType::TyperExpectedTypeIsntAnEnum { .. });
    test_err!("const E = enum { A }; let x: E = .B", ErrType::TyperEnumDoesntHaveVariant { .. });
}

#[test]
fn const_errs() {
    test_err!("const X = 2 * Y; const Y = 2 * X", ErrType::TyperConstResolvingCycle);
    test_err!("const (X, X) = (1, 2)", ErrType::TyperConstNameAlreadyExists { .. });

    test_err!("const mut X = 5", ErrType::TyperConstCantBeMutable);
    test_err!("type N = num; impl N { let x = 5 }", ErrType::TyperRuntimeValuesArentAllowedInImplBlocks);
}


#[test]
fn array_bounds() {
    test_err!("let x = (1, 2, 3); x[3]^", ErrType::RuntimeError { .. });
    test_err!("let x = (); x[0]^", ErrType::TyperCantIndexEmptyTuple { .. });
    test_err!("let x = (1, true); x[0]^", ErrType::TyperCantIndexHeterogenousTuple { .. });
}


#[test]
fn new_types() {
    test!("type Number = num; Number{ 320 }", RuntimeValue::Num(320.0));
    test_err!("type Number = num; Number{ 320 } == 320", ErrType::TyperMismatch { .. });
    test_err!("type Number = num; let x: Number = 5", ErrType::TyperMismatch { .. });
}


#[test]
fn typecheck_functions() {
    test_err!("fn square(x: num) {}; square(2, 3)", ErrType::TyperWrongNumberOfArguments { .. });
    test_err!("fn square(x: num) {}; square()", ErrType::TyperWrongNumberOfArguments { .. });
    test_err!("fn square(x) {}", ErrType::TyperRequiresTypeAnnotation);
    
    test_err!("fn foo() -> num { return \"string\" }", ErrType::TyperMismatch { .. });
    test_err!("fn foo() -> num { }", ErrType::TyperMismatch { .. });
}


#[test]
fn deref_non_local_pointer() {
    test_err!("type T = (); impl T { fn f(self: &Self) { self^^; } }", ErrType::TyperCantDerefUnknownPointerType);
}