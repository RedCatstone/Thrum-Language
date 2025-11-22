use thrum_engine::{parsing::ast_structure::Value, run_code};

#[test]
fn math() {
    let result = run_code("
        1 + 2 * 3 +9/ 3
    ");
    assert_eq!(result, Ok(Value::Num(10.0)));
}

#[test]
fn test_while() {
    let result = run_code(r#"
        let x = 0
        while x < 5 {
            x += 1
        }
        x
    "#);
    assert_eq!(result, Ok(Value::Num(5.0)));
}

#[test]
fn test_recursion() {
    let result = run_code(r#"
        fn fib(n) -> num {
            if n < 2 { return n }
            return fib(n-1) + fib(n-2)
        }
        fib(10)
    "#);
    assert_eq!(result, Ok(Value::Num(55.0)));
}



#[test]
fn test_match() {
    let result = run_code(r#"
        match (69, "yay!") {
            (69, "") -> "nope"
            (0, "yay!") -> "nope";  // optional semicolon
            (69, x) -> x
            (0, "") -> "nope"
        }
    "#);
    assert_eq!(result, Ok(Value::Str("yay!".to_string())));
}