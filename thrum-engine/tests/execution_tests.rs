use thrum_engine::{parsing::ast_structure::Value, run_code};

#[test]
fn math() {
    assert_eq!(
        run_code("
            1 + 2 * 3 +9/ 3
        "),
        Ok(Value::Num(10.0))
    );
}

#[test]
fn test_while() {
    assert_eq!(
        run_code(r#"
        let x = 0
        while x < 5 {
            x += 1
        }
        x
        "#),
        Ok(Value::Num(5.0))
    );
}

#[test]
fn test_recursion() {
    assert_eq!(
        run_code(r#"
        fn fib(n) -> num {
            if n < 2 { return n }
            return fib(n-1) + fib(n-2)
        }
        fib(10)
        "#),
        Ok(Value::Num(55.0))
    );
}



#[test]
fn test_match() {
    assert_eq!(
        run_code(r#"
        match (69, "yay!") {
            (69, "") -> "nope"
            (0, "yay!") -> "nope";  // optional semicolon
            (69, x) -> x
            (0, "") -> "nope"
        }
        "#),
        Ok(Value::Str("yay!".to_string()))
    );
}





#[test]
fn test_case_expr() {
    assert_eq!(
        run_code(r#"
        fn test(tup: tup<num, num>, expected_bool: bool) {
            let maybe = { (case (x, 0) = tup) and x > 10 }
            if maybe != expected_bool {
                panic("nope.")
            }
        }

        test((30, 1), false)
        test((3, 0), false)
        test((30, 0), true)
        "#),
        Ok(Value::Void)
    );
}