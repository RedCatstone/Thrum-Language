#![allow(clippy::needless_raw_string_hashes)]
use thrum_engine::{parsing::ast_structure::Value, run_code};

#[test]
fn test_math() {
    assert_eq!(
        run_code(r#"
            1 + 2 * 3 +9/ 3
        "#),
        Ok(Value::Num(10.0))
    );
}

#[test]
fn test_strings() {
    assert_eq!(
        run_code(r#"
            let piece = "orl"
            "hel" + "lo w{piece^}d!"
        "#),
        Ok(Value::Str("hello world!".to_string()))
    );
}

#[test]
fn test_delayed_let() {
    assert_eq!(
        run_code(r#"
            { #bloc
                let x
                if true {
                    x = 5
                }
                else break #bloc -1
                x^
            }
        "#),
        Ok(Value::Num(5.0))
    );
}

#[test]
fn test_while() {
    assert_eq!(
        run_code(r#"
        let mut x = 0
        while x < 5 {
            x += 1
        }
        x^
        "#),
        Ok(Value::Num(5.0))
    );
}

#[test]
fn test_recursion() {
    assert_eq!(
        run_code(r#"
        fn fib(n: own num) -> num {
            if n < 2 { return n }
            return fib(n-1) + fib(n-2)
        }
        fib(8)
        "#),
        Ok(Value::Num(21.0))
    );
}



#[test]
fn test_match() {
    assert_eq!(
        run_code(r#"
        match (69, "yay!") {
            (69, "") -> "nope"
            (0, "yay!") -> "nope";  // optional semicolon
            (0, "") -> "nope" 
            (69, x) -> x^
            _ -> "nope"
        }
        "#),
        Ok(Value::Str("yay!".to_string()))
    );
}


#[test]
fn test_logic_short_circuit() {
    assert_eq!(
        run_code(r#"
        let t1 = (10, 20)
        let t2 = (10, 20)
                
        let res1 = case (10, _) = t1^ or panic("OR short-circuit failed...")
        let res2 = case (99, _) = t2^ and panic("AND short-circuit failed...")

        res1 and !res2
        "#),
        Ok(Value::Bool(true))
    );
}



#[test]
fn test_case_expr() {
    assert_eq!(
        run_code(r#"
        fn test(tup: own (num, num), expected_bool: own bool) {
            let maybe = { case (x, 0) = tup^ and x > 10 }
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


#[test]
fn test_labeled_loops() {
    assert_eq!(
        run_code(r#"
        let mut res = 1
        loop #outer {
            res += 1
            loop {
                res += 22
                if res < 30 { continue #outer }
                if res < 50 { continue } // inner
                break #outer res^
            }
            // Should never reach this point.
            break -1
        }
        "#),
        Ok(Value::Num(69.0))
    );
}


#[test]
fn test_labeled_blocks() {
    assert_eq!(
        run_code(r#"
        { #bloc
            break #bloc 69420
            5
        }
        "#),
        Ok(Value::Num(69420.0))
    );
}


#[test]
fn test_tuples() {
    assert_eq!(
        run_code(r#"
        let status = "ok"
        let data = (id: 184, status:)

        if case (status: "ok", id:) = data^ {
            id^
        }
        else { -1 }
        "#),
        Ok(Value::Num(184.0))
    );
}