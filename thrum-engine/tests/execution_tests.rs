use thrum_engine::vm_compiling::RuntimeValue;
mod common;



#[test]
fn math() {
    test_eval!("1+2 * 3", RuntimeValue::Num(7.0));
    test_eval!("(1 + 2)*3", RuntimeValue::Num(9.0));
    test_eval!("1 + 2 * 3 +9/ 3", RuntimeValue::Num(10.0));
    test_eval!("6--21 * 3", RuntimeValue::Num(69.0));
    test_eval!("3 % 2", RuntimeValue::Num(1.0));
}


#[test]
fn scoping() {
    test_eval!(r#"
        let x = 50
        x^
    "#, RuntimeValue::Num(50.0));

    test_eval!(r#"
        let x = 12
        let res = {
            let x = 33  // shadowing!
            x + 5
        }
        res + x
    "#, RuntimeValue::Num(50.0));

    test_eval!(r#"
        { #bloc
            break #bloc 50
            1
        }
    "#, RuntimeValue::Num(50.0));
}

#[test]
fn comments() {
    test_eval!(r#"
        15 /* this is a comment
        + 4
        */
        + /* weee * / */ 2
        // + 16
        + 3
    "#, RuntimeValue::Num(20.0));
}

#[test]
fn loop_shenanigance() {
    test_eval!(r#"
        let mut i = 0
        while i < 5 {
            i += 1
        }
        i^
    "#, RuntimeValue::Num(5.0));

    test_eval!(r#"
        let mut sum = 0
        let mut i = 0
        loop {
            i += 1
            if i > 5 { break }
            if i == 3 { continue }
            sum += i^
        }
        sum^  // 1 + 2 + 4 + 5 = 12
    "#, RuntimeValue::Num(12.0));

    test_eval!(r#"
        let mut res = 1
        loop #outer {
            res += 1
            loop {
                res += 22
                if res < 30 { continue #outer }
                if res < 50 { continue } // inner
                break #outer res^
            }
            break -1
        }
    "#, RuntimeValue::Num(69.0));
    
    test_eval!("loop { break #loop }", RuntimeValue::Void);
}

#[test]
fn diabolical_loops() {
    test_eval!("1 + loop { 1 + break 1 }", RuntimeValue::Num(2.0));

    test_eval!(r#"
        let mut i = 1

        1 + loop {
            i += 1
            if i < 3 {
                (1, 1, 1, continue)
            }
            (1, 1, 1, break 1)
        }
    "#, RuntimeValue::Num(2.0));
}


#[test]
fn tup_destructuring() {
    test_eval!(r#"
        let tup = ((1, 2), 2, (3, 4, 5))
        let (_, _, (_, x, _)) = tup^
        x^
    "#, RuntimeValue::Num(4.0));

    test_eval!(r#"
        let status = "ok"
        let data = (id: 42, status:)

        if data^ is (status: "ok", id:) {
            id^
        }
        else { -1 }
    "#, RuntimeValue::Num(42.0));
}

#[test]
fn tup_arrays() {
    test_eval!(r#"
        let x = (1, 2, 3)
        x[0] + x[1] * x[2]
    "#, RuntimeValue::Num(7.0));

    test_eval!(r#"
        let mut x = (7; 30)
        x[2] %= 4
        x[2]^
    "#, RuntimeValue::Num(3.0));
}

#[test]
fn some_strings() {
    test_eval!(r#"
        let s = "klaus"
        let arr = (s, "x")
        arr[0]^
    "#, RuntimeValue::Str("klaus".to_string()));

    test_eval!(r#"
        let piece = "orl"
        "hello w{piece^}d!"
    "#, RuntimeValue::Str("hello world!".to_string()));
}

#[test]
fn delayed_let() {
    test_eval!(r#"
        { #bloc
            let x
            if true { x = 5 }
            else break #bloc -1
            x^
        }
    "#, RuntimeValue::Num(5.0));

    test_eval!("let a;         (a, let b) = (1, 2); a + b", RuntimeValue::Num(3.0));
    test_eval!("let mut a = 5; (a, let b) = (1, 2); a + b", RuntimeValue::Num(3.0));
}



#[test]
fn short_circuiting() {
    test_eval!(r#"
        true or panic("OR short-circuit failed...")
    "#, RuntimeValue::Bool(true));
    test_eval!(r#"
        false and panic("AND short-circuit failed...")
    "#, RuntimeValue::Bool(false));

    test_eval!(r#"
        let t1 = (10, 20)
        let t2 = (10, 20)

        let res1 = t1^ is (10, _) or panic("OR short-circuit failed...")
        let res2 = t2^ is (99, _) and panic("AND short-circuit failed...")

        res1 and !res2
    "#, RuntimeValue::Bool(true));
}


#[test]
fn match_exprs() {
    test_eval!(r#"
        match (69, "yay!")
        is (69, "") => "nope"
        is (0, "yay!") => "nope"
        is (0, "") => "nope" 
        is (69, x) => x^
        is _ => "nope"
    "#, RuntimeValue::Str("yay!".to_string()));
}




#[test]
fn recursion() {
    test_eval!(r#"
        fn fib(n: num) -> num {
            if n < 2 { return n }
            return fib(n-1) + fib(n-2)
        }
        fib(8)
    "#, RuntimeValue::Num(21.0));
}



#[test]
fn functions() {
    test_eval!(r#"
        fn fun() -> num {
            4
        }
        fun()
    "#, RuntimeValue::Num(4.0));

    test_eval!(r#"
        fn fun(x: num, y: num) -> num {
            x * y + x
        }
        fun(2, 4)
    "#, RuntimeValue::Num(10.0));


    test_eval!(r#"
        fn test(tup: (num, num), expected_bool: bool) {
            let maybe = { tup^ is let (x, 0) and x > 10 }
            if maybe != expected_bool {
                panic("nope.")
            }
        }

        test((30, 1), false)
        test((3, 0), false)
        test((30, 0), true)
    "#, RuntimeValue::Void);
}


#[test]
fn impls() {
    test_eval!(r#"
        type N = num
        impl N {
            fn get() -> N => N{ 5 }
        }
        fn get() -> N => N{ 3 }

        (N.get(), get())
    "#, RuntimeValue::Tup(vec![RuntimeValue::Num(5.0), RuntimeValue::Num(3.0)]));

    test_eval!(r#"
        type N = num
        impl N {
            fn square(self: Self) -> Self {
                self * self
            }
            fn triangle(self: Self) -> Self {
                (self * (self + N{ 1 })) / N{ 2 }
            }
        }
        
        N{ 2 }.triangle().square().triangle().square()
    "#, RuntimeValue::Num(2025.0))
}

#[test]
fn single_tuple_type_instantiation() {
    test_eval!("type X = num;         X{ 2 }", RuntimeValue::Num(2.0));
    test_eval!("type X = (num,);      X{ 2 }", RuntimeValue::Tup(vec![RuntimeValue::Num(2.0)]));
    test_eval!("type X = (num, bool); X{ 2, true }", RuntimeValue::Tup(vec![RuntimeValue::Num(2.0), RuntimeValue::Bool(true)]));
    test_eval!("type X = ();          X{ }", RuntimeValue::Tup(vec![]));
}

#[test]
fn single_tuple_type_destruction() {
    test_eval!("type X = num;              let X{ a } = X{ 2 };                 a^",     RuntimeValue::Num(2.0));
    test_eval!("type X = (num,);           let X{ a } = X{ 2 };                 a^",     RuntimeValue::Num(2.0));
    test_eval!("type X = (num, num);       let X{ a, b } = X{ 3, 4 };           a * b", RuntimeValue::Num(12.0));
    test_eval!("type X = (x: num, y: num); let X{ x: a, y: } = X{ x: 3, y: 4 }; a * y", RuntimeValue::Num(12.0));
    test_eval!("type X = ();               let X{ } = X{ }",                            RuntimeValue::Void);
}


#[test]
fn point_impl_test() {
    test_eval!("
        type Point = (x: num, y: num)
        impl Point {
            fn squared_distance(self: &Self) -> num {
                self.x * self.x + self.y * self.y
            }
        }

        let mut p = Point{ x: 3, y: 2 }
        p.y = 4
        p.squared_distance()
    ", RuntimeValue::Num(25.0));
}

#[test]
fn enums() {
    test_eval!("
        const Dir = enum { Up, Down(bool, bool) }

        let down: Dir = .Down(true, false)
        let down2: Dir = .Down(true, false)
        let up: Dir = .Up
        let up2: Dir = .Up

        up^ is .Up
        and down^ is .Down(_, false)
        and up2^ is !.Down(_, _)
        and down2^ is !.Up
    ", RuntimeValue::Bool(true));
}


#[test]
fn random_examples() {
    test_eval!("
        fn sum_to(max: num) -> num {
            let mut sum = 0
            let mut i = 0
            while i < max {
                i += 1
                sum += i^
            }
            sum
        }
        sum_to(100)
    ", RuntimeValue::Num(5050.0));
}