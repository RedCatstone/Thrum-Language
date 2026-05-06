use thrum_engine::vm_compiling::RuntimeValue;
mod common;



#[test]
fn math() {
    test!("1+2 * 3", RuntimeValue::Num(7.0));
    test!("(1 + 2)*3", RuntimeValue::Num(9.0));
    test!("1 + 2 * 3 +9/ 3", RuntimeValue::Num(10.0));
    test!("6--21 * 3", RuntimeValue::Num(69.0));
    test!("3 % 2", RuntimeValue::Num(1.0));
}

#[test]
fn assignments() {
    test!("
        let mut x = 10
        x += 5   // 15
        x *= 2   // 30
        x -= 6   // 24
        x /= 3   // 8
        x %= 5   // 3
        x^
    ", RuntimeValue::Num(3.0));

    test!("
        let mut a = 1
        let mut b = 2
        (a, b) = (b, a)
        a * 10 + b
    ", RuntimeValue::Num(21.0));
}


#[test]
fn scoping() {
    test!("let x = 50; x^", RuntimeValue::Num(50.0));

    test!("
        let x = 12
        let res = { let x = 33; x + 5 }
        res + x
    ", RuntimeValue::Num(50.0));

    test!("
        let x = 1
        let res = {
            let x = 2
            {
                let x = 3
                x * 10
            }
            + x * 100
        }
        res + x
    ", RuntimeValue::Num(231.0)); // 200 + 30 + 1

    test!("
        { #bloc
            break #bloc 50
            1
        }
    ", RuntimeValue::Num(50.0));
    
    test!("{ #label }", RuntimeValue::Void);
    test!("{}", RuntimeValue::Void);
}

#[test]
fn comments() {
    test!("
        15 /* this is a comment
        + 4
        */
        + /* weee * / */ 2
        // + 16
        + 3
    ", RuntimeValue::Num(20.0));
}

#[test]
fn loop_shenanigance() {
    test!("
        let mut i = 0
        while i < 5 {
            i += 1
        }
        i^
    ", RuntimeValue::Num(5.0));

    test!("
        let mut sum = 0
        let mut i = 0
        loop {
            i += 1
            if i > 5 { break }
            if i == 3 { continue }
            sum += i^
        }
        sum^  // 1 + 2 + 4 + 5 = 12
    ", RuntimeValue::Num(12.0));

    test!("
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
    ", RuntimeValue::Num(69.0));
    
    test!("loop { break #loop }", RuntimeValue::Void);
}

#[test]
fn diabolical_loops() {
    test!("1 + loop { 1 + break 1 }", RuntimeValue::Num(2.0));

    test!("
        let mut i = 1

        1 + loop {
            i += 1
            if i < 3 {
                (1, 1, 1, continue)
            }
            (1, 1, 1, break 1)
        }
    ", RuntimeValue::Num(2.0));
}


#[test]
fn tup_destructuring() {
    test!("
        let tup = ((1, 2), 2, (3, 4, 5))
        let (_, _, (_, x, _)) = tup^
        x^
    ", RuntimeValue::Num(4.0));

    test!(r#"
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
    test!("
        let x = (1, 2, 3)
        x[0] + x[1] * x[2]
    ", RuntimeValue::Num(7.0));

    test!("
        let mut x = (7; 30)
        x[2] %= 4
        x[2]^
    ", RuntimeValue::Num(3.0));

    test!("
        let mut grid = ((1, 2), (3, 4))
        grid[1][0] = 99
        grid[0][1] + grid[1][0]
    ", RuntimeValue::Num(101.0)); // 2 + 99
}

#[test]
fn some_strings() {
    test!(r#"
        let s = "klaus"
        let arr = (s, "x")
        arr[0]^
    "#, RuntimeValue::Str("klaus".to_string()));

    test!(r#"
        let piece = "orl"
        "hello w{piece^}d!"
    "#, RuntimeValue::Str("hello world!".to_string()));
}

#[test]
fn delayed_let() {
    test!("
        { #bloc
            let x
            if true { x = 5 }
            else break #bloc -1
            x^
        }
    ", RuntimeValue::Num(5.0));

    test!("let a;         (a, let b) = (1, 2); a + b", RuntimeValue::Num(3.0));
    test!("let mut a = 5; (a, let b) = (1, 2); a + b", RuntimeValue::Num(3.0));
}



#[test]
fn short_circuiting() {
    test!(r#"
        true or panic("OR short-circuit failed...")
    "#, RuntimeValue::Bool(true));
    test!(r#"
        false and panic("AND short-circuit failed...")
    "#, RuntimeValue::Bool(false));

    test!(r#"
        let t1 = (10, 20)
        let t2 = (10, 20)

        let res1 = t1^ is (10, _) or panic("OR short-circuit failed...")
        let res2 = t2^ is (99, _) and panic("AND short-circuit failed...")

        res1 and !res2
    "#, RuntimeValue::Bool(true));
}


#[test]
fn pattern_matching() {
    test!("7 is 7", RuntimeValue::Bool(true));
    test!("7 is 0", RuntimeValue::Bool(false));
    test!("7 is !0", RuntimeValue::Bool(true));
    test!("7 is !7", RuntimeValue::Bool(false));
    test!("7 is 5 | 6 | 7", RuntimeValue::Bool(true));
    test!("5 is 5 | 6 | 7", RuntimeValue::Bool(true));
    test!("0 is 5 | 6 | 7", RuntimeValue::Bool(false));
    test!("0 is !_", RuntimeValue::Bool(false));
    test!("3 + 3 is 3 + 3", RuntimeValue::Bool(true));
    test!("3 + 3 is !3 * 3", RuntimeValue::Bool(true));
    test!("(1, 2) is (1, 1) | (1, 2)", RuntimeValue::Bool(true));

    test!(r#"5 is (!(4 | 5)) and panic("pattern should've failed...")"#, RuntimeValue::Bool(false));
    test!(r#"5 is (!(4 | 6)) or panic("pattern should've failed...")"#, RuntimeValue::Bool(true));
}

#[test]
fn match_exprs() {
    test!(r#"
        match (69, "yay!")
        is (69, "") => "nope"
        is (0, "yay!") => "nope"
        is (0, "") => "nope" 
        is (69, let x) => x^
        is _ => "nope"
    "#, RuntimeValue::Str("yay!".to_string()));
}
#[test]
fn enum_match_expr() {
    test!("
        const Opt = enum { None, Some(num) }
        
        let val: Opt = .Some(42)
        
        match val^
        is .None => -1
        is .Some(0 | 1 | 2) => 0
        is .Some(!42) => 1
        is .Some(let x) => x^
    ", RuntimeValue::Num(42.0));
}




#[test]
fn recursion() {
    test!("
        fn fib(n: num) -> num {
            if n < 2 { return n }
            return fib(n-1) + fib(n-2)
        }
        fib(8)
    ", RuntimeValue::Num(21.0));
}



#[test]
fn functions() {
    test!("
        fn fun() -> num => 4
        fun()
    ", RuntimeValue::Num(4.0));

    test!("
        fn fun(x: num, y: num) -> num {
            x * y + x
        }
        fun(2, 4)
    ", RuntimeValue::Num(10.0));


    test!(r#"
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
    test!("type Number = num; Number{ 320 }", RuntimeValue::Num(320.0));
    
    test!("
        type N = num
        impl N {
            fn get() -> N => N{ 5 }
        }
        fn get() -> N => N{ 3 }

        (N.get(), get())
    ", RuntimeValue::Tup(vec![RuntimeValue::Num(5.0), RuntimeValue::Num(3.0)]));

    test!("
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
    ", RuntimeValue::Num(2025.0));
}

#[test]
fn single_tuple_type_instantiation() {
    test!("type X = num;         X{ 2 }", RuntimeValue::Num(2.0));
    test!("type X = (num,);      X{ 2 }", RuntimeValue::Tup(vec![RuntimeValue::Num(2.0)]));
    test!("type X = (num, bool); X{ 2, true }", RuntimeValue::Tup(vec![RuntimeValue::Num(2.0), RuntimeValue::Bool(true)]));
    test!("type X = ();          X{ }", RuntimeValue::Tup(vec![]));
}

#[test]
fn single_tuple_type_destruction() {
    test!("type X = num;              let X{ a } = X{ 2 };                 a^",     RuntimeValue::Num(2.0));
    test!("type X = (num,);           let X{ a } = X{ 2 };                 a^",     RuntimeValue::Num(2.0));
    test!("type X = (num, num);       let X{ a, b } = X{ 3, 4 };           a * b", RuntimeValue::Num(12.0));
    test!("type X = (x: num, y: num); let X{ x: a, y: } = X{ x: 3, y: 4 }; a * y", RuntimeValue::Num(12.0));
    test!("type X = ();               let X{ } = X{ }",                            RuntimeValue::Void);
}

#[test]
fn consts() {
    test!("const (x, _) = (5, 3); x^", RuntimeValue::Num(5.0));
    test!("const X = 2 * Y; const Z = 20; const Y = 3 * Z; X^", RuntimeValue::Num(120.0));
}


#[test]
fn point_impl_test() {
    test!("
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
    test!("
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

    test!("
        const Res = enum { Err, Ok((num, bool)) }
        let r: Res = .Ok((100, true))

        r^ is .Ok((100, !false))
    ", RuntimeValue::Bool(true));
}


#[test]
fn random_examples() {
    test!("
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