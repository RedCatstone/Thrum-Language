# Thrum Language
Thrum is a very scripty language that takes inspiration from Rust, Javascript, Swift, Zig and probably more!
Its Syntax is simple, but still compiles down to efficient code (hopefully.)
It is strictly typed, which means that its harder to write at first, but incredibly easy refactoring later on.
It has very expressive Pattern-matching, which makes for some very satisfying code.

## Compiler Types
1. num - f64 // there is gonna be u{n}, i{n}, f{n} for any n later on
2. bool - false / true
3. slice\<T> - has data and length.
4. dict\<K, V> - Map/Object in one maybe seperate Set
5. range - 0..10 (inclusive 0-10) or 0..<10 (exclusive 0-9)

## Default Types
1. Vec\<T> - has data length and a capacity. you can push and pop values here.
2. str - one string type for everything, (maybe? it effectively is just a Vec\<u8> though)

## Operators
- Plus +
- Minus -
- Multiplication *
- Division /
- Exponentiation **
- Modulo %
- Nullish Coalescing ??
- Bitwise AND ~&
- Bitwise OR ~|
- Bitwise XOR ~^
- Bitwise NOT ~!
- Left Shift ~<<
- Right Shift ~>>
- all of the above can be paired with =

Boolean Operators:
- And &
- Or |
- Not !
- Less <
- Greater >
- Less (inclusive) <=
- Greater (inclusive) >=
- Equals ==
- Not Equals !=

## Base Syntax
1. Semicolons aren't required, but can still be used instead of a new line to write multiple expressions on one line: `let x = 1; x + 2`
2. {}-blocks can be placed anywhere. the last expression will be returned
    - `let x = { do_something(); do_another_thing() }`
    - semicolons themselves are expressions and can therefore supress a blocks return value.
    - `{ do_something(); do_another_thing(); } //-> void`
3. Type Annotations are required if the language can't figure them out itself.


## Nullable ?Types
1. nullable types are types with a ? before them:
    - `let maybe_str: ?str = ?"hello"`
2. the language can automatically put ? before expressions if needed.
    - `let maybe_str: ?str = "hello"`
3. internally these are just an `Option` enum. you can also just type `Option::Some("hello")` instead of `?"hello"` if you really wanted to.


## Strings
1. `"this is a string"`
2. string interpolation:
    - variables can be put into {}.
    - `"1 + 1 = {1 + 1}" //-> "1 + 1 = 2"`
    - if the expression is too long, you can put it into parentheses after the string. These arguments will then fill the empty `{}` in order.
```thrum
"hello {} and {other_name}, today is {}"(
    System.name.hack
    System.calender.hack.getCurrentDayAsWeekday(),
)
```

## Tuples
1. `(0, "hi")` has type: `(num, str)`
2. Tuples can be named or unnamed: `(0, "hi")` or `(.x = 0, .y = 1)`
3. `(.x = 0, .y = 1)` is the same as `(.y = 1, .x = 0)`
4. MAYBE tuples can be extended using spreading `(...old_tuple, 3)`

## Variable Bindings
1. `let a = 5`
    - a can then be used anywhere: `let b = a + 2`
2. `let mut b = 3`
    - b can then be modified: `b += 51`
3. Destructuring:
    - `let [var1, _, var2] = [1, 2, 3]`  // _ means ignore
    - `let (x, y) = (1, "string")`
    - `let (.id = x, .y) = (.y = 1, .id = 30)`
    - `let x..y = 1..5`


## If Statements
- {} are required on the if block, but else {} can be omitted.
- returns the value of both if/else arms -> meaning they have to match types.
    - `let x: str? = if (var1 == "123") 123 else null`
- Nullish operators (maybe):
    - ?. `let x_option = point_option?.x`  
        - desugared from: `point_option.map_some(|p -> p.x)`
        - allthough this can just be `if case ?p = point_option { p.x } else null`
    - ?? `let point = point_option ?? Point { .x = 1, .y = 4 }`


## Functions
2. named: `fn greet(p: str) { print"Hello {p}" }`, `fn square = (x) -> x * x`
3. a function parameter can be left out if it has a default 
    - `fn math(x: num, y: num? = null) -> x*y` can be called with `math(2)`, y will be null
```thrum
fn point(x: num, y:num) -> { "{x}, {y}" }
point(2, 3)  //-> "2, 3"

// destructuring into function parameters:
point(...[1, 42])  //-> "1, 42"
point(...[1, 2, 3]) //-> "1, 2"  // 3 gets discarded

// calling using named parameters:
point(y = 3, x = 5)  //-> "5, 3"
```
5. anonymous functions / closure syntax is `|param1, param2, ... -> body`:
    - `(0..10).map(|x -> x + 2).collect<Vec>() //-> [2, 3, 4, 5, ..., 12]`
    - ranges themselves are iterators, so this works.
6. can be called right at creation: `(|x -> x+1)(5)` -> 6


## Match
```thrum
// not valid, since these are 3 different types, just for show
match response {
    (200, 202) -> "Success";
    404 -> { "Not Found" }
    500..600 -> "500s!";
    _ -> "default"
}
```
1. quick one line matches can be done using a `case`-expression.
    - `if case 40 = x { /* x was 40. */ }`
    - `if case ?x = num_option && x > 100 { /* unwrapped num_option and it was greater than 100*/ }`
    - case expressions can be used if they are the last expression of a block. They are mostly used within `if`/`while` though. They can also be chained with `&&`, but NOT with `||`, `!`.
    - `let matched = { case 30 = x }; ...` this only works because of the {}

2. i was thinking about making this case-syntax even shorter, but i'm not so sure about it, as the syntax would be inverted (`expression ~> pattern` instead of `case pattern = expression`). It does however get rid of the `=`, which can be confusing.
    - `while case ?x = queue.pop() { ... }`
    - `while queue.pop() ~> ?x { ... }` 4 chars shorter
    - `if case 0..10 = num { ... }`
    - `if num ~> 0..10 { ... }`

## Ensure
```thrum
if case ?elem = arr.get(10) {
    print("{elem}")
}
else { return }

// is the same as
ensure case ?elem = arr.get(10) else {
    return // something that has type never
}
print("{elem}")
```

## Labeled Loops and Break/Continue
1. loops can be given labels like so: `for #outer i in 0..10 { ... }`
2. these labels can then be used in `break #outer` or `continue #outer`
3. each loop has a default label so you can just type `break #for` to break the nearest for-loop without a custom label
4. inside infinite `loop`s break can return values: `break 10` or `break #outer 10`


## Type Definitions
1. types can be defined anywhere, but they have the same scoping as variables.
    - `type Point = ( .x = num, .y = num)`
    - `enum Directions { North, East, West, South(num) }`
    - enum types can have extra Tuples as data on them.
2. types can have `impl Point { ... }` blocks. any variables/functions implemented here can then be accessed using `Point::distance()`.

## Special Syntax
1. `expression_Type`:
    - `[1, 2, 3]_Vec` desugars to `Vec::new(data = [1, 2, 3])`
    - `let complex_number = 1 + 2_Complex` this syntax would be really cool, and very scripty, which is the goal. `1 + 2_I` would be even better, but i doubt it would work with this system.
2. special panicky sugared syntax:
    - .get(...) can be called using struct[...].
    - .insert(..., ...) can be called using struct[...] = ... 
3. MAYBE pipe operator: `users.filter(x -> x.isAdmin) |> sort(^) |> print"sorted: {^}"`
    - extreme example: `numbers.map(n -> n**2) |> { even: ^.where(x -> x%2 == 0), odd: ^.where(x -> x%2 == 1) }`
4. MAYBE a function called with only 1 string argument can be called without parantheses.
    - `"a=b=c".split"=" //-> ["a", "b", "c"]`
    - `print"{} + {} = {}"(1, 1, 2) //-> 1 + 1 = 2` this does look weird though...
    - `print("{} + {} = {}"(1, 1, 2)) //-> 1 + 1 = 2`
5. slice, Vec, Set, range, str all implement .iter()


# Motivations
Just a fun project I am working on!  
I'm trying to make the absolute perfect language for myself.
```
|\   /|_,,,---,,_
/o`.o`        -. `\-;;,_
|,4- , ) )-,_  ,\ (  `'-'
 `-'' (_/-'   `-'\_)
```