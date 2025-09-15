# Thrum Language

## Predefined Structs
1. num - f64
2. bool - false / true
3. str - one string type for everything, compiler will figure it out
4. arr\<T> - one Array type for everything, compiler will figure it out
5. dict\<K, V> - Map/Object in one maybe seperate Set
6. range - 0..10 (inclusive 0-10) or 0..<10 (exclusive 0-9)

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
- Less <
- Greater >
- Less (inclusive) <=
- Greater (inclusive) >=
- Equals ==
- Not Equals !=
- Not !


## Base Syntax
1. `let a = "123"`
2. Type Annotations are required if the language can't figure them out itself.
3. Destructuring:
    - `let [var1, _, var2] = [1, 2, 3]`  // _ means ignore
    - `let {x, y} = positionDict`
    - `let x..y = 1..5`
4. easy variables inside strings: `let y: Str = "var1 is {var1}, hehe {var2}"`
5. {}-blocks can be placed anywhere. the last expression will be returned
7. arr, dict, range, str all implement .iter() by default

## If Statements
- {} are required on the if block, but else {} can be omitted.
- returns the value of both if/else arms -> meaning they have to match types.
    - `let x: str? = if (var1 == "123") 123 else null`
- Nullish Optional Chaining ?. `let pos = event?.target?.position`

## Functions
- anonymous (no-name): `x: num -> 2*x`, `(x, y) -> x**y`, `() -> "lel"`
- named: `fn greet(p: str) { print"Hello {p}" }`, `fn square = (x) -> x * x`
- a function parameter can be left out if it can either be null OR it has a default 
    - `fn math(x:num, y:num?) -> x*y` can be called like `math(2)`, y will be null
    - `fn math(x:num, y:bool=false) -> x*y` can be called like `math(2)`, y will be false
- can be called right at creation: `(x -> x+1)(5)` -> 6
- destructuring into function parameters also works, not named though.
```
fn point(x: num, y:num) -> "{x}, {y}";
point(2, 3)  // -> "2, 3"

point(...[1, 42])  // -> "1, 42"
```
- extra parameters get discarded. `point(...[1, 2, 3])` -> "1, 2"

## Match
```
// not valid, since these are 3 different types, just for show
match response {
    (200, 202) -> "Success";
    404 -> { "Not Found" }
    500..600 -> "500s!";
    _ -> "default"
}
```

## Special Syntax

1. Semicolons aren't required, but can still be used instead of a new line to write multiple expressions in one line

2. lambda function syntax: `(0..10).map(x -> x + 2).as_vec()`  -> [2, 3, 4, 5, ..., 12]
    - special stuff here, functions defined on .iter() just work on the base, here a Range.
    - vs code would show a gray rust like .iter() call in the middle there

3. nullable types with `let lel: String? = "hello"`
    - `let x = lel ?? "default string"`
    - `let x = if lel.not_none() { /* lel now gets treated as a non null type */ } else "default"`

4. special panicky sugared syntax:
    - .get(...) can be called using struct[...].
    - .insert(..., ...) can be called using struct[...] = ... 

5. pipe operator: `users.filter(x -> x.isAdmin) |> sort(^) |> print"sorted: {^}"`
    - extreme example: `numbers.map(n -> n**2) |> { even: ^.where(x -> x%2 == 0), odd: ^.where(x -> x%2 == 1) }`

6. a function called with only 1 string argument can be called like `"a=b=c".split"="` -> ["a", "b", "c"]