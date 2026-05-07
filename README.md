# Thrum Language Ideas
Thrum is a very scripty language that takes inspiration mainly from Rust, but also Swift, Zig, Javascript and probably more!  
The Syntax is simple, but still compiles to just as efficient code as rust (in the future, for now its a vm) because it uses rusts memory model. No GC/RC.  
It has a borrow checker, which can be made less strict than Rusts for single-threaded code.  
It is strictly typed, which means that it might be slightly harder to write at first, but incredibly easy to refactor later on.  
Since languages that have really fun and nice pattern-matching are usually really fun and nice, pattern-matching is a main-focus (for now).  

## Types
- num - f64
    - there is gonna be u{n}, i{n}, f{16 * n} for any n later on
- bool - false / true
- tup - (num, num) or named (x: num, y: num)
    - allthough the () might change to {} later
    - () is nicer for unnamed tuples
    - {} is nicer for named tuples
- [T] or Vec\<T> - behaves like a rust Vec. can be created with just `let x = [1, 2, 3]`
- &[T] - slice of a Vec.
- Range - 0..10 (exclusive 0-9) or 0..=10 (exclusive 0-9)

## Operators
- Math: `+` `-` `*` `/` `%`
    - all can be paired with =
- Bitwise: AND: `~&`, OR: `~|`, XOR: `~^`, NOT: `~!`, L-SHIFT: `~<<`, R-SHIFT: `~>>`
    - all can be paired with =
    - might turn them into `b&`, `b|` ...
- Boolean: `and` `or` `!`
- Comparison: `<` `<=` `>` `>=` `is` (pattern matching for `==` and `!=`)


## Base Syntax
1. Semicolons aren't required, but can still be used to write multiple expressions one one line
    - `let x = 1; x + 2`
2. {}-blocks can be placed anywhere. the last expression will be returned
    - `let x = { do_something(); do_another_thing() }`
    - semicolons themselves are expressions and can therefore supress a blocks return value.
    - `{ do_something(); do_another_thing(); } //-> void`
3. Anywhere where a `{}`-block is required you can also put `=> expr` for small one-liners
    - `if some_condition() => failed = true`
3. Type Annotations are required if the language can't figure them out itself.


## Option
1. Options can be autoboxed:
    - `let maybe_str: Option<str> = "hello"`
    - `let maybe_str: Option<str> = .None`
    - PROBABLY only works when the compiler already knows the type, like in the example above.
    - MAYBE any enum/struct can implement this autobox behaviour.
2. you can be explicit for the some case if needed.
    - `let maybe_str: Option<str> = .Some("hello")`
3. MAYBE pattern matching with ?
    - `if maybe_str is ?str { ... }`


## Strings
- `"this is a string"` -> normal string-slice type
- string interpolation:
    - `"1 + 1 = {1 + 1}" //-> "1 + 1 = 2"`
    - a string with any `{}` holes in them is of `StrFmt` type, which includes the raw strings,
    and lazy evaluation for the data. StrFmt can be used completely normal in print() and other similar functions.
    - If you need a String you can call a .to_string() function. With this the language doesn't need macros for string formatting!
    - MAYBE StrFmt can have Empty `{}`. You will need to fill them before actually printing it. This allows for weird functions like `greet_user("Hello there {}!")`.  
    could also be used in `print("Heyo {}!", user.name)`.

## Tuples
- `(0, "hi")` has type: `(num, str)`
- Tuples can be labeled or unlabeled: `(0, "hi")` or `(x: 0, y: 1)`
- `(x: 0, y: 1)` is the same as `(y: 1, x: 0)`
- Arrays are just tuples.
    - you can iterate over a tuple if its unlabeled and all types are the same.
    - `for x in (1, 2, 3) => print(x)`
    - there is also a neater syntax to create arrays directly: `(false; 30)` (false repeated 30 times)
- if you want dynamic heap arrays, use: `[]`
- MAYBE tuples can be extended using spreading `(...old_tuple, 3)`
- MAYBE tuple types can have defaults: `let tup: (x: num, y: num = 3) = (x: 2)`
- MAYBE tuple syntax should be `{}`. or split {named labeled} and (unlabeled) like rust.


## Variable Bindings
```thrum
let a = 5
let mut b = a + 2
b += 51

let [var1, _, var2] = [1, 2, 3]  // _ ignores a value
let (x, y) = (1, "string")
let (id:, y:) = (y: 1, id: 30)  // binds id and y

// delayet let
let a
if true => a = 3
else => a = 5
```


## Borrowing from Variables
- in Thrum, everytime you use a variable it results in a borrow to that variable.
    - `let a = x` a stores a borrow to x
    - `let a = arr[2]` a stores a borrow to the second element
- to make this not annoying, it auto-derefs in a lot of places.
    - `a + 2` + and other operators can dereference borrows automatically
    - `let b = true; if b { ... }` the if-condition expects a boolean, so it automatically derefs here aswell.
- the left side of `=` needs a mut borrow that it assigns to.
    - `x = 5` (note: works because the compiler automatically changes this to `mut x = 5` because the mut here is quite obvious.)
- to create a mutable borrow, put `mut` before the variable name:
    - `let a = mut x`, now you can do `a *= 3` to mutate `x`!
    - `double_numbers(mut vec)`

## Moving Ownership `^`
- moving only works when a variable has no current borrows.
```thrum
let vec = [1, 2, 3]
let ref = vec  // this only creates a borrow, vec still exists.

let moved_vec = vec^
// now the vec has moved owners. both vec and ref are now invalid.

print("{moved_vec}")  //-> [1, 2, 3]

// arrays/tuples move things into them.
let a = [moved_vec]  // this now moved `moved_vec`
```
- simple types like: `bool` or numbers are autoclone, meaning that moving them just clones them.
- `vec.iter()` -> iters normally
- `mut vec.iter()` -> iters mutrefs
- `vec^.iter()` -> consuming iter
- DEFINITELY NOT if x was used here, the ^ above would mean clone instead of move ownership.


## "Complicated" Borrowing Rules
- normal rust rules:
    - you can have any amount of refs,
    - XOR
    - you can have 1 mutref
        - this allows for clean code, better optimizations and works flawlessly for multi-threading
- you can opt out to more relaxed rules with `alias`
    - you can have any number of `alias ref`s and `alias mutref`s to the same variable at the same time.
    - you CAN'T have a `alias mutref` to the left of an enum-boundary OR a heap-boundary.
        - example:
        ```thrum
        let vec = [1, 2, 3]
        let x = alias vec[0]
        vec[1] = 5  // works perfectly fine
        vec.push(5)  //! ERROR, `x` and `mut vec` are across a heap-boundary, so you can't have both.
        ```
- MAYBE Lifetime annotation:
    - fn returns a pointer: `fn get(&#a self, key: K) -> &#a V`
    - fn returns a mut pointer: `fn get_mut(mut &#a self, key: K) -> mut &#a V`
        - in these cases annotation wouldn't even be required because of elision
    - type with pointers: `type Pet<#a> = (&#a owner: str)`

## Moving Ownership `^`
```thrum
let x = [1, 2]
let y = x^  // moved owner
let nested_arr = [x, [2, 3]]
// collections always want ownership, unless you explicitely say that they should be pointers. x^ is moved and owned, [2, 3] is owned, perfect!
```




## If Statements
- {} or => are required on the if block, but on else those can be omitted.
    - `if x > 5 => total += 1`
- returns the value of both if/else arms, meaning they have to match types.
    - `let x: Option<num> = if var1 == "123" { .Some(123) } else .None`
- Nullish operators:
    - ?. `let x_option = point_option?.x`
        - desugared from `if point_option is ?p { p.x^ } else .None`
    - ?? `let point = point_option ?? Point { x: 1, y: 4 }`
        - desugared from `if point_option is ?p { p^ } else Point { x: 1, y: 4 }`


## Functions
- `fn greet(p: str) => print("Hello {p}")`, `fn square = |x => x * x`
- the fn arguments are basically just a tuple, so anything that works for them will work here
    - named calling `add_nums(num2: 3, num1: 4)`
    - defaults `fn math(x: num, y: Option<num> = null) -> ...` can be called with `math(2)`
    - spreading `add_nums(...(1, 2))`

- anonymous functions / closure syntax `|param1, param2, ... => body`:
    - `(0..10).iter().map(|x => x + 2).collect<Vec>() //-> [2, 3, 4, 5, ..., 11]`
    - `thread.spawn(|=> print("spawned!"))`
    - `(|x => x + 1)(5)` -> 6
```thrum
// MAYBE anonymous functions do not eat return/break/continues.
`(0..10).iter()
    .map(|x => if x < 8 {
        x * x
    } else {
        // this would return the outer function, not this closure
        return "error, number too big"
    })
    .collect<Vec>()`
// if you do want to return to the closure, use a labeled block and break to that.
// this would need extra rules so "return"-closures don't get moved out of the function.
```


## Pattern Matching
```thrum
match response
is 200 | 202 => "Success"
is 404 => { "Not Found" }
is 500..600 => "500s!"
is _ => "default"
```
- all patterns are:
    - compare expressions: `3`, `true`, `3 * 3` (literally any expression)
    - wildcard: `_`
    - tuple `(x, y)`, `(x: 2, y: 3)`, `(x, ...)`
    - enum `.North`, `.Some(x: 2)`, `?sugar`
    - string `"result: 42" is "result: {let result}"`
    - or `pattern | other`
    - conditional `pattern and cond`, e.g. `x is let y and y % 2 is 0`
- quick one line matches can be done using an `is`-expression.
    - `if x is 40 { ... }`
    - `if num_option is let ?x and x > 100 { ... }`
    - is-expressions that bind variables can only be used in if/while conditions.
        - fine: `let matched = x is 3..40`
        - BAD: `let matched = y_option is let ?y` conditionally binds y
    - `x is y` basically does x == y
    - `x is (y, _)` basically does x.0 == y
    - `x is let (y, _)` binds y

    - other old syntaxes before `is` i went through:
        - `case 0..5 = x` (swift)
        - `x ~> 0..5`
        - `x matches 0..5`
        - but now its `x is 0..5`
```thrum
ensure File.open("data.txt") is let .Ok(file) else return .Err("Failed to open")
for line in file.lines() {
    ...
}

// string matching, only 0 or 1 holes allowed.
// (otherwise this would need a specific searching order and probably the heap)
if output is "ERROR: {let msg}" {
    print("{msg}")
}

// slices
if recipe is let [...ingredients, result] {
    print_recipe(ingredients, result)
}
```

## Labeled Loops and Break/Continue
- loops can be given labels like so: `for #outer i in 0..10 { ... }`
- these labels can then be used in `break #outer` or `continue #outer`
- each loop has a default label so you can just type `break #for` to break the nearest for-loop without a custom label
- inside infinite `loop`s break can return values: `break 10` or `break #outer 10`


## Constants
- constants are evaluated at compile time.
- `const x = 5 * 3`
- `const x = { let y = 5; y * 3 }`
- any functions that doesn't do any io things can be used in const contexts
- consts are used in these places:
    - `const x = ...`
    - `let x: ... = 3`  (type annotation)
    - `(false; ...)`  (array const length)

## Types are values
- types are just normal variables, but they can only be used in const contexts
```thrum
const Number = u32
let x: Number = 15

// newtype
type NewNumber = u32
let x = Number(13)
x + 4  //! ERROR: can't add types NewNumber and u32
x + Number(4)  // this works

const Number2 = NewNumber  // this is just a const variable, not a newtype.
x + Number2(5)  // works


// a tuple can coerce to a Type, if all its fields are types.
type Point = (x: num, y: num)
let p = Point(x: 3, y: 4)
assert!(p.distance() is 5)

impl Point {
    fn distance(&self) -> num {
        (self.x.pow(2) + self.y.pow(2)).sqrt()
    }
}


type Direction = enum { North, East, West, South(num) }
let dir1: Direction = .North
let dir2 = Direction.South(4)
assert!(dir2 is .South(let x and x % 2 == 0))


// would be really cool if this works:
type isize = i{System.bits}
// but probably gonna be this
type isize = match System.bits
    is 64 => i64
    is 32 => i32
    is _ => panic("could not generate type isize...")
```

## as Conversions
- `as` will always be a safe conversion
    - converting from i32 -> i64 is safe.
    - converting i64 -> i32 would need an unwrap. `as?` or similar
- `as` can behave kinda like inline type annoations
    - `let numbers = [1 as i32, 1, 2, 3, 5]`
    - `[1, 2, 3] as VecDequeue`
    - `let i = 1 + 2 as Complex`
- MAYBE make it `.as()` instead, because the precedence is more readable then

## Special Syntax
- special panicky sugared syntax:
    - .get_panicky(...) can be called using struct[...].
- ensure for inverted binding-if's: `ensure x_option is ?x else { return }`
- DEFINITELY NOT a function called with only 1 string argument can be called without parantheses.
    - `"a=b=c".split"=" //-> ["a", "b", "c"]`
    - `print"{} + {} = {}".fmt!(1, 1, 2) //-> 1 + 1 = 2` this does look weird though...
    - `print("{} + {} = {}".fmt(1, 1, 2)) //-> 1 + 1 = 2`


# Motivations
Just a fun project I am working on!  
I'm trying to make the absolute perfect language for myself.
```
|\   /|_,,,---,,_
/o`.o`        -. `\-;;,_
|,4- , ) )-,_  ,\ (  `'-'
 `-'' (_/-'   `-'\_)
```


# Todos
- [Todo List](TODO.md)