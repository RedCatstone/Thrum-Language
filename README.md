# Thrum Language Ideas
Thrum is a very scripty language that takes inspiration mainly from Rust, but also Swift, Zig, Javascript and probably more!  
The Syntax is simple, but still compiles to just as efficient code as rust (in the future, for now its a vm) because it uses rusts memory model. No GC, no RC.  
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
- Boolean: `and` `or` `!`
- Comparison: `<` `<=` `>` `>=` `is` (pattern matching for `==` and `!=`)


## Base Syntax
1. Semicolons aren't required, but can still be used instead of a new line to write multiple expressions on one line: `let x = 1; x + 2`
2. {}-blocks can be placed anywhere. the last expression will be returned
    - `let x = { do_something(); do_another_thing() }`
    - semicolons themselves are expressions and can therefore supress a blocks return value.
    - `{ do_something(); do_another_thing(); } //-> void`
3. Type Annotations are required if the language can't figure them out itself.


## Option
1. Options can be autoboxed:
    - `let maybe_str: Option<str> = "hello"`
    - `let maybe_str: Option<str> = .None`
    - any enum/struct can implement this autobox behaviour.
2. you can be explicit for the some case if needed.
    - `let maybe_str: Option<str> = .Some("hello")`
3. MAYBE pattern matching with ?
    - `if maybe_str is ?str { ... }`


## Strings
- `"this is a string"`
- string interpolation:
    - `"1 + 1 = {1 + 1}" //-> "1 + 1 = 2"`
    - empty `{}` get ignored by interpolation. This is so you can use .fmt!() if stuff ends up too long. These arguments will then fill the empty `{}` in order. (that function is a macro because the searching for {} is exclusively done at compiletime.)
```thrum
"hello {} and {other_name}, today is {}".fmt!(
    System.name.hack
    System.calender.hack.getCurrentDayAsWeekday(),
)
```

## Tuples
- `(0, "hi")` has type: `(num, str)`
- Tuples can be named or unnamed: `(0, "hi")` or `(x: 0, y: 1)`
- `(x: 0, y: 1)` is the same as `(y: 1, x: 0)`
- MAYBE tuples can be extended using spreading `(...old_tuple, 3)`
- MAYBE tuple types can have defaults: `let tup: (x: num, y: num = 3) = (x: 2)`

## Variable Bindings
```thrum
let a = 5
let mut b = a + 2
b += 51

let [var1, _, var2] = [1, 2, 3]  // _ ignores a value
let (x, y) = (1, "string")
let (id:, y:) = (y: 1, id: 30)  // binds id and y
```


## Ownership / Borrowing
1. MAYBE defaults for type annotation: Owning `own` / Borrowing `ref`:
    - function arguments: **Borrow**
        - `fn print_list(list: [num]) { ... }` (ref to a Vec of owned nums)
    - function return type: **Owned**
        - `fn make_list() -> [num] { ... }` (owned Vec)
    - type fields: **Owned**
        - `type Grid<T> = (h: num, w: num, grid: [[T]])` (all fields are owned)
    - let binding: **Owned**
        - `let x: [num] = [1, 2]` (owned Vec)
        - `let y: ref [num] = x` (ref to a Vec)

2. any binding can also be mut
    - e.g. `fn append_elem(list: mut [num]) { ... }`
    - would be called:
        - `append_elem(mut numbers, 3)`
        - `append_elem([1, 2, 3], 4)`
        - `append_elem(x^, 4)` this would move x (quite useless here because it will be discarded after the function returns)

## References
- In thrum everytime you use a variable, under the hood its a reference to that variable.
    - `arr[2]` is getting a reference to arr and indexing into it
    - `a + 2` + and other operators can dereference referebces automatically
    - `let b = true; if b { ... }` the if-condition expects a boolean, so it automatically derefs here aswell.
    - `x = 5` the left side of = needs a mut reference that it assigns to. the compiler automatically changes this to `mut x = 5` because the mut here is quite obvious.
    - `let a = x` a stores a reference to x
- to create a mut reference, put `mut` before the variable name:
    - `let a = mut x`
    - `double_num(mut x)`
    - MAYBE rust-like-sugar for `let a = mut 5`

## Borrow checking
- normal rust rules:
    - you can have any amount of refs,
    - XOR
    - you can have 1 mutref
- you can opt out to less strict rules with `alias ref`s
    - you can have refs and mutrefs to the same variable at the same time.
    - you CAN'T have a `alias mutref` to the left of an enum-boundary OR a heap-boundary.
        - example:
        ```thrum
        let vec = [1, 2, 3]
        let x = loose vec[0]
        vec[1] = 5  // works perfectly fine
        vec.push(5)  //! ERROR, `x` is across a heap-boundary, so you can't get a mutref of vec
        ```
- MAYBE Lifetime annotation:
    - fn returns a pointer: `fn get(ref#a self, key: K) -> ref#a V`
    - fn returns a mut pointer: `fn get_mut(mut ref#a self, key: K) -> mut ref#a V`
        - in these cases annotation wouldn't even be required because of elision
    - type with pointers: `type Pet<#a> = (ref#a owner: str)`

## Moving Ownership `^`
```thrum
let x = [1, 2]
let y = x^  // moved owner
let nested_arr = [x, [2, 3]]
// collections always want ownership, unless you explicitely say that they should be pointers. x^ is moved and owned, [2, 3] is owned, perfect!
```
- `vec.iter()` -> iters normally
- `mut vec.iter()` -> iters mutrefs
- `vec^.iter()` -> consuming iter
- DEFINITELY NOT if x was used here, the ^ above would mean clone instead of move ownership.



## If Statements
- {} or => are required on the if block, but on else those can be omitted.
    - `if x > 5 => total += 1`
- returns the value of both if/else arms -> meaning they have to match types.
    - `let x: Option<num> = if var1 == "123" { 123 } else .None`
- Nullish operators:
    - ?. `let x_option = point_option?.x`
        - desugared from `if point_option is ?p { p.x^ } else .None`
    - ?? `let point = point_option ?? Point { x: 1, y: 4 }`
        - desugared from `if point_option is ?p { p^ } else Point { x: 1, y: 4 }`


## Functions
- `fn greet(p: str) { print("Hello {p}") }`, `fn square = |x => x * x`
- the fn arguments are just 1 tuple, so anything that works for them will work here
    - named calling `add_nums(num2: 3, num1: 4)`
    - defaults `fn math(x: num, y: Option<num> = null) -> ...` can be called with `math(2)`
    - spreading `add_nums(...(1, 2))`

- anonymous functions / closure syntax `|param1, param2, ... => body`:
    - `(0..10).iter().map(|x => x + 2).collect<Vec>() //-> [2, 3, 4, 5, ..., 11]`
    - `thread.spawn(|=> print("spawned!"))`
- can be called right at creation: `(|x => x + 1)(5)` -> 6


## Pattern Matching
```thrum
match response
is 200 | 202 => "Success";
is 404 => { "Not Found" }
is 500..600 => "500s!";
is _ => "default"
```
- all patterns are:
    - literals: `3`, `true`
    - wildcard: `_`
    - tuple `(x, y)`, `(x: 2, y: 3)`, `(x, ...)`
    - enum `.North`, `.Some(x: 2)`, `?sugar`
    - string `"result: 42" is "result: {let result}"`
    - or `pattern | other`
    - conditional `pattern and cond`, e.g. `x is let y and y > 5`
- quick one line matches can be done using an `is`-expression.
    - `if x is 40 { ... }`
    - `if num_option is let ?x and x > 100 { ... }`
    - is-expressions that bind variables can only be used in if/while conditions.
        - fine: `let matched = x is 3..40`
        - BAD: `let matched = y_option is let ?y` conditionally binds y
    - `x is y` basically does x == y
    - `x is (y, _)` basically does x.0 == y
    - `x is (let y, _)` binds y

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

// string matching (0 or 1 holes allowed, otherwise this would need a specific searching order and probably the heap)
if output is "ERROR: {let msg}" => print("{msg}")

// slices
if recipe is let [...ingredients, result] => print_recipe(ingredients, result)
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
- they 

## Types are values
- types can be defined anywhere, but they have the same scoping as variables.
```thrum
type Number = u32
let x = Number(13)
x + 4  //! ERROR: can't add types Number and u32
x + Number(4)  // this works

const Number2 = Number  // this is just a const variable, not a newtype.
x + Number2(5)  // works


// a tuple is a Type, if all its fields are types.
type Point = (x: num, y: num)
let p = Point(x: 3, y: 4)
assert!(p.distance() is 5)

impl Point {
    fn distance(&self) -> num => (self.x.pow(2) + self.y.pow(2)).sqrt()
}


type Direction = enum { North, East, West, South(num) }
let dir1: Direction = .North
let dir2 = Direction.South(4)


// would be really cool if this works:
type usize = u{System.bits}
// but probably gonna be this
type isize = match System.bits
    is 64 => i64
    is 32 => i32
    is _ => panic("could not generate type isize...")
```

## Special Syntax
- DEFINITELY NOT `expression_Type`:
    - `[1, 2, 3]_Vec` desugars to `Vec::new(data = [1, 2, 3])`
    - `let complex_number = 1 + 2_Complex` this syntax would be really cool, and very scripty, which is the goal. `1 + 2_I` would be even better, but i doubt it would work with this system.
- special panicky sugared syntax:
    - .get(...) can be called using struct[...].
    - .insert(..., ...) can be called using struct[...] = ...
- ensure for inverted binding-if's: `ensure x_option is ?x else { return }`
- DEFINITELY NOT a function called with only 1 string argument can be called without parantheses.
    - `"a=b=c".split"=" //-> ["a", "b", "c"]`
    - `print"{} + {} = {}".fmt!(1, 1, 2) //-> 1 + 1 = 2` this does look weird though...
    - `print("{} + {} = {}".fmt!(1, 1, 2)) //-> 1 + 1 = 2`


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
- data on enums
- for loops
- complete vm remake. (current one uses unsafe and slow push/pop stack)
    - with that also a simple optimizer and evaluator remake
- swap num to u{n} / i{n} / f{16n}
- slices
- pointer pattern matching
- far future: heap data [1, 2, 3]