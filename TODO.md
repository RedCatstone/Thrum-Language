# Could do right now
- [ ] make tuples coerce to Type::Tuple, instead of treating tuples as types
- [x] enums with data
- [ ] support for runtime & and &mut
- [ ] drop logic
- [ ] match / if else correct moving logic
- [ ] ref pattern matching `x is 4` instead of `x^ is 4`
- [ ] "{single hole} string patterns"
- [x] ! negation pattern
- [ ] enum variant stand alone types e.g. Dir.West
- [ ] Ranges 0..4
- [ ] for loops


- [ ] !!! complete VM rewrite
    - stack push pop VM -> register VM
    - no more unsafe
    - WAY easier optimizations
    - [ ] heap data

# Far Future
- [ ] runtime closures
- [ ] inference params <>
- [ ] indirection pointer rule:
    - mut-borrows to the left side of an indirection can't be alias
    - e.g. can't have `&alias mut vec` if `&alias vec[1]` is also borrowed
    - e.g. can't have `&alias mut enum` if some inner part of the enum is borrowed
- [ ] swap num to u{n} / i{n} / f{16n}