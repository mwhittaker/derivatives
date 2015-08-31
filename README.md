# Regular Expression Derivatives #
A pair of implementations of regular expression matching using derivatives, as
presented in [*Regular-expression derivatives reexamined*][reexamined] by
Owens, Reppy, Turon.

## Getting Started ##
This repository contains two implementations of extended regular expression
matching using derivatives: a [naive one in `naive`](naive) and an [optimized
one in `opt`](opt). Both implementations include a repl which accepts
bitstrings of the form `(0|1)*` and outputs whether each bitstring has even
parity as determined by the regular expression `(0*10*10*)*`.

Navigate to either `naive` or `opt`, build and launch the repl, and then enter
bitstrings. For example, here's what it looks like when you run the naive
implementation.

```bash
$ corebuild -pkg async main.byte && rlwrap ./main.byte
(0*10*10*)*
λ
true
λ 0
false
λ 1
false
λ 00
false
λ 11
true
λ 01010
true
λ 00101000010101000111110010111001000100110101010
Fatal error: out of memory.
```

And here's what it looks like when you run the optimized version:

```bash
$ corebuild -pkg async main.byte && rlwrap ./main.byte
(0*10*10*)*
λ
true
λ 0
false
λ 1
false
λ 00
false
λ 11
true
λ 01010
true
λ 00101000010101000111110010111001000100110101010
false
λ 00101010100011111111111111111111111111111100000000000000000000000000010100101
true
```

[reexamined]: http://www.mpi-sws.org/~turon/re-deriv.pdf
