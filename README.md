# Regular Expression Derivatives #
A naive implementation of regular expression matching using derivatives, as
presented in [*Regular-expression derivatives reexamined*][reexamined] by
Owens, Reppy, Turon.

## Getting Started ##
[`re.ml`](re.ml) contains a very naive implementation of extended regular
expression matching using regular expression derivatives. [`main.ml`](main.ml)
implements a repl which accepts bitstrings of the form `(0|1)*` and outputs
whether each bitstring has even parity as determined by the regular expression
`(0*10*10*)*`.

Here is an example of how to build and interact with the repl.
```bash
$ corebuild -pkg async main.byte && ./main.byte
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
λ ^C
```

[reexamined]: http://www.mpi-sws.org/~turon/re-deriv.pdf
