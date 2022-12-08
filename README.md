# aoc-melange

Setting up to do [Advent of Code](https://adventofcode.com/) problems using ReasonML, compiling with [Melange](https://github.com/melange-re/melange)

## Quick Start

```shell
make init

# build watch with:
make watch

# run something:
node _build/default/src/2022/Day01Part1.bs.js
```

## Commands

You can see all available commands by running `make help` or just `make`. Here
are a few of the most useful ones:

- `make init`: set up opam local switch and download OCaml, Melange and
JavaScript dependencies
- `make install`: install OCaml, Melange and JavaScript dependencies
- `make watch`: watch for the filesystem and have Melange rebuild on every
change