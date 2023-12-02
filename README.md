# ARCHIVED REPOSITORY INFO
This was what I used in 2022. Updated things in a new repo in 2023 at: https://github.com/trite/melange-aoc


# aoc-melange

Setting up to do [Advent of Code](https://adventofcode.com/) problems using ReasonML, compiling with [Melange](https://github.com/melange-re/melange)

## Quick Start

```shell
make init

# build watch with:
make watch

# run something:
node _build/default/src/2022/Day01.bs.js
```

## Folder Structure

Code and data folders are laid out to allow for multiple years of Advent of Code solutions in the same project.
 * Data in `data/{year}/` (ex: `data/2022/Day01.txt`)
 * Code in `src/{year}/` (ex: `src/2022/Day01.re`)

Commonly reused helper stuff lives in `src/shared`.

## Commands

You can see all available commands by running `make help` or just `make`. Here
are a few of the most useful ones:

- `make init`: set up opam local switch and download OCaml, Melange and
JavaScript dependencies
- `make install`: install OCaml, Melange and JavaScript dependencies
- `make watch`: watch for the filesystem and have Melange rebuild on every
change
