# Advent of Code Solutions 2020

https://adventofcode.com/2020

This year, let's get festive with [Haskell](https://www.haskell.org/)!

## Environment Setup

Install the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/).

```bash
brew install ghc
```

## Running the Solutions

Compile then run the solution

```bash
ghc day1.hs && ./day1
```

To debug performance issues, include profiling info with

```bash
ghc day1.hs -prof -fprof-auto && ./day1 +RTS -p
```

which creates an associated `day1.prof` file.

## Resources

- [Simple Haskell Examples](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples)
