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
ghc day01.hs && ./day01
```

## Debugging and Linting

To print a simple debug output (without complicating things with the IO monad!), use [Debug.Trace](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)

To debug performance issues, include profiling info with

```bash
ghc day01.hs -prof -fprof-auto && ./day01 +RTS -p
```

which creates an associated `day1.prof` file.

To print stack trace for an exception, use the `xc` flag:

```bash
ghc day01.hs -prof -fprof-auto && ./day01 +RTS -xc
```

To lint a file using [Hlint](https://github.com/ndmitchell/hlint),

```
brew install cabal-install
cabal update
cabal install hlint
~/.cabal/bin/hlint day1.hs
```

## Resources

- [Simple Haskell Examples](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples)
