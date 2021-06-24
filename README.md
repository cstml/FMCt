# FMCt

Typed Functional Machine Calculus.

The project is in active development, and there will be many breaking changes. 

With that being said, I will try and correctly note the versions of each.

## Building

To build there are two ways, depending on which you prefer:

### With Nix

1. pull the repo,
2. run: `nix-build release.nix && ./result/bin/FMCt`,
3. you are in the repl,
4. Have fun!

### With Cabal

1. pull the repo,
2. run: `cabal new-build && cabal run`,
3. you are in the repl,
4. have fun!

#### With GNUMake

- this will just use the NIX, but might update later.

## Examples

To play with examples, have a look in the source files as there are examples
sprinkled around. Especially in `src-exe/Examples.hs`.
