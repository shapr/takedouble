# takedouble
duplicate file finder that reads the filesize and first 4k and last 4k of a file and assumes that's enough to tell you if files are duplicates.

# How do I make it go?
You can use nix or cabal to build this.

`cabal build` should produce a binary. (use [ghcup](https://www.haskell.org/ghcup/) to install cabal and the latest GHC version).

After that, `takedouble <dirname>` so you could use `takedouble ~/` for example.

# is it fast?

On my ThinkPad with six Xeon cores and a 2TB Samsung 970 Evo Plus,  I can check 124822 files in 1.9 seconds.
