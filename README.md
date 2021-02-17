# takedouble
duplicate file finder

initial code copied from https://rosettacode.org/wiki/Find_duplicate_files#Haskell

# How do I make it go?
You can use nix or cabal to build this.

`cabal build` should produce a binary. (use [ghcup](https://www.haskell.org/ghcup/) to install cabal and the latest GHC version).

After that, `takedouble <dirname> <maxfilesizebytes>` so you could use `takedouble ~/ 10000` for example.

# Does Control.Parallel go faster?
I have a handy directory with about 50k small files and some number of duplicates. I ran the initial working commit to get this from the shell command time:
```
real    1m58.394s
user    1m44.327s
sys     0m6.516s
```
I hacked in Control.Parallel and here's the new time:
```
real    0m56.193s
user    0m50.792s
sys     0m5.959s
```
That's not as much of an improvement as I hoped on my six core Xeon.
