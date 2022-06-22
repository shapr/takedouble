# takedouble
TakeDouble is a duplicate file finder that reads and checks the filesize and first 4k and last 4k of a file and only then checks the full file to find duplicates.

# How do I make it go?
You can use nix or cabal to build this.

`cabal build` should produce a binary. (use [ghcup](https://www.haskell.org/ghcup/) to install cabal and the latest GHC version).

After that, `takedouble <dirname>` so you could use `takedouble ~/` for example.

# Is it Fast?

On my ThinkPad with six Xeon cores, 128GB RAM, and a 1TB Samsung 970 Pro NVMe (via PCIe 3.0), I can check 34393 uncached files in 6.4 seconds.
A second run on the same directory takes 2.8 seconds due to file metainfo cached in memory.
