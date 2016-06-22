# Alioth Computer Language Benchmark Game for Haskell

A central repository for playing [The Computer Language Benchmark Game](http://benchmarksgame.alioth.debian.org).

The object of the game is to move Haskell to a higher rank on the [Which programs are fastest?](http://benchmarksgame.alioth.debian.org/u64q/which-programs-are-fastest.html) page.

To play the game, one usually goes to the benchmarks game website, copies an existing benchmark Haskell implementation, and then makes a `.cabal` file and a `stack.yaml`, gets the program to build, and then sets up a testing harness. This repo sets all that up so that playing the game is less like work and more like a game.


## Haskell Champions 2016-05-29

| Benchmark | How Much Slower | Program |
|---|---|---|
|[pidigits](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=pidigits)|1.8×|[Haskell GHC #4](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=pidigits&lang=ghc&id=4)|
|[spectral-norm](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=spectralnorm)|2.0×|[Haskell GHC #4](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=spectralnorm&lang=ghc&id=4)|
|[n-body](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=nbody)|2.6×|[Haskell GHC #2](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=nbody&lang=ghc&id=2)|
|[fannkuch-redux](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=fannkuchredux)|1.9×|[Haskell GHC #3](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=fannkuchredux&lang=ghc&id=3)|
|[reverse-complement](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=revcomp)|3.4×|[Haskell GHC #3](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=revcomp&lang=ghc&id=3)|
|[regex-dna](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=regexdna)|4.9×|[Haskell GHC #2](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=regexdna&lang=ghc&id=2)|
|[fasta](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=fasta)|2.2×|[Haskell GHC #6](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=fasta&lang=ghc&id=6)|
|[binary-trees](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=binarytrees)|6.4×|[Haskell GHC #4](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=binarytrees&lang=ghc&id=4)|
|[mandelbrot](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=mandelbrot)|2.6×|[Haskell GHC #2](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=mandelbrot&lang=ghc&id=2)|
|[k-nucleotide](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=knucleotide)|~~2.7×~~|Disqualified: ~~[Haskell GHC](http://benchmarksgame.alioth.debian.org/u64q/program.php?test=knucleotide&lang=ghc&id=1)~~ "k-nucleotide now explicitly requires built-in / library HashMap" -- Issac Gouy|
|[fasta-redux](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=fastaredux)||No entry|

TODO generate this table from the [Summary Data](http://benchmarksgame.alioth.debian.org/u64q/summarydata.php).

## Benchmarking

### Benchmarking with [Criterion](https://hackage.haskell.org/package/criterion)

TODO

#### Comparing the Criterion benchmarks for two commits

TODO

### Benchmarking with the [Alioth Python scripts](http://benchmarksgame.alioth.debian.org/download/benchmarksgame-script.zip)

TODO

#### Comparing the Alioth Python script benchmarks for two commits

TODO

## Profiling

TODO

#### Comparing profiles for two commits

TODO

## Reading the Core

TODO

#### Comparing Core for two commits

TODO

## Submitting to Alioth

Instructions: [Contribute your programs](http://benchmarksgame.alioth.debian.org/u64q/play.html)

#### Tips

Issac Gouy doesn't like to see any compiler flags embedded in the source file, like `{-# LANGUAGE BangPatterns #-}`. He prefers them on the command line like `ghc --make -XBangPatterns`.

Published benchmark program measurements are best out of 6. See [details about how programs are measured](http://benchmarksgame.alioth.debian.org/how-programs-are-measured.html) page.

By substituting `u64` for `u64q` in the benchmarks game website urls, you can see the [shadow website with the 64-bit one-core benchmark results](http://benchmarksgame.alioth.debian.org/u64). It looks like Isaac stopped maintaining the one-core benchmarks in October 2015?

I guess the [32-bit one-core benchmarks](http://benchmarksgame.alioth.debian.org/u32) are still being maintained?

## FAQ

#### Can I fork this repo and submit a faster Haskell benchmark program to Alioth and then merge my work back here?

Yes please!

#### Can I fork this repo and submit a faster Haskell benchmark program to Alioth without merging back here?

Yeah, go for it. We can always copy-paste your code back here from the website. We just want to see Haskell ranked in its rightful place on the Benchmarks Game website: tied for #1 with Rust.

#### Is this a game or like, real?

Bad software has consequences. Good languages produce good software. In the endless quarrel over which languages are good, the Alioth website is the sturdiest point of reference.

#### Isn't it true that all computer programming languages are good, you just have to pick the right tool for the job?

[No.](http://www.thocp.net/biographies/papers/backus_turingaward_lecture.pdf)
