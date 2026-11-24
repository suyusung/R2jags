# R2jags 0.9-0 (dev)

_November 2026_

* `jags()` and `jags.parallel()` gained an `initrep` argument, passed on to `rjags::jags.model()`: the number of replicate samples JAGS 5 draws from the prior when it initializes unobserved stochastic nodes automatically (#27). `initrep = 1` draws the initial values themselves from the prior; a vector of length 2 activates JAGS's expert mode. The default `NULL` does not pass the argument at all, leaving the choice to `rjags` and keeping installations on rjags 4-x working untouched.

* `jags()` and `jags.parallel()` gained an `n.threads` argument, passed on to `rjags::jags.model()`: the number of threads JAGS 5 uses for sampling, parallelizing the sampler manager across the chains of one compiled model (#27). It is a different mechanism from `jags.parallel()`, which runs each chain in its own R process. The default `NULL` does not pass the argument, leaving rjags's own default (one thread per chain) in place.

* `jags(pD = TRUE)` keeps `pD` and `DIC2` in its output when the penalty is exactly 0: the logical argument was overwritten by the computed penalty, and `if (pD)` then read a 0 as "not computed". (#24)

* The warning raised when a parameter's saved values are not the whole array now says so in those words instead of "error/missing in parameter", because saving part of an array -- the upper triangle of a matrix, say -- is a legitimate request. (#20)

* `count_nodes()` compares classes with `inherits()`.

* The functions that replace `R2WinBUGS`'s -- `monitor()`/`conv.par()`, `write.model()`, `attach.bugs()`/`detach.bugs()` -- are written as this package's own code rather than as a transcription of it, and are measured against it: bit-identical across 48 combinations of draw sets and settings, byte-identical model files, same attached names.

* `R2WinBUGS` is no longer a dependency. The four functions that came from it -- `monitor()` (the convergence summary behind every table), `write.model()`, `attach.bugs()` and `detach.bugs()` -- are now part of this package, with the S-PLUS branches dropped. Nothing else moved: `monitor()` is bit-identical to `R2WinBUGS::monitor()` across five draw sets and every combination of `keep.all`, `Rupper.keep` and `trans`, `write.model()` writes byte-identical files, and a seeded `jags()` call returns an identical summary. (#3)

_Octoer 2025_

* Add `NEWS.md`, `README.Rmd` (which expands and completes the previous `README.md`), `CONTRIBUTING.md` and `CONDUCT.md`.

_June 2024_

* Fix `print` table to also show the running time. (806f3c4)

* Change defaults for the option `progress.bar` to "none"" and for `quiet` to TRUE. This helps when running `R2jags` in the background (eg for `Rmarkdown` or `qmd`). (5e6cb5c)

* Add adaptation after model compilation, otherwise it would happen automatically at the point of sampling and would throw an annoying message. This way, it still allows for 
proper burnin, but also does an arbitrarily short/long adaptation phase. (27424a8)

_July 2024_

* Change in `jags.R` to allow for the option `burnin=0`. (00b7478)

_Mar 2024_

* Start `devel` branch by adding the `count_nodes` function. (fb0c898)