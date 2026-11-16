# R2jags 0.9-0 (dev)

_November 2026_

* `jags(pD = TRUE)` keeps `pD` and `DIC2` in its output when the penalty is exactly 0: the logical argument was overwritten by the computed penalty, and `if (pD)` then read a 0 as "not computed". (#24)

* The warning raised when a parameter's saved values are not the whole array now says so in those words instead of "error/missing in parameter", because saving part of an array -- the upper triangle of a matrix, say -- is a legitimate request. (#20)

* `count_nodes()` compares classes with `inherits()`, and the help page link to `R2WinBUGS::write.model` carries its package anchor.

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