## The convergence summary the package computes itself since 0.9-0 (issue #3).
##
## Until then these two functions were imported from R2WinBUGS, and the numbers
## below are the ones it produced.  What the package computes now was checked
## against R2WinBUGS::monitor() -- 48 combinations of five draw sets (multi-chain
## mixed signs, all positive, single chain, proportions, degenerate) with every
## keep.all, Rupper.keep and trans setting -- and every comparison was
## bit-identical at the time, with the same errors on the same out-of-domain
## inputs. These tests pin the numbers so that a future change cannot move them
## silently. The numeric comparisons use a relative tolerance of 1e-12 rather
## than exact identity: quantile() results move by one ulp between platforms
## (FMA contraction on arm64 versus x86-64), far below any change that could
## matter statistically, and that drift is not a regression.

test_that("monitor() summarises the kept draws exactly as it always has (#3)", {
  set.seed(1)
  a <- array(rnorm(100 * 3 * 2), c(100, 3, 2))
  dimnames(a)[[3]] <- c("alpha", "beta")
  s <- R2jags:::monitor(a, 3, keep.all = TRUE)

  expect_identical(dim(s), c(2L, 9L))
  expect_identical(colnames(s),
                   c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%", "Rhat", "n.eff"))
  expect_identical(rownames(s), c("alpha", "beta"))

  expect_equal(unname(s["alpha", ]),
               c(0.033584275126047752, 0.963695897185394390, -1.862394047205248926,
                 -0.589051101241765851, -0.038437087100108583, 0.672734465686813943,
                 2.030193581332730979, 1.001970061720514948, 300),
               tolerance = 1e-12)
  expect_equal(unname(s["beta", ]),
               c(-0.010683912860323479, 1.043838678256150621, -2.122223535529929705,
                 -0.698227738514206608, -0.041826297550518803, 0.679773061311617965,
                 1.891769126525841394, 1.004391902846560880, 300),
               tolerance = 1e-12)
})

test_that("one chain gets the seven-column form (#3)", {
  set.seed(2)
  b <- array(rnorm(50 * 1 * 1), c(50, 1, 1))
  dimnames(b)[[3]] <- "gamma"
  s <- R2jags:::monitor(b, 1, keep.all = TRUE)

  expect_identical(dim(s), c(1L, 7L))
  expect_identical(colnames(s),
                   c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%"))
  expect_equal(unname(s["gamma", ]),
               c(0.069137914357987940, 1.129682364802339523, -2.231876754981608091,
                 -0.596333751146496560, -0.037656989861423344, 0.778887103665268254,
                 1.989469973321651874),
               tolerance = 1e-12)
})

test_that("a parameter whose draws never vary does not produce NaN (#3)", {
  d <- array(rep(7, 100 * 3 * 1), c(100, 3, 1))
  dimnames(d)[[3]] <- "const"
  s <- R2jags:::monitor(d, 3, keep.all = TRUE)

  expect_identical(unname(s["const", "mean"]), 7)
  expect_identical(unname(s["const", "sd"]), 0)
  ## all positive draws are summarised through the log scale and transformed
  ## back, so the quantiles come back to within a rounding step of 7
  expect_equal(unname(s["const", c("2.5%", "97.5%")]), c(7, 7))
  expect_identical(unname(s["const", "Rhat"]), 1)     # the degenerate case
  expect_identical(unname(s["const", "n.eff"]), 1)
})

test_that("R2WinBUGS is no longer one of the package's dependencies (#3)", {
  imports <- packageDescription("R2jags")$Imports
  depends <- packageDescription("R2jags")$Depends
  expect_false(!is.null(imports) && grepl("R2WinBUGS", imports))
  expect_false(!is.null(depends) && grepl("R2WinBUGS", depends))
  ## and the functions it used to supply are ours now, defined in this namespace
  ns <- asNamespace("R2jags")
  for (f in c("monitor", "conv.par", "write.model", "attach.bugs", "detach.bugs"))
    expect_true(exists(f, envir = ns, inherits = FALSE), info = f)
})
