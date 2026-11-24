## Regression tests for issue #27: jags() and jags.parallel() must let the
## user control JAGS 5's 'initrep' (prior-based automatic initialization),
## which rjags::jags.model() gained as an argument.

## 'initrep' only exists in rjags >= 5 / JAGS >= 5.0.0; older rjags on the
## check farm must not fail these tests.
skip_if_no_initrep <- function() {
  testthat::skip_if_not_installed("rjags")
  if (!"initrep" %in% names(formals(rjags::jags.model))) {
    testthat::skip("installed rjags::jags.model() has no 'initrep' argument")
  }
}

## Replace the jags.model binding inside R2jags's imports environment with a
## spy that records the call and delegates to the real function. This is the
## binding jags() actually resolves 'jags.model' through (NAMESPACE has
## importFrom(rjags, "jags.model")), so it captures exactly what jags()
## forwards, without touching the rjags namespace itself.
with_jags.model_spy <- function(code) {
  imports <- parent.env(asNamespace("R2jags")) # the 'imports:R2jags' env
  original <- imports$jags.model
  seen <- new.env(parent = emptyenv())
  seen$args <- NULL
  spy <- function(...) {
    seen$args <- list(...)
    original(...)
  }
  unlockBinding("jags.model", imports)
  on.exit({
    assign("jags.model", original, envir = imports)
    lockBinding("jags.model", imports)
  }, add = TRUE)
  assign("jags.model", spy, envir = imports)
  code(seen)
}

schools_args <- function(...) {
  model.file <- system.file(package = "R2jags", "model", "schools.txt")
  args <- list(
    data = list(
      y = c(28.4, 7.9, -2.8, 6.8, -0.6, 0.6, 18.0, 12.2),
      sd = c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6),
      J = 8.0
    ),
    parameters.to.save = c("mu", "sigma", "theta"),
    model.file = model.file,
    n.chains = 2,
    n.iter = 50,
    progress.bar = "none",
    quiet = TRUE,
    ...
  )
  args
}

test_that("jags() forwards initrep to rjags::jags.model() (#27)", {
  skip_if_no_initrep()
  args <- schools_args(initrep = 37)
  seen <- with_jags.model_spy(function(seen) {
    suppressWarnings(suppressMessages(do.call(jags, args)))
    seen
  })
  expect_identical(seen$args$initrep, 37L)
})

test_that("jags() supports expert-mode length-2 initrep (#27)", {
  skip_if_no_initrep()
  args <- schools_args(initrep = c(10, 5))
  seen <- with_jags.model_spy(function(seen) {
    suppressWarnings(suppressMessages(do.call(jags, args)))
    seen
  })
  expect_identical(seen$args$initrep, c(10L, 5L))
})

test_that("jags() does not pass initrep by default (#27)", {
  skip_if_no_initrep()
  ## The default must leave rjags in charge of the value, and must not break
  ## an rjags that has no 'initrep' argument at all.
  args <- schools_args()
  seen <- with_jags.model_spy(function(seen) {
    suppressWarnings(suppressMessages(do.call(jags, args)))
    seen
  })
  expect_null(seen$args$initrep)
})

test_that("initrep = 1 runs end to end and returns samples (#27)", {
  skip_if_no_initrep()
  fit <- suppressWarnings(suppressMessages(do.call(jags, schools_args(initrep = 1))))
  expect_true(inherits(fit, "rjags"))
  expect_true(all(c("model", "BUGSoutput") %in% names(fit)))
  expect_gt(nrow(fit$BUGSoutput$sims.list$mu), 0)
})

test_that("invalid initrep is rejected before the model is compiled (#27)", {
  skip_if_no_initrep()
  for (bad in list(0, -1, c(1, 2, 3), "a", NA, NaN, Inf)) {
    args <- schools_args(initrep = bad)
    expect_error(suppressWarnings(suppressMessages(do.call(jags, args))),
                 "'initrep'", info = paste(format(bad), collapse = "/"))
  }
})

## 'n.threads' is the other rjags 5 / JAGS 5 argument surfaced by issue #27.

skip_if_no_n_threads <- function() {
  testthat::skip_if_not_installed("rjags")
  if (!"n.threads" %in% names(formals(rjags::jags.model))) {
    testthat::skip("installed rjags::jags.model() has no 'n.threads' argument")
  }
}

test_that("jags() forwards n.threads to rjags::jags.model()", {
  skip_if_no_n_threads()
  args <- schools_args(n.threads = 2)
  seen <- with_jags.model_spy(function(seen) {
    suppressWarnings(suppressMessages(do.call(jags, args)))
    seen
  })
  expect_identical(seen$args$n.threads, 2L)
})

test_that("jags() does not pass n.threads by default", {
  skip_if_no_n_threads()
  ## NULL must leave rjags in charge (its default is n.threads = n.chains),
  ## and must not break an rjags that has no 'n.threads' argument at all.
  args <- schools_args()
  seen <- with_jags.model_spy(function(seen) {
    suppressWarnings(suppressMessages(do.call(jags, args)))
    seen
  })
  expect_null(seen$args$n.threads)
})

test_that("n.threads = 2 runs end to end and returns samples", {
  skip_if_no_n_threads()
  fit <- suppressWarnings(suppressMessages(
    do.call(jags, schools_args(n.threads = 2))))
  expect_true(inherits(fit, "rjags"))
  expect_gt(nrow(fit$BUGSoutput$sims.list$mu), 0)
})

test_that("invalid n.threads is rejected before the model is compiled", {
  skip_if_no_n_threads()
  for (bad in list(0, -1, c(1, 2), "a", NA, NaN, Inf)) {
    args <- schools_args(n.threads = bad)
    expect_error(suppressWarnings(suppressMessages(do.call(jags, args))),
                 "'n.threads'", info = paste(format(bad), collapse = "/"))
  }
})

test_that("jags.parallel() validates n.threads before spawning workers", {
  ## Rejection happens before makeCluster(), so this needs no JAGS at all.
  for (bad in list(0, -1, c(1, 2), "a", NA, NaN, Inf)) {
    expect_error(
      suppressWarnings(suppressMessages(
        jags.parallel(data = list(J = 8.0),
                      inits = function() list(mu = 0),
                      parameters.to.save = "mu",
                      model.file = system.file(package = "R2jags", "model",
                                               "schools.txt"),
                      n.threads = bad))),
      "'n.threads'", info = paste(format(bad), collapse = "/"))
  }
})
