## Regression tests for the reverse-dependency pretest failure: dropping the
## R2WinBUGS dependency removed the globally registered S3 methods for class
## "bugs", so coda::as.mcmc.list(fit$BUGSoutput) (used by mvnma's examples)
## fell through to as.mcmc.list.default and died with "Arguments must be mcmc
## objects". R2jags must register as.mcmc.list.bugs (and print.bugs) itself.

schools_args <- function(...) {
  model.file <- system.file(package = "R2jags", "model", "schools.txt")
  c(list(
    data = list(
      y = c(28.4, 7.9, -2.8, 6.8, -0.6, 0.6, 18.0, 12.2),
      sd = c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6),
      J = 8.0
    ),
    inits = function() list(mu = rnorm(1), sigma = runif(1), theta = rnorm(8)),
    parameters.to.save = c("mu", "sigma"),
    n.chains = 2,
    n.iter = 100,
    n.burnin = 25,
    model.file = model.file,
    progress.bar = "none",
    quiet = TRUE
  ), list(...))
}

test_that("the bugs S3 methods are registered by R2jags (#3, CRAN pretest)", {
  m <- utils::getS3method("as.mcmc.list", "bugs", optional = TRUE)
  expect_false(is.null(m))
  expect_identical(environmentName(environment(m)), "R2jags")
  p <- utils::getS3method("print", "bugs", optional = TRUE)
  expect_false(is.null(p))
  expect_identical(environmentName(environment(p)), "R2jags")
})

test_that("as.mcmc.list(fit$BUGSoutput) gives an mcmc.list (mvnma use case)", {
  skip_if_not_installed("rjags")
  fit <- suppressWarnings(suppressMessages(do.call(jags, schools_args())))
  b <- fit$BUGSoutput
  expect_identical(class(b), "bugs")

  ml <- coda::as.mcmc.list(b)
  expect_true(inherits(ml, "mcmc.list"))
  expect_identical(coda::nchain(ml), 2L)
  expect_equal(coda::niter(ml), b$n.sims / b$n.chains)
  expect_identical(dimnames(ml[[1]])[[2]], sort(dimnames(b$sims.array)[[3]]))
  expect_equal(attr(ml[[1]], "mcpar"), c(b$n.burnin + 1, b$n.iter, b$n.thin))
})

test_that("as.mcmc.list.bugs rejects non-bugs objects", {
  expect_error(R2jags:::as.mcmc.list.bugs(list()),
               "only intended for bugs objects")
})

test_that("print on a bugs object shows the scheme, summary and DIC block", {
  skip_if_not_installed("rjags")
  fit <- suppressWarnings(suppressMessages(do.call(jags, schools_args())))
  out <- capture.output(print(fit$BUGSoutput))
  expect_true(any(grepl("chains, each with", out, fixed = TRUE)))
  expect_true(any(grepl("n.sims", out, fixed = TRUE)))
  expect_true(any(grepl("DIC", out, fixed = TRUE)))
})

test_that("print shows the dic.samples pD line when pD was computed", {
  skip_if_not_installed("rjags")
  fit <- suppressWarnings(suppressMessages(do.call(jags, schools_args(pD = TRUE))))
  out <- capture.output(print(fit$BUGSoutput))
  expect_true(any(grepl("dic.samples", out, fixed = TRUE)))
  expect_true(any(grepl("pD =", out, fixed = TRUE)))
})
