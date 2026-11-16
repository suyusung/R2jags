test_that("pD and DIC2 survive a penalty of exactly 0 (#24)", {
  # A saturated likelihood: the deviance cannot vary, so rjags::dic.samples()
  # returns a penalty of exactly 0.  jags() overwrote the logical `pD` argument
  # with that number, and the final `if (pD)` then read the 0 as "pD was never
  # computed", so pD and DIC2 were missing from the output object and the print
  # methods had nothing to show.
  model <- "model {
    for (i in 1:n) {
      y[i] ~ dbern(1)
    }
  }"
  model_file <- tempfile(fileext = ".bug")
  writeLines(model, model_file)

  capture.output(
    out <- jags(list(n = 5, y = rep(1, 5)), parameters.to.save = "y",
                model.file = model_file, n.chains = 2, n.iter = 100,
                n.burnin = 10, DIC = TRUE, pD = TRUE, n.iter.pd = 100,
                jags.module = character(0))
  )

  expect_true("pD" %in% names(out$BUGSoutput))
  expect_true("DIC2" %in% names(out$BUGSoutput))
  expect_equal(out$BUGSoutput$pD, 0)
  expect_true(is.finite(out$BUGSoutput$DIC2))

  # the print method runs over that object, not over a list missing the members
  expect_silent(capture.output(print(out)))
})

test_that("pD is still absent when it was not asked for (#24)", {
  model <- "model {
    for (i in 1:n) {
      y[i] ~ dnorm(mu, 1)
    }
    mu ~ dnorm(0, 1)
  }"
  model_file <- tempfile(fileext = ".bug")
  writeLines(model, model_file)

  capture.output(
    out <- jags(list(n = 20, y = rnorm(20)), parameters.to.save = "mu",
                model.file = model_file, n.chains = 2, n.iter = 100,
                n.burnin = 10, DIC = TRUE, jags.module = character(0))
  )

  expect_false("pD" %in% names(out$BUGSoutput))
  expect_false("DIC2" %in% names(out$BUGSoutput))
})
