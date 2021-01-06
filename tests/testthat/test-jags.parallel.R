test_that("fewer cores than chains", {
  model <- "model {
    for (i in 1:n) {
      y[i] ~ dnorm(x[i] * beta, 1)
    }
    beta ~ dnorm(0, 1)
  }"
  model_file <- tempfile()
  writeLines(model, model_file)
  data <- list(
    n = 10,
    x = rnorm(10),
    y = rnorm(10)
  )
  tmp <- capture.output(
    out <- jags.parallel(
      data,
      parameters.to.save = "beta",
      model.file = model_file,
     n.chains = 4,
     n.cluster = 2
    )
  )
  expect_true(inherits(out, "rjags"))
  expect_true(inherits(out$BUGSoutput, "bugs"))
})
