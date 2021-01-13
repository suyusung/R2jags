test_that("printing jags() output does not error if DIC is FALSE (#7)", {
  model <- "model {
    for (i in 1:n) {
      y[i] ~ dnorm(x[i] * beta, 1)
    }
    beta ~ dnorm(0, 1)
  }"
  tmp <- tempfile()
  writeLines(model, tmp)
  data <- list(
    n = 10,
    x = rnorm(10),
    y = rnorm(10)
  )
  library(coda)
  tmp <- utils::capture.output(
    out <- jags(
      data,
      parameters.to.save = "beta",
      model.file = tmp,
      n.chains = 4,
      DIC = FALSE,
      jags.module = character(0)
    )
  )
  expect_silent(tmp <- utils::capture.output(print(out)))
  expect_true(inherits(out, "rjags"))
})
