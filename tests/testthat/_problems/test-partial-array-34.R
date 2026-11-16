# Extracted from test-partial-array.R:34

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "R2jags", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
model <- "model {
    for (i in 1:3) {
      d[i] ~ dnorm(0, 0.001)
    }
    for (i in 1:2) {
      for (j in (i+1):3) {
        or[i,j] <- exp(d[j] - d[i])
      }
    }
  }"
model_file <- tempfile(fileext = ".bug")
writeLines(model, model_file)
warnings <- character()
capture.output(
    out <- withCallingHandlers(
      jags(list(), parameters.to.save = c("d", "or[1,2]", "or[1,3]", "or[2,3]"),
           model.file = model_file, n.chains = 2, n.iter = 50, n.burnin = 5,
           DIC = FALSE, jags.module = character(0)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
  )
expect_true(length(warnings) >= 1)
expect_true(any(grepl("or", warnings)))
expect_true(any(grepl("3 of the 9 elements", warnings)))
