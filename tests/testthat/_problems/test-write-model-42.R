# Extracted from test-write-model.R:42

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "R2jags", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
model <- function() {
    alpha ~ dnorm(0, 0.001 %_% 1)
    beta ~ dnorm(1.5, 2.5)
  }
f <- tempfile()
R2jags:::write.model(model, con = f, digits = 5)
txt <- readLines(f, warn = FALSE)
expect_true(any(grepl("0.001 1", txt, fixed = TRUE)))
