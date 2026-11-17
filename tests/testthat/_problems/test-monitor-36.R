# Extracted from test-monitor.R:36

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "R2jags", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
set.seed(2)
b <- array(rnorm(50 * 1 * 1), c(50, 1, 1))
dimnames(b)[[3]] <- "gamma"
s <- R2jags:::monitor(b, 1, keep.all = TRUE)
