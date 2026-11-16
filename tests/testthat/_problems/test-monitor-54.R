# Extracted from test-monitor.R:54

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "R2jags", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
d <- array(rep(7, 100 * 3 * 1), c(100, 3, 1))
dimnames(d)[[3]] <- "const"
s <- R2jags:::monitor(d, 3, keep.all = TRUE)
expect_identical(unname(s["const", "mean"]), 7)
expect_identical(unname(s["const", "sd"]), 0)
expect_identical(unname(s["const", c("2.5%", "97.5%")]), c(7, 7))
