## Writing a model that was given as an R function (issue #3: this used to come
## from R2WinBUGS::write.model).  The expected text below is what R2WinBUGS
## produces for the same function -- the two writers were compared on seven
## models, including one long enough that deparse() breaks the line, and their
## files are byte-identical -- and it pins the part that matters: the
## scientific-notation constants are rewritten in the form the JAGS parser
## accepts, while 1e-3 and an ordinary number are left alone.

test_that("a model given as an R function is written unchanged (#3)", {
  model <- function() {
    for (i in 1:n) {
      y[i] ~ dnorm(mu, 1e-6)
    }
    mu ~ dnorm(0, 1e-5)
    sigma <- 1e5
    a <- 1e-3
    b <- 1234.5678
  }
  f <- tempfile()
  R2jags:::write.model(model, con = f, digits = 5)

  expect_identical(readLines(f, warn = FALSE),
    c("model",
      "{",
      "    for (i in 1:n) {",
      "        y[i] ~ dnorm(mu, 1.00000E-06)",
      "    }",
      "    mu ~ dnorm(0.00000E+00, 1.00000E-05)",
      "    sigma <- 1.00000E+05",
      "    a <- 0.001",
      "    b <- 1234.5678",
      "}"))
})

test_that("the %_% spacer writes a space in a model (#3)", {
  model <- function() {
    alpha ~ dnorm(0, 0.001 %_% 1)
    beta ~ dnorm(1.5, 2.5)
  }
  f <- tempfile()
  R2jags:::write.model(model, con = f, digits = 5)
  txt <- readLines(f, warn = FALSE)
  ## removing the spacer leaves the two spaces that surrounded it, and 0 is
  ## rewritten in scientific notation like any other small constant
  expect_true(any(grepl("alpha ~ dnorm(0.00000E+00, 0.001  1)", txt, fixed = TRUE)))
  expect_false(any(grepl("%_%", txt, fixed = TRUE)))
})

test_that("jags() takes a model supplied as an R function (#3)", {
  ## the path through jags() that needs write.model()
  model <- function() {
    for (i in 1:n) {
      y[i] ~ dnorm(x[i] * beta, 1)
    }
    beta ~ dnorm(0, 1)
  }
  set.seed(4)
  capture.output(
    out <- jags(list(n = 10, x = rnorm(10), y = rnorm(10)),
                parameters.to.save = "beta", model.file = model,
                n.chains = 2, n.iter = 100, n.burnin = 10, DIC = TRUE,
                jags.module = character(0), progress.bar = "none", quiet = TRUE)
  )
  expect_true("beta" %in% rownames(out$BUGSoutput$summary))
  expect_true(file.exists(out$model.file))
  expect_identical(readLines(out$model.file, warn = FALSE)[1], "model")
})
