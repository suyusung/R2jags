test_that("saving part of an array warns informatively, not alarmingly (#20)", {
  # Network meta-analysis models routinely save only the upper triangle of a
  # contrast matrix.  That is a legitimate request, but it used to produce
  # "error/missing in parameter ... Be aware of the output results", which reads
  # as if something had gone wrong.
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

  # it still tells you, naming the parameter and what was saved ...
  expect_true(length(warnings) >= 1)
  expect_true(any(grepl("elements of the array 'or'", warnings)))
  expect_true(any(grepl("cover the elements you named", warnings)))
  # ... says it is not an error ...
  expect_true(any(grepl("not an error", warnings)))
  # ... and no longer uses the wording the issue was filed about
  expect_false(any(grepl("error/missing", warnings)))

  # and the elements that were asked for are the ones summarised
  expect_true(all(c("d[1]", "d[2]", "d[3]", "or[1,2]", "or[1,3]", "or[2,3]") %in%
                    rownames(out$BUGSoutput$summary)))
})
