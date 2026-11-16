## Write a JAGS model that the user has given as an R function, the way
## R2WinBUGS::write.model does and as ?jags documents for the model.file
## argument.  Imported from R2WinBUGS until 0.9-0 (issue #3); the S-PLUS branch
## is gone and the output is unchanged -- tests/testthat/test-write-model.R
## writes the same models through both and compares the files.

write.model <- function(model, con = "model.bug", digits = 5) {
  model.text <- c("model", replaceScientificNotationR(body(model), digits = digits))
  ## "a %_% b" is the R2WinBUGS way of writing "a b" in a model, which the
  ## R parser would not otherwise accept.
  model.text <- gsub("%_%", "", model.text)
  writeLines(model.text, con = con)
}


## Rewrite the numeric constants of a model body in the notation the JAGS parser
## accepts: R deparses a small or large number as 1e-05, and the model language
## wants 1.0E-5.  Each constant is replaced by a placeholder name first, the body
## is deparsed whole, and the placeholders are then substituted back, so that
## deparse() cannot undo the formatting.
replaceScientificNotationR <- function(bmodel, digits = 5) {
  env <- new.env()
  assign("rSNRidCounter", 0, envir = env)

  replaceID <- function(bmodel, env, digits = 5) {
    for (i in seq_along(bmodel)) {
      if (length(bmodel[[i]]) == 1) {
        ## ':', '[' and '[[' keep their meaning: only numbers are formatted.
        if (as.character(bmodel[[i]]) %in% c(":", "[", "[[")) return(bmodel)
        if ((typeof(bmodel[[i]]) %in% c("double", "integer")) &&
            ((abs(bmodel[[i]]) < 1e-3) || (abs(bmodel[[i]]) > 1e+4))) {
          counter <- get("rSNRidCounter", envir = env) + 1
          assign("rSNRidCounter", counter, envir = env)
          id <- paste("rSNRid", counter, sep = "")
          assign(id, formatC(bmodel[[i]], digits = digits, format = "E"), envir = env)
          bmodel[[i]] <- id
        }
      } else {
        bmodel[[i]] <- replaceID(bmodel[[i]], env, digits = digits)
      }
    }
    bmodel
  }

  bmodel <- deparse(replaceID(bmodel, env, digits = digits), control = NULL)
  for (i in ls(env)) {
    bmodel <- gsub(paste('"', i, '"', sep = ''), get(i, envir = env), bmodel, fixed = TRUE)
  }
  bmodel
}
