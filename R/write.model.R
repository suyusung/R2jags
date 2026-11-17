## Write a JAGS model that the user has given as an R function, the form ?jags
## documents for the model.file argument.  The model language wants a small or
## large constant written as 1.00000E-06 where R writes 1e-06, so the body is
## rewritten on its way out.  What is written is what R2WinBUGS::write.model
## writes, and tests/testthat/test-write-model.R pins it.

write.model <- function(model, con = "model.bug", digits = 5) {
  ## "a %_% b" is how a model says "a b": dropping the spacer leaves a space
  ## where the R parser would otherwise not allow one.
  writeLines(gsub("%_%", "", c("model", model.lines(body(model), digits = digits))),
             con = con)
}


## The body of a model function as character lines, with every numeric constant
## that is very small or very large already in E notation.
##
## The constants cannot simply be formatted where they are: deparse() prints a
## double the way R writes numbers, whatever form it was handed, so a formatted
## constant would come back as 1e-06.  Each one is therefore parked under a short
## name, the body is deparsed whole, and the parked text is substituted back.
## Short names matter: deparse() decides where to break lines from the width of
## what it prints, and long placeholders would move the breaks.
model.lines <- function(expr, digits = 5) {
  parked <- new.env(parent = emptyenv())
  park <- function(value) {
    ## Six characters and the counter, no more: deparse() breaks lines by the
    ## width of what it prints, and the placeholders decide where the breaks fall
    ## in the text that gets written.
    id <- paste0("R2jags", length(ls(parked)) + 1L)
    assign(id, formatC(value, digits = digits, format = "E"), envir = parked)
    id      # a string, so deparse() prints it quoted and it can be found again
  }
  needs.e.notation <- function(value) abs(value) < 1e-3 || abs(value) > 1e4

  rewrite <- function(expr) {
    if (is.numeric(expr) && length(expr) == 1L) {
      if (needs.e.notation(expr)) return(park(expr))
      return(expr)
    }
    if (is.call(expr)) {
      ## an index is left exactly as it was written
      if (as.character(expr[[1L]]) %in% c("[", "[[", ":")) return(expr)
      for (i in seq_along(expr)) expr[[i]] <- rewrite(expr[[i]])
    }
    expr
  }

  lines <- deparse(rewrite(expr), control = NULL)
  for (id in ls(parked))
    lines <- gsub(paste0('"', id, '"'), get(id, envir = parked), lines, fixed = TRUE)
  lines
}
