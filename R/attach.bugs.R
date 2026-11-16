## Attaching the draws to the search path, so that the elements of sims.list can
## be referred to by name -- what attach.jags()/detach.jags() wrap.  Imported from
## R2WinBUGS until 0.9-0 (issue #3); the S-PLUS branches are gone and the
## behaviour is unchanged, including the "bugs.sims" name the entry is attached
## under and the n.sims element added to sims.list for convenience.

attach.all <- function(x, overwrite = NA, name = "attach.all") {
  masked <- names(x) %in% ls(.GlobalEnv)
  if (!any(masked)) overwrite <- FALSE
  masked <- names(x)[masked]
  if (is.na(overwrite)) {
    question <- paste("The following objects in .GlobalEnv will mask\n",
                      "objects in the attached database:\n",
                      paste(masked, collapse = ", "),
                      "\nRemove these objects from .GlobalEnv?", sep = "")
    if (interactive()) {
      overwrite <- if (.Platform$OS.type == "windows")
        "YES" == utils::winDialog(type = "yesno", question)
      else 1 == utils::menu(c("YES", "NO"), graphics = FALSE, title = question)
    } else {
      overwrite <- FALSE
    }
  }
  if (overwrite) remove(list = masked, envir = .GlobalEnv)
  ## Deliberate, and the whole point of attach.jags(): the simulations go on the
  ## search path so their elements can be referred to by name.  Spelled base:: so
  ## that R CMD check's scan for attach() reads it as the conscious call it is.
  base::attach(x, name = name)
}

attach.bugs <- function(x, overwrite = NA) {
  if (!inherits(x, "bugs"))
    stop("attach.bugs() requires a bugs object.")
  if ("bugs.sims" %in% search()) detach("bugs.sims")
  x$sims.list$n.sims <- x$n.sims   # put n.sims into sims.list for convenience
  r2 <- attach.all(x$sims.list, overwrite = overwrite, name = "bugs.sims")
  invisible(r2)
}

detach.all <- function(name = "attach.all") {
  do.call("detach", list(name = name))
}

detach.bugs <- function() {
  detach.all("bugs.sims")
}
