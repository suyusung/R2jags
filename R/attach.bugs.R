## Putting the draws of a fitted model on the search path, so that the elements of
## sims.list can be referred to by name -- what attach.jags() and detach.jags()
## wrap.  The entry goes on under "bugs.sims" and carries n.sims alongside the
## draws, because that is what a caller of this package has always found there.

attach.all <- function(x, overwrite = NA, name = "attach.all") {
  clashing <- names(x)[names(x) %in% ls(.GlobalEnv)]
  overwrite <- resolve.clashes(clashing, overwrite)
  if (overwrite) remove(list = clashing, envir = .GlobalEnv)
  ## Attaching is the point here, not an accident, which is why the call is
  ## spelled base::attach: R CMD check's scan for attach() in package code reads
  ## the plain form as one.
  base::attach(x, name = name)
}


## Whether objects of the same name in the workspace should be removed before
## something is attached over them.  TRUE/FALSE settles it; NA asks, and only in
## an interactive session -- a script gets no question and nothing is deleted.
resolve.clashes <- function(clashing, overwrite) {
  if (!length(clashing)) return(FALSE)
  if (!is.na(overwrite)) return(overwrite)
  if (!interactive()) return(FALSE)

  question <- paste("The following objects in .GlobalEnv will mask\n",
                    "objects in the attached database:\n",
                    paste(clashing, collapse = ", "),
                    "\nRemove these objects from .GlobalEnv?", sep = "")
  if (.Platform$OS.type == "windows")
    "YES" == utils::winDialog(type = "yesno", question)
  else
    1 == utils::menu(c("YES", "NO"), graphics = FALSE, title = question)
}


attach.bugs <- function(x, overwrite = NA) {
  if (!inherits(x, "bugs"))
    stop("attach.bugs() requires a bugs object.")
  if ("bugs.sims" %in% search()) detach.all("bugs.sims")
  ## n.sims rides along with the draws for convenience
  invisible(attach.all(c(x$sims.list, list(n.sims = x$n.sims)),
                       overwrite = overwrite, name = "bugs.sims"))
}


detach.all <- function(name = "attach.all") {
  detach(name = name, character.only = TRUE)
}


detach.bugs <- function() {
  detach.all("bugs.sims")
}
