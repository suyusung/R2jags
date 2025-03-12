.onLoad <- function(lib, pkg)
{
  ## Set global defaults
  if (is.null(getOption("r2j.pb"))) {
    options("r2j.pb"="text")
  }
  if (is.null(getOption("r2j.quiet"))) {
    options("r2j.quiet"=FALSE)
  }  
  if (is.null(getOption("r2j.print.program"))) {
    options("r2j.print.program"=TRUE)
  }  
}

#' @title .onAttach
#' @description prints out a friendly reminder message to the user
#' @inheritParams base .onAttach
#' @return NULL
#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The R2jags version loaded is: ", utils::packageVersion("R2jags"))
}