##' Attach/detach elements of \sQuote{JAGS} objects to search path
##' 
##' These are wraper functions for \code{\link[R2WinBUGS]{attach.bugs}} and
##' \code{\link[R2WinBUGS]{detach.bugs}}, which attach or detach
##' three-way-simulation array of bugs object to the search path. See
##' \code{\link[R2WinBUGS]{attach.all}} for details.
##' 
##' See \code{\link[R2WinBUGS]{attach.bugs}} for details
##' 
##' @aliases attach.jags detach.jags
##' @param x An \code{rjags} object.
##' @param overwrite If \code{TRUE}, objects with identical names in the
##' Workspace (.GlobalEnv) that are masking objects in the database to be
##' attached will be deleted.  If \code{NA} (the default) and an interactive
##' session is running, a dialog box asks the user whether masking objects
##' should be deleted.  In non-interactive mode, behaviour is identical to
##' \code{overwrite=FALSE}, i.e. nothing will be deleted.
##' @author Yu-Sung Su \email{suyusung@@tsinghua.edu.cn},
##' @references Sibylle Sturtz and Uwe Ligges and Andrew Gelman. (2005).
##' \dQuote{R2WinBUGS: A Package for Running WinBUGS from R.} \emph{Journal of
##' Statistical Software} 3 (12): 1--6.
##' @keywords interface
##' @examples
##' 
##' 
##'   # See the example in ?jags for the usage.
##' 
##' 
attach.jags <- function(x, overwrite = NA){
  r2 <- attach.bugs(x$BUGSoutput, overwrite = overwrite)
  invisible(r2)
}

detach.jags <- function(){
  detach.bugs()
}
