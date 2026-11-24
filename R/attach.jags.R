##' Attach/detach elements of \sQuote{JAGS} objects to search path
##' 
##' These attach or detach the three-way-simulation array of a bugs object to the
##' search path, so that the elements of \code{sims.list} can be referred to by
##' name.  They do what \code{R2WinBUGS::attach.bugs} and
##' \code{R2WinBUGS::detach.bugs} do, and until 0.9-0 the package carried a
##' dependency on \pkg{R2WinBUGS} for their sake alone (issue #3).
##' 
##' See the examples in \code{\link{jags}} for the usage.
##' 
##' @aliases attach.jags detach.jags
##' @param x An \code{rjags} object.
##' @param overwrite If \code{TRUE}, objects with identical names in the
##' Workspace (.GlobalEnv) that are masking objects in the database to be
##' attached will be deleted.  If \code{NA} (the default) and an interactive
##' session is running, a dialog box asks the user whether masking objects
##' should be deleted.  In non-interactive mode, behaviour is identical to
##' \code{overwrite=FALSE}, i.e. nothing will be deleted.
##' @author Yu-Sung Su \email{suyusung@@tsinghua.edu.cn}
##' @references Sibylle Sturtz and Uwe Ligges and Andrew Gelman. (2005).
##' \dQuote{R2WinBUGS: A Package for Running WinBUGS from R.} \emph{Journal of
##' Statistical Software} 3 (12): 1--6.
##' @keywords interface
##' @usage attach.jags(x, overwrite = NA)
##' @usage detach.jags()
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
