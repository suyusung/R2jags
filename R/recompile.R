##' Function for recompiling rjags object
##' 
##' The \code{recompile} takes a \code{rjags} object as input. \code{recompile}
##' will re-compile the previous saved \code{rjags} object.
##' 
##' 
##' @aliases recompile recompile.rjags
##' @param object an object of \code{rjags} class.
##' @param n.iter number of iteration for adapting, default is 100
##' @param refresh refresh frequency for progress bar, default is
##' \code{n.iter/50}
##' @param progress.bar type of progress bar. Possible values are
##' \dQuote{text}, \dQuote{gui}, and \dQuote{none}. Type \dQuote{text} is
##' displayed on the R console. Type \dQuote{gui} is a graphical progress bar
##' in a new window. The progress bar is suppressed if \code{progress.bar} is
##' \dQuote{none}
##' @author Yu-Sung Su \email{suyusung@@tsinghua.edu.cn}
##' @keywords models
##' @examples
##' 
##' 
##'  # see ?jags for an example.
##' 
##' 
recompile <- function(object, n.iter, refresh, progress.bar) UseMethod("recompile")


recompile.rjags <- function(object, n.iter = 100, refresh = n.iter/50, progress.bar)
{
  ## Checks for global options (re progress.bar and quiet)
  if (missing(progress.bar)) {
    progress.bar <- getOption("r2j.pb")
  }
  if (!is.null(progress.bar)) {
    match.arg(progress.bar, c("text","gui","none"))
    if (progress.bar=="none")
      progress.bar <- NULL
  }
  
  object$model$recompile()
  adapt(object$model, n.iter = n.iter, by = refresh, progress.bar = progress.bar, end.adaptation=TRUE) 

}
  
recompile.rjags.parallel <- function(object, n.iter = 100, refresh = n.iter/50, progress.bar)
{
  ## Checks for global options (re progress.bar and quiet)
  if (missing(progress.bar)) {
    progress.bar <- getOption("r2j.pb")
  }
  if (!is.null(progress.bar)) {
    match.arg(progress.bar, c("text","gui","none"))
    if (progress.bar=="none")
      progress.bar <- NULL
  }
  
  nchains <- length(object$model)
  for(i in 1:nchains){
    object$model[[i]]$recompile()
    adapt(object$model[[i]], n.iter = n.iter, by = refresh, progress.bar = progress.bar, end.adaptation=TRUE) 
  }
}
