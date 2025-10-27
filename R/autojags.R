##' Function for auto-updating \sQuote{JAGS} until the model converges
##' 
##' The \code{autojags} takes a \code{rjags} object as input. \code{autojags}
##' will update the model until it converges.
##' 
##' 
##' @aliases autojags update.rjags
##' @param object an object of \code{rjags} class.
##' @param n.iter number of total iterations per chain, default=1000
##' @param n.thin thinning rate. Must be a positive integer, default=1
##' @param Rhat converegence criterion, default=1.1.
##' @param n.update the max number of updates, default=2.
##' @param refresh refresh frequency for progress bar, default is
##' \code{n.iter/50}
##' @param progress.bar type of progress bar. Possible values are
##' \dQuote{text}, \dQuote{gui}, and \dQuote{none}. Type \dQuote{text} is
##' displayed on the R console. Type \dQuote{gui} is a graphical progress bar
##' in a new window. The progress bar is suppressed if \code{progress.bar} is
##' \dQuote{none}
##' @param \dots further arguments pass to or from other methods.
##' @author Yu-Sung Su \email{suyusung@@tsinghua.edu.cn}
##' @references Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B. (2003):
##' \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.
##' @keywords models
##' @examples
##' 
##' 
##'  # see ?jags for an example.
##' 
##' 
autojags <- function(object, n.iter=1000, n.thin=1, Rhat=1.1, n.update=2, refresh=n.iter/50, 
    progress.bar,...)
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

  n.burnin = object$n.iter
  n.thin.auto <- max( 1, floor( ( n.iter - n.burnin )/1000 ) )
  n.thin <- ifelse(n.thin > n.thin.auto, n.thin, n.thin.auto)

  if(any(!class(object) %in% c("rjags","rjags.parallel"))) stop("model must be a rjags object")
    object <- update(object, n.iter=n.iter, n.thin=n.thin, 
                      refresh=refresh, progress.bar = progress.bar,...)
    check <- any(object$BUGSoutput$summary[,"Rhat"] > Rhat)
    if (check){
      count <- 1
      while (check & (count < n.update)) {
          object <- update(object, n.iter=n.iter, n.thin=n.thin, 
                      refresh=refresh, progress.bar = progress.bar, ...)
          count <- count + 1
          check <- any(object$BUGSoutput$summary[,"Rhat"] > Rhat)
      }
    }
    return(object)
}
