##' Read jags output files in CODA format
##' 
##' This function reads Markov Chain Monte Carlo output in the CODA format
##' produced by \pkg{jags} and returns an object of class
##' \code{\link[coda]{mcmc.list}} for further output analysis using the
##' \pkg{coda} package.
##' 
##' 
##' @param path sets working directory during execution of this function; This
##' should be the directory where CODA files are.
##' @param parameters.to.save character vector of the names of the parameters
##' to save which should be monitored.
##' @param n.chains number of Markov chains (default: 3)
##' @param n.iter number of total iterations per chain (including burn in;
##' default: 2000)
##' @param n.burnin length of burn in, i.e. number of iterations to discard at
##' the beginning. Default is \code{n.iter/2}, that is, discarding the first
##' half of the simulations.
##' @param n.thin thinning rate, default is 2
##' @param DIC logical; if \code{TRUE} (default), compute deviance, pD, and
##' DIC. The rule \code{pD=var(deviance) / 2} is used.
##' @author Yu-Sung Su \email{suyusung@@tsinghua.edu.cn}, Masanao Yajima
##' \email{yajima@@stat.columbia.edu}
##' @keywords IO file
jags2bugs <- function(path=getwd(), parameters.to.save, n.chains=3, n.iter=2000, n.burnin=1000, n.thin=2, DIC=TRUE){
  setwd(path)
  #require(R2WinBUGS)
  fit <- jags.sims(parameters.to.save, n.chains=n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin, DIC = DIC)
  class(fit) <- "bugs"
  return(fit)
}                       
