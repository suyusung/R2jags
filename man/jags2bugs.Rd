\name{jags2bugs}
\alias{jags2bugs}

\title{Read jags output files in CODA format}


\description{This function reads Markov Chain Monte Carlo output in the
  CODA format produced by \pkg{jags} and returns an object of class
  \code{\link[coda]{mcmc.list}} for further output analysis using the
  \pkg{coda} package.}


\usage{
jags2bugs(path=getwd(), parameters.to.save, 
  n.chains=3, n.iter=2000, n.burnin=1000, n.thin=2,
  DIC=TRUE)
}

\arguments{
  \item{path}{sets working directory during execution of
    this function; This should be the directory where CODA files are.}
  \item{parameters.to.save}{character vector of the names of the
    parameters to save which should be monitored.}
  \item{n.chains}{number of Markov chains (default: 3)}
  \item{n.iter}{number of total iterations per chain (including burn in;
    default: 2000)}
  \item{n.burnin}{length of burn in, i.e. number of iterations to
    discard at the beginning. Default is \code{n.iter/2}, that is,
    discarding the first half of the simulations.}
  \item{n.thin}{thinning rate, default is 2}
  \item{DIC}{logical; if \code{TRUE} (default), compute deviance, pD,
    and DIC. The rule \code{pD=var(deviance) / 2} is used.}
}



\author{ 
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
  Masanao Yajima \email{yajima@stat.columbia.edu}
}

\keyword{IO}
\keyword{file}
