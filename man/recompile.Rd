\name{recompile}
\alias{recompile}
\alias{recompile.rjags}


\title{Function for recompiling rjags object}
\description{
The \code{recompile} takes a \code{rjags} object as input.  
\code{recompile} will re-compile the previous saved \code{rjags} object.
}

\usage{

recompile(object, n.iter, refresh, progress.bar)
\method{recompile}{rjags}(object, n.iter=100, refresh=n.iter/50, 
    progress.bar = "text")


}


\arguments{
  \item{object}{an object of \code{rjags} class.}
  \item{n.iter}{number of iteration for adapting, default is 100}
  \item{refresh}{refresh frequency for progress bar, default is \code{n.iter/50}}
  \item{progress.bar}{type of progress bar. Possible values are \dQuote{text}, 
      \dQuote{gui}, and \dQuote{none}. Type \dQuote{text} is displayed
      on the R console. Type \dQuote{gui} is a graphical progress bar 
      in a new window. The progress bar is suppressed if \code{progress.bar} is 
      \dQuote{none}}
}


\author{ 
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn}
}


\examples{
 # see ?jags for an example.
}

\keyword{models}
