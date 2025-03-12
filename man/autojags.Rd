\name{autojags}
\alias{update.rjags}
\alias{autojags}



\title{Function for auto-updating \sQuote{JAGS} until the model converges}
\description{
The \code{autojags} takes a \code{rjags} object as input.
\code{autojags} will update the model until it converges.
}

\usage{

\method{update}{rjags}(object, n.iter=1000, n.thin=1,
                      refresh=n.iter/50, progress.bar, \dots)
autojags(object, n.iter=1000, n.thin=1, Rhat=1.1, n.update=2,
      refresh=n.iter/50, progress.bar, \dots)

}


\arguments{
  \item{object}{an object of \code{rjags} class.}
  \item{n.iter}{number of total iterations per chain, default=1000}
  \item{n.thin}{thinning rate. Must be a positive integer, default=1}
  \item{\dots}{further arguments pass to or from other methods.}
  \item{Rhat}{converegence criterion, default=1.1.}
  \item{n.update}{the max number of updates, default=2.}
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


\references{
    Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B. (2003):
    \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.
}




\examples{
 # see ?jags for an example.
}

\keyword{models}
