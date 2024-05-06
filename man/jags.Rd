\name{jags}
\docType{class}
% Classes
\alias{rjags-class}
\alias{rjags.parallel-class}
% functions
\alias{jags}
\alias{jags2}
\alias{jags.parallel}


\title{Run \sQuote{JAGS} from R}
\description{
  The \code{jags} function takes data and starting values as input.  It
  automatically writes a \code{jags} script, calls the model, and
  saves the simulations for easy access in R.
}

\usage{
jags(data, inits, parameters.to.save, model.file="model.bug",
  n.chains=3, n.iter=2000, n.burnin=floor(n.iter/2),
  n.thin=max(1, floor((n.iter - n.burnin) / 1000)),
  DIC=TRUE, working.directory=NULL, jags.seed = 123,
  refresh = n.iter/50, progress.bar = "text", digits=5,
  RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry",
              "Super-Duper", "Mersenne-Twister"),
  jags.module = c("glm","dic"), quiet = FALSE,
  checkMissing = FALSE
  )

jags.parallel(data, inits, parameters.to.save, model.file = "model.bug",
             n.chains = 2, n.iter = 2000, n.burnin = floor(n.iter/2),
             n.thin = max(1, floor((n.iter - n.burnin)/1000)),
             n.cluster= n.chains, DIC = TRUE,
             working.directory = NULL, jags.seed = 123, digits=5,
             RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry",
              "Super-Duper", "Mersenne-Twister"),
             jags.module = c("glm","dic"),
             export_obj_names=NULL,
             envir = .GlobalEnv
             )

jags2(data, inits, parameters.to.save, model.file="model.bug",
  n.chains=3, n.iter=2000, n.burnin=floor(n.iter/2),
  n.thin=max(1, floor((n.iter - n.burnin) / 1000)),
  DIC=TRUE, jags.path="",
  working.directory=NULL, clearWD=TRUE,
  refresh = n.iter/50)
}


\arguments{
  \item{data}{(1) a vector or list of the names of the data objects used by
    the model, (2) a (named) list of the data objects themselves, or
  (3) the name of a "dump" format file containing the data objects,
   which must end in ".txt", see example below for details.}
  \item{inits}{a list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \code{BUGS} model,
    \emph{or} a function creating (possibly random) initial values. If inits is
    \code{NULL}, \code{JAGS} will generate initial values for parameters.}
  \item{parameters.to.save}{character vector of the names of the
    parameters to save which should be monitored.}
  \item{model.file}{file containing the model written in \code{BUGS}
  code. Alternatively, as in \pkg{R2WinBUGS}, \code{model.file} can be an R
  function that contains a \code{BUGS} model that is written to a
  temporary model file (see \code{\link{tempfile}}) using \code{\link{write.model}}}
  \item{n.chains}{number of Markov chains (default: 3)}
  \item{n.iter}{number of total iterations per chain (including burn in;
    default: 2000)}
  \item{n.burnin}{length of burn in, i.e. number of iterations to
    discard at the beginning. Default is \code{n.iter/2}, that is,
    discarding the first half of the simulations. If n.burnin is 0,
    \code{jags()} will run 100 iterations for adaption.}
  \item{n.cluster}{number of clusters to use to run parallel chains.
    Default equals n.chains.}
  \item{n.thin}{thinning rate. Must be a positive integer.  Set
    \code{n.thin} > 1 to save memory and computation time if
    \code{n.iter} is large.  Default is \code{max(1, floor(n.chains *
    (n.iter-n.burnin) / 1000))} which will only thin if there are at
    least 2000 simulations.}
%  \item{n.sims}{The approximate number of simulations to keep after thinning.}
%  \item{n.adapt}{the number of iterations for adaptation.  When a model is
%          first created, some of the samplers may have an adaptive
%          mode. In this case, the samplers are run for \code{n.adapt}
%          iterations and then the adaptive mode is switched off.}
  \item{DIC}{logical; if \code{TRUE} (default), compute deviance, pD,
    and DIC. The rule \code{pD=var(deviance) / 2} is used.}
  \item{working.directory}{sets working directory during execution of
    this function; This should be the directory where model file is.}
  \item{jags.seed}{random seed for \code{JAGS}, default is 123.  This function is used for jags.parallell() and does not work for jags().  Use set.seed() instead if you want to produce identical result with jags() }.
  \item{jags.path}{directory that contains the \code{JAGS} executable.
    The default is \dQuote{}.  }
  \item{clearWD}{indicating whether the files \file{data.txt},
    \file{inits[1:n.chains].txt}, \file{codaIndex.txt}, \file{jagsscript.txt},
    and \file{CODAchain[1:nchains].txt} should be removed after \code{jags} has
    finished, default=TRUE.}
  \item{refresh}{refresh frequency for progress bar, default is \code{n.iter/50}}
  \item{progress.bar}{type of progress bar. Possible values are \dQuote{text},
      \dQuote{gui}, and \dQuote{none}. Type \dQuote{text} is displayed
      on the R console. Type \dQuote{gui} is a graphical progress bar
      in a new window. The progress bar is suppressed if \code{progress.bar} is
      \dQuote{none}}
    \item{digits}{as in \code{\link{write.model}} in the \pkg{R2WinBUGS} package:  number of significant digits used for
  \code{BUGS} input, see \code{\link{formatC}}.  Only used if specifying  a \code{BUGS} model as an R function.}
  \item{RNGname}{the name for random number generator used in JAGS. There are four RNGS
  supplied by the base moduale in JAGS: \code{Wichmann-Hill}, \code{Marsaglia-Multicarry},
  \code{Super-Duper}, \code{Mersenne-Twister}}
  \item{jags.module}{the vector of jags modules to be loaded.  Default are \dQuote{glm} and \dQuote{dic}. Input NULL if you don't want to load any jags module.}
  \item{export_obj_names}{character vector of objects to export to the clusters.}
  \item{envir}{default is .GlobalEnv}
  \item{quiet}{Logical, whether to suppress stdout in \code{jags.model()}.}
  \item{checkMissing}{Default: FALSE. When TRUE, checks for missing data in categorical parameters 
  and returns a \code{sim.list} with NA values if detected. It's recommended to supply \code{jags()} with complete data.}
}



\details{
  To run:
  \enumerate{
    \item Write a \code{JAGS} model in an ASCII file.
    \item Go into \R.
    \item Prepare the inputs for the \code{jags} function and run it (see
      Example section).
    \item The model will now run in \code{JAGS}. It might take awhile. You
      will see things happening in the R console.
  }

%  BUGS version support:
%  \itemize{
%    \item \pkg{jags} 1.0.3 {default}
% }
}

\author{
  Yu-Sung Su \email{suyusung@tsinghua.edu.cn},
  Masanao Yajima \email{yajima@bu.edu}
}


\references{
    Plummer, Martyn (2003)
    \dQuote{JAGS: A program for analysis of Bayesian graphical models using Gibbs sampling.}
    \url{https://www.r-project.org/conferences/DSC-2003/Proceedings/Plummer.pdf}.

    Gelman, A., Carlin, J. B., Stern, H.S., Rubin, D.B. (2003)
    \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.

    Sibylle Sturtz and Uwe Ligges and Andrew Gelman. (2005).
      \dQuote{R2WinBUGS: A Package for Running WinBUGS from R.}
      \emph{Journal of Statistical Software} 3 (12): 1--6.
}




\examples{
  # An example model file is given in:
  model.file <- system.file(package="R2jags", "model", "schools.txt")
  # Let's take a look:
  file.show(model.file)
  # you can also write BUGS model as a R function, see below:

#=================#
# initialization  #
#=================#

  # data
  J <- 8.0
  y <- c(28.4,7.9,-2.8,6.8,-0.6,0.6,18.0,12.2)
  sd <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)


  jags.data <- list("y","sd","J")
  jags.params <- c("mu","sigma","theta")
  jags.inits <- function(){
    list("mu"=rnorm(1),"sigma"=runif(1),"theta"=rnorm(J))
  }

  ## You can input data in 4 ways
  ## 1) data as list of character
  jagsfit <- jags(data=list("y","sd","J"), inits=jags.inits, jags.params,
                n.iter=10, model.file=model.file)

  ## 2) data as character vector of names
  jagsfit <- jags(data=c("y","sd","J"), inits=jags.inits, jags.params,
                n.iter=10, model.file=model.file)

  ## 3) data as named list
  jagsfit <- jags(data=list(y=y,sd=sd,J=J), inits=jags.inits, jags.params,
                n.iter=10, model.file=model.file)

  ## 4) data as a file
  fn <- "tmpbugsdata.txt"
  dump(c("y","sd","J"), file=fn)
  jagsfit <- jags(data=fn, inits=jags.inits, jags.params,
                  n.iter=10, model.file=model.file)
  unlink("tmpbugsdata.txt")

  ## You can write bugs model in R as a function

  schoolsmodel <- function() {
    for (j in 1:J){                     # J=8, the number of schools
      y[j] ~ dnorm (theta[j], tau.y[j]) # data model:  the likelihood
      tau.y[j] <- pow(sd[j], -2)        # tau = 1/sigma^2
    }
    for (j in 1:J){
      theta[j] ~ dnorm (mu, tau)        # hierarchical model for theta
    }
    tau <- pow(sigma, -2)               # tau = 1/sigma^2
    mu ~ dnorm (0.0, 1.0E-6)            # noninformative prior on mu
    sigma ~ dunif (0, 1000)             # noninformative prior on sigma
  }

  jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params,
                n.iter=10, model.file=schoolsmodel)


#===============================#
# RUN jags and postprocessing   #
#===============================#
  jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params,
    n.iter=5000, model.file=model.file)

  # Run jags parallely, no progress bar. R may be frozen for a while,
  # Be patient. Currenlty update afterward does not run parallelly
  #
   jagsfit.p <- jags.parallel(data=jags.data, inits=jags.inits, jags.params,
     n.iter=5000, model.file=model.file)

  # display the output
  print(jagsfit)
  plot(jagsfit)

  # traceplot
  traceplot(jagsfit.p)
  traceplot(jagsfit)

  # or to use some plots in coda
  # use as.mcmmc to convert rjags object into mcmc.list
  jagsfit.mcmc <- as.mcmc(jagsfit.p)
  jagsfit.mcmc <- as.mcmc(jagsfit)
  ## now we can use the plotting methods from coda
  #require(lattice)
  #xyplot(jagsfit.mcmc)
  #densityplot(jagsfit.mcmc)

  # if the model does not converge, update it!
  jagsfit.upd <- update(jagsfit, n.iter=100)
  print(jagsfit.upd)
  print(jagsfit.upd, intervals=c(0.025, 0.5, 0.975))
  plot(jagsfit.upd)

  # before update parallel jags object, do recompile it
  recompile(jagsfit.p)
  jagsfit.upd <- update(jagsfit.p, n.iter=100)



  # or auto update it until it converges! see ?autojags for details
  # recompile(jagsfit.p)
  jagsfit.upd <- autojags(jagsfit.p)
  jagsfit.upd <- autojags(jagsfit)

  # to get DIC or specify DIC=TRUE in jags() or do the following#
  dic.samples(jagsfit.upd$model, n.iter=1000, type="pD")

  # attach jags object into search path see "attach.bugs" for details
  attach.jags(jagsfit.upd)

  # this will show a 3-way array of the bugs.sim object, for example:
  mu

  # detach jags object into search path see "attach.bugs" for details
  detach.jags()

  # to pick up the last save session
  # for example, load("RWorkspace.Rdata")
  recompile(jagsfit)
  jagsfit.upd <- update(jagsfit, n.iter=100)

  recompile(jagsfit.p)
  jagsfit.upd <- update(jagsfit, n.iter=100)

#=============#
# using jags2 #
#=============#
  ## jags can be run and produces coda files, but cannot be updated once it's done
  ## You may need to edit "jags.path" to make this work,
  ## also you need a write access in the working directory:
  ## e.g. setwd("d:/")

  ## NOT RUN HERE
  \dontrun{
    jagsfit <- jags2(data=jags.data, inits=jags.inits, jags.params,
      n.iter=5000, model.file=model.file)
    print(jagsfit)
    plot(jagsfit)
    # or to use some plots in coda
    # use as.mcmmc to convert rjags object into mcmc.list
    jagsfit.mcmc <- as.mcmc.list(jagsfit)
    traceplot(jagsfit.mcmc)
    #require(lattice)
    #xyplot(jagsfit.mcmc)
    #densityplot(jagsfit.mcmc)
  }

}

\keyword{interface}
\keyword{models}
