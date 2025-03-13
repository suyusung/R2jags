# YS 2024.6.23
# better way to get the internal function update.jags from rjags package.
# ::: simply doesn't work.
.update.jags <- get("update.jags", envir = asNamespace("rjags"))
.quiet.messages <- get(".quiet.messages", envir = asNamespace("rjags"))

##' Run \sQuote{JAGS} from R
##' 
##' The \code{jags} function takes data and starting values as input.  It
##' automatically writes a \code{jags} script, calls the model, and saves the
##' simulations for easy access in R.
##' 
##' To run: \enumerate{ \item Write a \code{JAGS} model in an ASCII file.
##' \item Go into \R.  \item Prepare the inputs for the \code{jags} function
##' and run it (see Example section).  \item The model will now run in
##' \code{JAGS}. It might take awhile. You will see things happening in the R
##' console.  }
##' 
##' % BUGS version support: % \itemize{ % \item \pkg{jags} 1.0.3 {default} % }
##' 
##' @name jags
##' @aliases rjags-class rjags.parallel-class jags jags2 jags.parallel
##' @docType class
##' @param data (1) a vector or list of the names of the data objects used by
##' the model, (2) a (named) list of the data objects themselves, or (3) the
##' name of a "dump" format file containing the data objects, which must end in
##' ".txt", see example below for details.
##' @param inits a list with \code{n.chains} elements; each element of the list
##' is itself a list of starting values for the \code{BUGS} model, \emph{or} a
##' function creating (possibly random) initial values. If inits is
##' \code{NULL}, \code{JAGS} will generate initial values for parameters.
##' @param parameters.to.save character vector of the names of the parameters
##' to save which should be monitored.
##' @param model.file file containing the model written in \code{BUGS} code.
##' Alternatively, as in \pkg{R2WinBUGS}, \code{model.file} can be an R
##' function that contains a \code{BUGS} model that is written to a temporary
##' model file (see \code{tempfile}) using \code{write.model}
##' @param n.chains number of Markov chains (default: 3)
##' @param n.iter number of total iterations per chain (including burn in;
##' default: 2000)
##' @param n.burnin length of burn in, i.e. number of iterations to discard at
##' the beginning. Default is \code{n.iter/2}, that is, discarding the first
##' half of the simulations. If n.burnin is 0, \code{jags()} will run 100
##' iterations for adaption.
##' @param n.cluster number of clusters to use to run parallel chains.  Default
##' equals n.chains.
##' @param n.thin thinning rate. Must be a positive integer.  Set \code{n.thin}
##' > 1 to save memory and computation time if \code{n.iter} is large.  Default
##' is \code{max(1, floor(n.chains * (n.iter-n.burnin) / 1000))} which will
##' only thin if there are at least 2000 simulations.
##' @param DIC logical; if \code{TRUE} (default), compute deviance, pD, and
##' DIC. The rule \code{pD=var(deviance) / 2} is used.
##' @param pD logical; if \code{TRUE} and \code{DIC} is also \code{TRUE}, then
##' adds the computation of 'pD', using 'rjags::dic.samples()'. Defaults to
##' \code{FALSE}.
##' @param n.iter.pd number of iterations to feed 'rjags::dic.samples()' to
##' compute 'pD'. Defaults at 1000.
##' @param n.adapt number of iterations for which to run the adaptation, when
##' creating the model object. Defaults at 100.
##' @param working.directory sets working directory during execution of this
##' function; This should be the directory where model file is.
##' @param jags.seed random seed for \code{JAGS}, default is 123.  This
##' function is used for jags.parallell() and does not work for jags().  Use
##' set.seed() instead if you want to produce identical result with jags()
##' @param jags.path directory that contains the \code{JAGS} executable.  The
##' default is \dQuote{}.
##' @param clearWD indicating whether the files \file{data.txt},
##' \file{inits[1:n.chains].txt}, \file{codaIndex.txt}, \file{jagsscript.txt},
##' and \file{CODAchain[1:nchains].txt} should be removed after \code{jags} has
##' finished, default=TRUE.
##' @param refresh refresh frequency for progress bar, default is
##' \code{n.iter/50}
##' @param progress.bar type of progress bar. Possible values are
##' \dQuote{text}, \dQuote{gui}, and \dQuote{none}. Type \dQuote{text} is
##' displayed on the R console. Type \dQuote{gui} is a graphical progress bar
##' in a new window. The progress bar is suppressed if \code{progress.bar} is
##' \dQuote{none}. Default set globally to \dQuote{text}
##' @param digits as in \code{\link{write.model}} in the \pkg{R2WinBUGS}
##' package: number of significant digits used for \code{BUGS} input, see
##' \code{\link{formatC}}.  Only used if specifying a \code{BUGS} model as an R
##' function.
##' @param RNGname the name for random number generator used in JAGS. There are
##' four RNGS supplied by the base moduale in JAGS: \code{Wichmann-Hill},
##' \code{Marsaglia-Multicarry}, \code{Super-Duper}, \code{Mersenne-Twister}
##' @param jags.module the vector of jags modules to be loaded.  Default are
##' \dQuote{glm} and \dQuote{dic}. Input NULL if you don't want to load any
##' jags module.
##' @param export_obj_names character vector of objects to export to the
##' clusters.
##' @param envir default is .GlobalEnv
##' @param quiet Logical, whether to suppress stdout in \code{jags.model()}.
##' Default set globally to \dQuote{FALSE}
##' @param checkMissing Default: FALSE. When TRUE, checks for missing data in
##' categorical parameters and returns a \code{sim.list} with NA values if
##' detected. It's recommended to supply \code{jags()} with complete data.
##' @author Yu-Sung Su \email{suyusung@@tsinghua.edu.cn}, Masanao Yajima
##' \email{yajima@@bu.edu}, Gianluca Baio \email{g.baio@@ucl.ac.uk}
##' @references Plummer, Martyn (2003) \dQuote{JAGS: A program for analysis of
##' Bayesian graphical models using Gibbs sampling.}
##' \url{https://www.r-project.org/conferences/DSC-2003/Proceedings/Plummer.pdf}.
##' 
##' Gelman, A., Carlin, J. B., Stern, H.S., Rubin, D.B. (2003) \emph{Bayesian
##' Data Analysis}, 2nd edition, CRC Press.
##' 
##' Sibylle Sturtz and Uwe Ligges and Andrew Gelman. (2005).
##' \dQuote{R2WinBUGS: A Package for Running WinBUGS from R.} \emph{Journal of
##' Statistical Software} 3 (12): 1--6.
##' @keywords interface models
##' @examples
##' # Can set global options for some of the features of R2jags. For example
##' # running the command
##' options(r2j.pb="none")
##' # before launching jags shuts down the progress bar. Or running the command
##' options(r2j.quiet=FALSE)
##' # forces jags to not show the compilation summary. Or running the command
##' options(r2j.print.progam=FALSE)
##' # modifies the print method and prevents R2jags from printing the name of 
##' # software used to run the MCMC analysis. These options may be helpful 
##' # when using Rmarkdown or quarto to help formtting the output.
##' #
##' # An example model file is given in:
##' model.file <- system.file(package="R2jags", "model", "schools.txt")
##' # Let's take a look:
##' file.show(model.file)
##' 
##' #=================#
##' # Initialization  #
##' #=================#
##' 
##' # Data
##' J <- 8.0
##' y <- c(28.4, 7.9, -2.8, 6.8, -0.6, 0.6, 18.0, 12.2)
##' sd <- c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6)
##' 
##' jags.data <- list(y = y, sd = sd, J = J)
##' jags.params <- c("mu", "sigma", "theta")
##' jags.inits <- function() {
##'   list(mu = rnorm(1), sigma = runif(1), theta = rnorm(J))
##' }
##' 
##' ## You can input data in 4 ways:
##' 
##' # 1) Data as a list of character strings
##' jagsfit <- jags(data = list("y", "sd", "J"), inits = jags.inits, 
##'                 parameters.to.save = jags.params, 
##'                 n.iter = 10, model.file = model.file)
##' 
##' # 2) Data as a character vector of names
##' jagsfit <- jags(data = c("y", "sd", "J"), inits = jags.inits, 
##'                 parameters.to.save = jags.params, 
##'                 n.iter = 10, model.file = model.file)
##' 
##' # 3) Data as a named list
##' jagsfit <- jags(data = list(y = y, sd = sd, J = J), inits = jags.inits, 
##'                 parameters.to.save = jags.params, 
##'                 n.iter = 10, model.file = model.file)
##' 
##' # 4) Data from a file
##' fn <- "tmpbugsdata.txt"
##' dump(c("y", "sd", "J"), file = fn)
##' jagsfit <- jags(data = fn, inits = jags.inits, 
##'                 parameters.to.save = jags.params, 
##'                 n.iter = 10, model.file = model.file)
##' unlink("tmpbugsdata.txt")
##' 
##' ## Writing a BUGS model as an R function
##' schoolsmodel <- function() {
##'   for (j in 1:J) {
##'     y[j] ~ dnorm(theta[j], tau.y[j])  # Likelihood
##'     tau.y[j] <- pow(sd[j], -2)        # Precision
##'   }
##'   for (j in 1:J) {
##'     theta[j] ~ dnorm(mu, tau)         # Hierarchical model
##'   }
##'   tau <- pow(sigma, -2)
##'   mu ~ dnorm(0.0, 1.0E-6)
##'   sigma ~ dunif(0, 1000)
##' }
##' 
##' jagsfit <- jags(data = jags.data, inits = jags.inits, 
##'                 parameters.to.save = jags.params, 
##'                 n.iter = 10, model.file = schoolsmodel)
##' 
##' #=================================#
##' # Running JAGS and postprocessing #
##' #=================================#
##' 
##' jagsfit <- jags(data = jags.data, inits = jags.inits, 
##'                 parameters.to.save = jags.params, 
##'                 n.iter = 5000, model.file = model.file)
##' 
##' ## Computing DIC with pD
##' \dontrun{
##' jagsfit.pD <- jags(data = jags.data, inits = jags.inits, 
##'                    parameters.to.save = jags.params, 
##'                    n.iter = 5000, model.file = model.file, pD = TRUE)
##' }
##' 
##' ## Running JAGS in parallel
##' \dontrun{
##' jagsfit.p <- jags.parallel(data = jags.data, inits = jags.inits, 
##'                             parameters.to.save = jags.params, 
##'                             n.iter = 5000, model.file = model.file)
##' print(jagsfit.p,digits=3,interval=c(0.025,0.975))
##' plot(jagsfit.p)
##' }
##' 
##' ## Checking model convergence
##' \dontrun{
##' traceplot(jagsfit)
##' traceplot(jagsfit.p)
##' }
##' 
##' ## Updating JAGS if the model does not converge
##' \dontrun{
##' jagsfit.upd <- update(jagsfit, n.iter = 100)
##' print(jagsfit.upd)
##' plot(jagsfit.upd)
##' }
##' 
##' ## Auto-updating until convergence
##' \dontrun{
##' jagsfit.upd <- autojags(jagsfit)
##' jagsfit.upd <- autojags(jagsfit.p)
##' }
##' 
##' ## Obtaining DIC separately
##' \dontrun{
##' dic.samples(jagsfit.upd$model, n.iter = 1000, type = "pD")
##' }
##' 
##' ## Attaching and detaching JAGS results
##' \dontrun{
##' attach.jags(jagsfit.upd)
##' mu  # View parameter estimates
##' detach.jags()
##' }
##' 
##' ## Picking up the last saved session
##' \dontrun{
##' recompile(jagsfit)
##' jagsfit.upd <- update(jagsfit, n.iter = 100)
##' }
##'
jags <- function( data, inits,
                  parameters.to.save,
                  model.file  = "model.bug",
                  n.chains     = 3,
                  n.iter       = 2000,
                  n.burnin     = floor(n.iter/2),
                  n.thin       = max( 1, floor( ( n.iter - n.burnin )/1000 ) ),
                  DIC          = TRUE,
                  pD           = FALSE,
                  n.iter.pd    = NULL,
                  n.adapt      = 100,
                  working.directory = NULL,
                  jags.seed    = 123,
                  refresh      = n.iter/50,
                  progress.bar,
                  digits = 5,
                  RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", "Mersenne-Twister"),
                  jags.module = c("glm","dic"),
                  quiet,
                  checkMissing = FALSE
                  )
{
  #require( rjags )
  # GB: starts the clock to record the running time
  tic <- proc.time()

  if( !is.null( working.directory ) ){
    working.directory <- path.expand( working.directory )
    savedWD <- getwd()
    setwd( working.directory )
    on.exit( setwd( savedWD ) )
  } else {
    savedWD <- getwd()
    working.directory <- savedWD
  }
  
  ## Checks for global options (re progress.bar and quiet)
  if (missing(progress.bar)) {
    progress.bar <- getOption("r2j.pb")
  }
  if (!is.null(progress.bar)) {
    match.arg(progress.bar, c("text","gui","none"))
    if (progress.bar=="none")
      progress.bar <- NULL
  }
  if (missing(quiet)) {
    quiet <- getOption("r2j.quiet")
  }
  if (!is.null(quiet)) {
    if(quiet) {
      .quiet.messages(TRUE)
      on.exit(.quiet.messages(FALSE), add=TRUE)
    }
  }
  
  ## jags.model() needs 'data' to be "a list or environment containing the data
  if( is.character( data ) && length(data) == 1
                           && regexpr( "\\.txt$", data ) > 0 ) {
    ## 1. 'data' looks like a file name [UNDOCUMENTED!]
    if ( all( basename( data ) == data )) {
      fn2 <- file.path( working.directory, data )
      if (normalizePath(fn2)!=normalizePath(data)) {  ## file.copy() to same place trashes the file
        try( file.copy(fn2 , data, overwrite = TRUE ) )
      }
    }
    if ( !file.exists( data ) ) {
      stop("File",data,"does not exist")
    }
    if (file.info(data)["size"]==0) {
      stop("Empty data file ",data)
    }
    e    <- new.env()
    eval( parse( data ), e )
    data <- as.list( e )
  } else if( is.character( data ) ||
             ( is.list( data ) && all( sapply( data,is.character ) ) ) ){
    ## 2. data is a character vector or a list of character
    dlist          <- lapply( as.list( data ), get, envir = parent.frame( 1 ) )
    names( dlist ) <- unlist( data )
    data           <- dlist
  } else if( !is.list( data ) ){
    stop( "data must be a character vector of object names, a list of object names, or a list of objects" )
  }

  ## copied from R2WinBUGS:
  if (is.function(model.file)) {
    temp <- tempfile("model")
    temp <- if (.Platform$OS.type != "windows") {
      paste(temp, "txt", sep = ".")
    }
    else {
      gsub("\\.tmp$", ".txt", temp)
    }
    write.model(model.file, con = temp, digits = digits) ## from R2WinBUGS
    model.file <- gsub("\\\\", "/", temp)
  }
  if( DIC ){
    parameters.to.save <- c( parameters.to.save, "deviance" )
    load.module( "dic", quiet = TRUE )
  }



  if (!missing(inits) && !is.function(inits) && !is.null(inits) && (length(inits) != n.chains)) {
    stop("Number of initialized chains (length(inits)) != n.chains")
  }

  RNGname <- match.arg(RNGname)
  if(RNGname %in% c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", "Mersenne-Twister")){
    RNGname <- paste("base::",RNGname,sep="")
  }else{
    stop("The name of the RNG is not correctly provided!")
  }


  #load.module("lecuyer")
  if(length(jags.module) > 0L) {
    n.module <- length(jags.module)
    for(m in seq_len(n.module)) {
        load.module(jags.module[m], quiet = quiet)
    }
  }

  #load.module(jags.module)
  init.values <- vector("list", n.chains)
  if(missing(inits)){
    for (i in 1:n.chains){
        init.values[[i]]$.RNG.name <- RNGname
        init.values[[i]]$.RNG.seed <- runif(1, 0, 2^31)#abs(.Random.seed[i+1])
    }
  } else if (is.null(inits)){
      for (i in 1:n.chains){
        init.values[[i]]$.RNG.name <- RNGname
        init.values[[i]]$.RNG.seed <- runif(1, 0, 2^31)#abs(.Random.seed[i+1])
      }
  } else if (is.function(inits)){
      if (any(names(formals(inits)) == "chain")){
        for (i in 1:n.chains){
          init.values[[i]] <- inits(chain = i)
          init.values[[i]]$.RNG.name <- RNGname
          init.values[[i]]$.RNG.seed <- runif(1, 0, 2^31)#abs(.Random.seed[i+1])
        }
      } else{
          for (i in 1:n.chains){
            init.values[[i]] <- inits()
            init.values[[i]]$.RNG.name <- RNGname
            init.values[[i]]$.RNG.seed <- runif(1, 0, 2^31)#abs(.Random.seed[i+1])
          }
      }
  } else {
    #if (!is.function(inits) && !is.null(inits) && (length(inits) != n.chains)) {
    if (!is.list(inits)) {
      stop("Invalid inits")
    }
    if (length(inits) != n.chains) {
      stop("Number of initialized chains (length(inits)) != n.chains")
    }
    for (i in 1:n.chains){
      init.values[[i]] <- inits[[i]]
      init.values[[i]]$.RNG.name <- RNGname
      init.values[[i]]$.RNG.seed <- runif(1, 0, 2^31)#abs(.Random.seed[i+1])
    }
   }

  # Compiles the model object
  m <- jags.model(model.file,
                  data     = data,
                  inits    = init.values,
                  n.chains = n.chains,
                  n.adapt  = 0,
                  quiet = quiet )
  
  # Adds adaptation. This never needs to be verbose, so force
  # 'progress.bar' to 'none' and 'quiet' to TRUE, irrespective of what the
  # global values for these options are in the function
  adapt(m,n.iter=n.adapt,progress.bar="none",quiet=TRUE,end.adaptation=TRUE)

  # Updates the model for the burning phase
  .update.jags( 
    m,
    n.iter=max(n.burnin,round(n.iter/2)),
    n.thin=n.thin,
    by=refresh,
    progress.bar=progress.bar,
    quiet = quiet
  )

  # Now saves the samples after burnin
  samples <- coda.samples( model          = m,
                           variable.names = parameters.to.save,
                           n.iter         = ( n.iter - n.burnin ),
                           thin           = n.thin,
                           by             = refresh,
                           quiet          = quiet,
                           progress.bar   = progress.bar )

  # GB: Add call to 'rjags::dic.samples()' to add pD if the argument 'pD' is
  # set to TRUE in the call to 'jags'
  if(pD) {
    if (is.null(n.iter.pd)) {n.iter.pd <- 1000}
    pDstuff <- rjags::dic.samples(
      model=m, n.iter=n.iter.pd, progress.bar="none", quiet=TRUE
    )
   pD <- sum(pDstuff$penalty)
   DIC2 <- sum(pDstuff$deviance) + pD
  }

  # GB: stops the clock and records the running time
  toc <- proc.time()-tic
  names(toc)[3] <- "Running time (secs)"

  fit <- mcmc2bugs( samples,
                    model.file = model.file,
                    program    = "jags",
                    DIC        = DIC,
                    DICOutput  = NULL,
                    n.iter     = n.iter,
                    n.burnin   = n.burnin,
                    n.thin     = n.thin,
                    checkMissing = checkMissing)
  # GB: adds the running time to the 'fit' object (which will then be
  # renamed as 'BUGSoutput)
  fit$time2run <- toc[3]
  # GB: *If it exists*, adds the pD and the resulting DIC to the 'fit' object
  if(pD) {
    fit$pD <- pD
    fit$DIC2 <- DIC2
  }

  out <- list( model              = m,
               BUGSoutput         = fit,
               parameters.to.save = parameters.to.save,
               model.file         = model.file,
               n.iter             = n.iter,
               DIC                = DIC)

  class(out) <- "rjags"
  return(out)
}
