#traceplot <- function(x, ...) UseMethod("traceplot")

##' Trace plot of bugs object
##' 
##' Displays a plot of iterations \emph{vs.} sampled values for each variable
##' in the chain, with a separate plot per variable.
##' 
##' @name traceplot
##' @aliases traceplot traceplot.default traceplot,mcmc.list-method
##' traceplot,rjags-method traceplot,bugs-method
##' @param x A bugs object
##' @param mfrow graphical parameter (see \code{par})
##' @param varname vector of variable names to plot
##' @param match.head matches the variable names by the beginning of the
##' variable names in bugs object
##' @param ask logical; if \code{TRUE}, the user is \emph{ask}ed before each
##' plot, see \code{par(ask=.)}.
##' @param col graphical parameter (see \code{par})
##' @param lty graphical parameter (see \code{par})
##' @param lwd graphical parameter (see \code{par})
##' @param \dots further graphical parameters
##' @author Masanao Yajima \email{yajima@@stat.columbia.edu}.
##' @seealso \code{\link[coda]{densplot}}, \code{\link[coda]{plot.mcmc}},
##' \code{\link[coda]{traceplot}}
##' @keywords hplot
##' @usage traceplot(x,...)
##' 
if (!isGeneric("traceplot")) {
    setGeneric("traceplot",
               function(x, ...)
               standardGeneric("traceplot"),
                useAsDefault = function(x, ...) coda::traceplot(x, ...))
}


#ttraceplot.default <- function(x, ...) coda::traceplot


# ========================================================================
# function for trace plot
# ========================================================================


setMethod("traceplot", signature(x = "mcmc.list"),
  function (x, smooth = TRUE, col = 1:6, type = "l", ylab = "", ...)
{
  args <- list(...)
  for (j in 1:nvar(x)) {
    xp <- as.vector(time(x))
    yp <- if (nvar(x) > 1)
        x[, j, drop = TRUE]
    else x
    yp <- do.call("cbind", yp)
    matplot(xp, yp, xlab = "Iterations", ylab = ylab, type = type,
        col = col, ...)
    if (!is.null(varnames(x)) && is.null(list(...)$main))
      title(paste("Trace of", varnames(x)[j]))
    if (smooth) {
      scol <- rep(col, length = nchain(x))
      for (k in 1:nchain(x)) lines(lowess(xp, yp[, k]),
        col = scol[k])
    }
  }
}
)

##' @rdname traceplot
##' @export
setMethod("traceplot", signature(x = "rjags"),
  function( x, mfrow = c( 1, 1 ), varname = NULL,
  match.head = TRUE, ask = TRUE,
  col = rainbow( x$n.chains ),
  lty = 1, lwd = 1, ... )
{
  x <- x$BUGSoutput
  op <- par()$ask
  par( mfrow = mfrow )
  par( ask = ask )
  n.chain    <- x$n.chains
  n.keep     <- x$n.keep
  bugs.array <- x$sims.array
  varnamelist <- gsub( "\\[.*\\]","", dimnames( bugs.array )[[3]], fixed = FALSE )
  if( is.null( varname ) ){ varname <- ".*" }
  if( match.head ) { varname <- paste( "^", varname, sep="" ) }
  index      <- unlist( sapply( varname, function( x ){ grep( x, varnamelist ) } ) )
  n.var      <- length( index )
  for( j in index ) {
    range.x  <- c( 1, n.keep )
    range.y  <- range( bugs.array[,,j] )
    v.name   <- dimnames( bugs.array )[[3]][j]
    plot( range.x, range.y, type = "n", main = v.name,
            xlab = "iteration", ylab = v.name,
              xaxt  = "n", xaxs = "i", ... )
    for( i in 1:n.chain ) {
      x.cord <- 1:n.keep
      y.cord <- bugs.array[,i,j]
      lines( x.cord , y.cord , col = col[i], lty = lty, lwd = lwd )
    }
    axis( 1, at = seq(0, n.keep, n.keep*0.1), tick = TRUE )
  }
  on.exit(par(ask=op))
}
)

setMethod("traceplot", signature(x = "bugs"),
  function( x, mfrow = c( 1, 1 ), varname = NULL,
  match.head = TRUE, ask = TRUE,
  col = rainbow( x$n.chains ),
  lty = 1, lwd = 1, ... )
{
  par( mfrow = mfrow )
  par( ask = ask )
  n.chain    <- x$n.chains
  n.keep     <- x$n.keep
  bugs.array <- x$sims.array
  varnamelist <- gsub( "\\[.*\\]","", dimnames( bugs.array )[[3]], fixed = FALSE )
  if( is.null( varname ) ){ varname <- ".*" }
  if( match.head ) { varname <- paste( "^", varname, sep="" ) }
  index      <- unlist( sapply( varname, function( x ){ grep( x, varnamelist ) } ) )
  n.var      <- length( index )
  for( j in index ) {
    range.x  <- c( 1, n.keep )
    range.y  <- range( bugs.array[,,j] )
    v.name   <- dimnames( bugs.array )[[3]][j]
    plot( range.x, range.y, type = "n", main = v.name,
            xlab = "iteration", ylab = v.name,
              xaxt  = "n", xaxs = "i", ... )
    for( i in 1:n.chain ) {
      x.cord <- 1:n.keep
      y.cord <- bugs.array[,i,j]
      lines( x.cord , y.cord , col = col[i], lty = lty, lwd = lwd )
    }
    axis( 1, at = seq(0, n.keep, n.keep*0.1), tick = TRUE )
  }
}
)
