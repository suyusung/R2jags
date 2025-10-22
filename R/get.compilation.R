##' Retrives the information from the JAGS compilation
##' 
##' Retrives the information from the JAGS compilation
##' 
##' 
##' @param x A 'rjags' object (obtained from running \code{R2jags::jags}) or a 
##' 'jags' object (resulting from running \code{rjags::jags.samples}.
##' @param ... Additional parameters that can be passed
##' @return A vector with the number of observed, unobserved and total nodes
##' included in the DAG resulting from the model assumptions encoded by the
##' code
##' @author Gianluca Baio
##' @examples
##' 
##' 
##' # An example model file is given in:
##' model.file <- system.file(package="R2jags", "model", "schools.txt")
##' #=================# 
##' # initialization  #
##' #=================#
##' # data
##' J <- 8.0
##' y <- c(28.4,7.9,-2.8,6.8,-0.6,0.6,18.0,12.2)
##' sd <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)
##' jags.data <- list("y","sd","J")
##' jags.params <- c("mu","sigma","theta")
##' jags.inits <- function(){
##'  list("mu"=rnorm(1),"sigma"=runif(1),"theta"=rnorm(J))
##' }
##' jagsfit <- jags(data=list("y","sd","J"), inits=jags.inits, jags.params,
##'              n.iter=10, model.file=model.file)
##' count_nodes(jagsfit)
##' 
##' 
##' 
count_nodes=function(x,...) {
  # First checks what type of object we have here. Could be a 'jags' (as from
  # the 'rjags' package), or, most likely, a 'rjags' object (from 'R2jags')
  if(class(x)=="jags") {
    x=list(model=x)
  } else if (class(x)=="rjags") {
    x=x
  } else {
    stop("The input x must be either a 'jags' (processed by the package 'rjags') or an 'rjags' (processed by the package 'R2jags') object.")
  }
  temp_file=tempfile(fileext=".jag")
  sink(temp_file)
  x$model$recompile()
  sink()
  output=readLines(temp_file)
  obs_nodes=as.numeric(sub(".*Observed stochastic nodes: ([0-9]+).*", "\\1", output[grep("Observed stochastic nodes", output)]))
  unobs_nodes=as.numeric(sub(".*Unobserved stochastic nodes: ([0-9]+).*", "\\1", output[grep("Unobserved stochastic nodes", output)]))
  graph_size=as.numeric(sub(".*Total graph size: ([0-9]+).*", "\\1", output[grep("Total graph size", output)]))
  unlink(temp_file)
  list(
    observed_stochastic_nodes=obs_nodes,
    unobserved_stochastic_nodes=unobs_nodes,
    total_size=graph_size
  )
}

