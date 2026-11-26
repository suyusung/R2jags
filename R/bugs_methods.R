## S3 methods for objects of class "bugs" -- the list that jags() stores in
## the BUGSoutput component of its result (and that mcmc2bugs() and
## jags2bugs() build). These methods come from R2WinBUGS (Sturtz, Ligges and
## Gelman 2005, Journal of Statistical Software 12(3)), which registered them
## globally; until 0.9-0 R2jags always loaded R2WinBUGS through a Depends and
## they were therefore available in any R2jags session. Dropping the
## dependency without re-registering them broke user code that calls
## coda::as.mcmc.list(fit$BUGSoutput) -- reported by the CRAN reverse-dependency
## pretest through mvnma. The implementations are the R2WinBUGS ones, with the
## S-PLUS branches dropped, as for the other ported functions (see monitor()).

##' @title Extract an mcmc.list from a bugs object
##' @description S3 method for \code{\link[coda]{as.mcmc.list}}: turns the
##' kept draws of an object of class \code{"bugs"} into an
##' \code{\link[coda]{mcmc.list}}, one \code{\link[coda]{mcmc}} per chain.
##' @param x an object of class \code{"bugs"}, as stored in the
##' \code{BUGSoutput} component of a \code{\link{jags}()} result.
##' @param \dots further arguments passed to methods.
##' @return An \code{\link[coda]{mcmc.list}} with \code{x$n.chains} chains.
##' @export
##' @keywords internal
as.mcmc.list.bugs <- function(x, ...)
{
    if (!inherits(x, "bugs"))
        stop("Method as.mcmc.list.bugs() is only intended for bugs objects.")
    if (dim(x$sims.array)[2] != x$n.chains)
        stop("Inconsistancy in bug object regarding the number of chains.")
    mclis <- vector("list", x$n.chains)
    strt <- x$n.burnin + 1
    end <- x$n.iter
    ord <- order(dimnames(x$sims.array)[[3]])
    for (i in 1:x$n.chains) {
        tmp1 <- x$sims.array[, i, ord]
        mclis[[i]] <- mcmc(tmp1, start = strt, end = end, thin = x$n.thin)
    }
    as.mcmc.list(mclis)
}

##' @title Print a bugs object
##' @description S3 method for \code{\link[base]{print}}: shows the model,
##' the sampling scheme, the summary table and (when available) the DIC
##' information of an object of class \code{"bugs"}.
##' @param x an object of class \code{"bugs"}, as stored in the
##' \code{BUGSoutput} component of a \code{\link{jags}()} result.
##' @param digits.summary number of decimal places for the summary table.
##' @param \dots further arguments passed to \code{print} for the summary.
##' @return \code{x}, invisibly.
##' @export
##' @keywords internal
print.bugs <- function(x, digits.summary = 1, ...)
{
    if (!is.null(x$model.file))
        cat("Inference for Bugs model at \"", x$model.file, "\", ", sep = "")
    if (!is.null(x$program))
        cat("fit using ", x$program, ",", sep = "")
    cat("\n ", x$n.chains, " chains, each with ", x$n.iter,
        " iterations (first ", x$n.burnin, " discarded)", sep = "")
    if (x$n.thin > 1)
        cat(", n.thin =", x$n.thin)
    cat("\n n.sims =", x$n.sims, "iterations saved\n")
    print(round(x$summary, digits.summary), ...)

    if (x$n.chains > 1) {
        cat("\nFor each parameter, n.eff is a crude measure of effective sample size,")
        cat("\nand Rhat is the potential scale reduction factor (at convergence, Rhat=1).\n")
    }

    if (x$isDIC) {
        msgDICRule <- ifelse(x$DICbyR,
                             "(using the rule: pV = var(deviance)/2)",
                             "(using the rule, pD = Dbar-Dhat)")
        cat(paste("\nDIC info ", msgDICRule, "\n", sep = ""))
        if (length(x$DIC) == 1) {
            cat("pV =", fround(x$pV, 1), "and DIC =", fround(x$DIC, 1))
        } else if (length(x$DIC) > 1) {
            print(round(x$DIC, 1))
        }
        if (!is.null(x$pD)) {
            cat("\nDIC info (using the rule: pD = Dbar-Dhat, computed via 'rjags::dic.samples')")
            cat(paste("\npD =", fround(x$pD, 1), "and DIC =", fround(x$DIC2, 1)))
        }
        cat("\nDIC is an estimate of expected predictive error (lower deviance is better).\n")
    }
    invisible(x)
}
