## The convergence summary every output table of this package is built from:
## the mean and sd of the kept draws, the five quantiles, and, for more than one
## chain, the Gelman-Rubin potential scale reduction (Rhat) and an effective
## sample size.
##
## Until 0.9-0 these two functions were imported from R2WinBUGS, which was then
## a hard dependency of the package for their sake alone (issue #3).  The
## estimator is the one in Gelman and Rubin (1992) and in Gelman, Carlin, Stern
## and Rubin, *Bayesian Data Analysis* -- the same one R2WinBUGS::monitor
## computes (Sturtz, Ligges and Gelman 2005).  The S-PLUS branches are gone and
## the numbers are unchanged: tests/testthat/test-monitor.R compares them with
## R2WinBUGS's own, and the test skips if that package is not installed.

monitor <- function(a, n.chains = dim(a)[2], trans = NULL, keep.all = FALSE,
                    Rupper.keep = FALSE) {
  ## `a` is a (n.sims x n.chains x n.params) array of draws.  With
  ## keep.all = FALSE the first half is discarded first: that is the shape the
  ## BUGS output arrives in, where both halves are kept but only the second is
  ## meant to be used.
  if (length(dim(a)) == 2) a <- array(a, c(dim(a), 1))
  n.params <- dim(a)[length(dim(a))]
  if (!keep.all) {
    n <- floor(dim(a)[1] / 2)
    a <- a[(n + 1):(2 * n), , , drop = FALSE]
  }

  ## trans is a vector of length n.params saying which scale each parameter is
  ## summarised on for Rhat: Rhat is scale invariant but behaves better on the
  ## log scale for a positive parameter and on the logit scale for a proportion.
  ## Left NULL, a parameter whose kept draws are all positive gets "log".  The
  ## quantiles are transformed back, so they are the ones the draws have
  ## whichever scale was used.
  if (is.null(trans)) trans <- ifelse(apply(a <= 0, 3, sum) == 0, "log", "")

  output <- matrix(NA_real_, nrow = n.params,
                   ncol = if (n.chains > 1) if (Rupper.keep) 10 else 9 else 7)
  for (i in seq_len(n.params)) {
    ai <- a[, , i, drop = FALSE]
    if (trans[i] == "log") {
      cp <- conv.par(log(ai), n.chains, Rupper.keep = Rupper.keep)
      quantiles <- exp(cp$quantiles)
    } else if (trans[i] == "logit") {
      cp <- conv.par(log(ai / (1 - ai)), n.chains, Rupper.keep = Rupper.keep)
      quantiles <- 1 / (1 + exp(-cp$quantiles))
    } else {
      cp <- conv.par(ai, n.chains, Rupper.keep = Rupper.keep)
      quantiles <- cp$quantiles
    }
    output[i, ] <- c(mean(ai), sd(as.vector(ai)), quantiles,
                     if (n.chains > 1) cp$confshrink,
                     if (n.chains > 1) round(cp$n.eff, min(0, 1 - floor(log10(cp$n.eff)))))
  }
  dimnames(output) <- list(dimnames(a)[[3]],
                           if (n.chains > 1)
                             c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%",
                               "Rhat", if (Rupper.keep) "Rupper", "n.eff")
                           else
                             c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%"))
  output
}


## The between/within-chain calculation behind monitor(): Rhat and n.eff from an
## (n.sims x n.chains) matrix of one parameter.  Not exported -- monitor() is its
## only caller -- and not a general-purpose Rhat: it is the BUGS version, kept
## identical to the one the package has always reported.
conv.par <- function(x, n.chains, Rupper.keep = TRUE) {
  m <- ncol(x)
  n <- nrow(x)

  ## xdot: sequence means;  s2: sequence variances;  W: mean of s2 (within MS);
  ## B: n * var(xdot) (between MS);  muhat: grand mean;  sig2hat: the pooled
  ## estimate of the variance.  The posterior interval combines both
  ## uncertainties in a t interval, which is what the correction to Rhat below
  ## accounts for.
  xdot <- apply(x, 2, mean)
  muhat <- mean(xdot)
  s2 <- apply(x, 2, var)
  W <- mean(s2)
  quantiles <- quantile(as.vector(x), probs = c(.025, .25, .5, .75, .975))

  if (!is.na(W) && (W > 1e-8) && (n.chains > 1)) {   # non-degenerate case
    B <- n * var(xdot)
    varW <- var(s2) / m
    varB <- B^2 * 2 / (m - 1)
    covWB <- (n / m) * (var(s2, xdot^2) - 2 * muhat * var(s2, xdot))
    sig2hat <- ((n - 1) * W + B) / n

    postvar <- sig2hat + B / (m * n)   # B/(mn) is the sampling variance of muhat
    varpostvar <- max(0,
        (((n - 1)^2) * varW + (1 + 1/m)^2 * varB + 2 * (n - 1) * (1 + 1/m) * covWB) / n^2)
    post.df <- min(2 * (postvar^2 / varpostvar), 1000)

    ## Rhat = sqrt(postvar/W), times sqrt((df+3)/(df+1)) for the width of the
    ## t interval; the approximation of (B/W) by an F distribution is what the
    ## upper bound (Rupper) is for, and nobody uses it.
    confshrink.range <- postvar / W
    if (Rupper.keep) {
      varlo.df <- 2 * (W^2 / varW)
      confshrink.range <- c(confshrink.range,
          (n - 1)/n + (1 + 1/m) * (1/n) * (B/W) * qf(.975, m - 1, varlo.df))
    }
    confshrink.range <- sqrt(confshrink.range * (post.df + 3) / (post.df + 1))

    ## n.eff = m*n*min(sig2hat/B, 1), a crude measure because B is estimated
    ## with only m degrees of freedom.
    n.eff <- m * n * min(sig2hat / B, 1)
    list(quantiles = quantiles, confshrink = confshrink.range, n.eff = n.eff)

  } else {   # degenerate case: every entry of the data matrix is identical
    list(quantiles = quantiles, confshrink = rep(1, Rupper.keep + 1), n.eff = 1)
  }
}
