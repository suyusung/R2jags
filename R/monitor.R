## The convergence summary behind every output table of the package: for each
## parameter, the mean and sd of the kept draws, the five quantiles, and, when
## there is more than one chain, the potential scale reduction (Rhat) and an
## effective sample size.
##
## The estimator is the potential scale reduction of Gelman and Rubin (1992), as
## described in Gelman, Carlin, Stern and Rubin, *Bayesian Data Analysis*, and it
## is the one the package has always reported: tests/testthat/test-monitor.R
## pins the numbers.

monitor <- function(a, n.chains = dim(a)[2], trans = NULL, keep.all = FALSE,
                    Rupper.keep = FALSE) {
  draws <- if (length(dim(a)) == 2L) array(a, dim = c(dim(a), 1L)) else a
  ## The BUGS output arrives with both halves of the run kept and only the
  ## second half meant to be used, so unless the caller says otherwise the first
  ## half goes.
  if (!keep.all) {
    half <- floor(dim(draws)[1L] / 2)
    draws <- draws[seq.int(half + 1L, 2L * half), , , drop = FALSE]
  }
  parameter.names <- dimnames(draws)[[3L]]
  n.params <- dim(draws)[length(dim(draws))]

  ## Which scale each parameter is summarised on before Rhat is computed: Rhat is
  ## scale invariant, but a strictly positive parameter is better behaved on the
  ## log scale and a proportion on the logit scale.  The quantiles are brought
  ## back, so they are quantiles of the draws whichever scale was used.  A caller
  ## who knows better passes trans itself.
  if (is.null(trans)) {
    strictly.positive <- vapply(seq_len(n.params),
                                function(i) all(draws[, , i] > 0), NA)
    trans <- ifelse(strictly.positive, "log", "")
  }

  n.columns <- if (n.chains > 1L) if (Rupper.keep) 10L else 9L else 7L
  summary <- t(vapply(seq_len(n.params),
                      function(i) parameter.summary(draws[, , i, drop = FALSE],
                                                    n.chains, trans[i], Rupper.keep),
                      numeric(n.columns)))
  dimnames(summary) <- list(parameter.names,
                            if (n.chains > 1L)
                              c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%",
                                "Rhat", if (Rupper.keep) "Rupper", "n.eff")
                            else
                              c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%"))
  summary
}


## One row of that summary for a single parameter: its draws summarised as they
## are, plus the quantiles and, with several chains, Rhat and n.eff read off the
## scale chosen above and brought back to the draws' scale.
parameter.summary <- function(ai, n.chains, trans, Rupper.keep) {
  ## One parameter's draws: n.draws x n.chains, however the caller handed them
  ## over.  A single parameter sliced out of the array keeps a third dimension of
  ## one, which the calculations below do not need and which would make a
  ## length-1 array out of a chain that has no chains to compare with.
  ai <- matrix(ai, ncol = n.chains)
  if (trans == "log") {
    convergence <- conv.par(log(ai), n.chains, Rupper.keep = Rupper.keep)
    quantiles <- exp(convergence$quantiles)
  } else if (trans == "logit") {
    convergence <- conv.par(log(ai / (1 - ai)), n.chains, Rupper.keep = Rupper.keep)
    quantiles <- 1 / (1 + exp(-convergence$quantiles))
  } else {
    convergence <- conv.par(ai, n.chains, Rupper.keep = Rupper.keep)
    quantiles <- convergence$quantiles
  }
  c(mean(ai), sd(as.vector(ai)), quantiles,
    if (n.chains > 1L) convergence$confshrink,
    if (n.chains > 1L)
      round(convergence$n.eff, min(0, 1 - floor(log10(convergence$n.eff)))))
}


## The between- and within-chain calculation behind monitor(): the potential
## scale reduction and the effective sample size of one parameter, from a
## (draws x chains) matrix.  Both are the quantities of Gelman and Rubin (1992).
## Not exported -- monitor() is the only caller -- and deliberately not a
## general-purpose Rhat: it is the version the package has always reported.
conv.par <- function(x, n.chains, Rupper.keep = TRUE) {
  ## n.chains is what the caller says the chain count is and decides whether
  ## there is anything to compare at all; m is what the matrix actually holds.
  if (length(dim(x)) > 2L) x <- matrix(x, ncol = dim(x)[2L])
  n.draws <- nrow(x)
  m <- ncol(x)

  chain.means <- apply(x, 2L, mean)
  chain.vars <- apply(x, 2L, var)
  grand.mean <- mean(chain.means)
  within <- mean(chain.vars)
  quantiles <- quantile(as.vector(x), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

  ## One chain, or draws that never vary, carry no between-chain information.
  if (is.na(within) || within <= 1e-8 || n.chains < 2L)
    return(list(quantiles = quantiles,
                confshrink = rep(1, Rupper.keep + 1),
                n.eff = 1))

  ## The between-chain mean square, how it and the within-chain one are
  ## estimated, and the pooled variance they give.
  between <- n.draws * var(chain.means)
  var.within <- var(chain.vars) / m
  var.between <- between^2 * 2 / (m - 1)
  cov.wb <- (n.draws / m) *
    (var(chain.vars, chain.means^2) - 2 * grand.mean * var(chain.vars, chain.means))
  pooled <- ((n.draws - 1) * within + between) / n.draws

  ## The variance of the pooled estimate carries the sampling variance of the
  ## grand mean on top of the pooled variance, and how well that variance is
  ## itself estimated sets the degrees of freedom of the t interval that widens
  ## Rhat below.
  var.pooled <- pooled + between / (m * n.draws)
  var.var.pooled <- max(0, (((n.draws - 1)^2) * var.within +
                            (1 + 1 / m)^2 * var.between +
                            2 * (n.draws - 1) * (1 + 1 / m) * cov.wb) /
                          n.draws^2)
  df.pooled <- min(2 * (var.pooled^2 / var.var.pooled), 1000)

  ## Rhat is the pooled variance over the within-chain variance.  The upper
  ## bound, which the caller can ask for, comes from reading between/within as an
  ## F statistic; nobody uses it.
  rhat <- var.pooled / within
  if (Rupper.keep) {
    df.within <- 2 * (within^2 / var.within)
    rhat <- c(rhat, (n.draws - 1) / n.draws +
                    (1 + 1 / m) * (1 / n.draws) * (between / within) *
                    qf(0.975, m - 1, df.within))
  }
  rhat <- sqrt(rhat * (df.pooled + 3) / (df.pooled + 1))

  ## A crude sample size: it leans on the between-chain mean square, which is
  ## only estimated with as many degrees of freedom as there are chains.
  list(quantiles = quantiles, confshrink = rhat,
       n.eff = m * n.draws * min(pooled / between, 1))
}
