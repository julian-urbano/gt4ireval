#' G-study (Generalizability)
#'
#' \code{gstudy} runs a G-study with the given data, assuming a fully crossed design (all systems
#' evaluated on the same queries). It can be used to estimate variance components, possibly with
#' confidence intervals, which can further be used to run a D-study with \code{\link{dstudy}}.
#' Confidence intervals are computed with the Jacknife procedure described in sections 6.1.2 and
#' 6.2.4 of Brennan (2001).
#'
#' @param data A data frame or matrix with the existing effectiveness scores. Systems are columns
#'   and queries are rows.
#' @param ensurePositive Whether to set negative variance estimates to zero or not. Defaults to
#'   \code{FALSE}.
#' @param alpha A vector of confidence levels to compute intervals for the variance components. This
#'   is the probability on each side of the interval, so for a 90\% confidence interval one must set
#'   \code{alpha} to 0.05. When set to \code{NULL}, no intervals are computed.
#' @param drop The fraction of worst-performing systems to drop from the data before analysis.
#'   Defaults to 0 (include all systems).
#'
#' @return An object of class \code{gstudy}, with the following components: \tabular{ll}{
#'   \code{n.s}, \code{n.q} \tab Number of systems and number of queries of the existing data. \cr
#'   \code{var.s}, \code{var.q}, \code{var.e} \tab Variance of the system, query, and residual
#'   effects. \cr \code{em.s}, \code{em.q}, \code{em.e} \tab Mean squares of the system, query and
#'   residual components. \cr \code{call} \tab A list with the existing \code{data} and the
#'   percentage of systems to \code{drop}.\cr }
#'
#' @seealso \code{\link{dstudy}}
#' @author Julián Urbano
#' @references R.L. Brennan (2001). Generalizability Theory. Springer.
#'
#'   L.J. Cronbach, G.C. Gleser, H. Nanda and N. Rajaratnam (1972). The dependability of behavioral
#'   measurements: Theory of generalizability for scores and profiles. Wiley. (pp. 54-57, 66,
#'   70-72).
#'
#'   J. Urbano, M. Marrero and D. Martín (2013). On the Measurement of Test Collection Reliability.
#'   ACM SIGIR, pp. 393-402.
#'
#' @examples
#' g <- gstudy(adhoc3)
#'
#' # same, but drop the 20% worst systems
#' g20 <- gstudy(adhoc3, drop = 0.2)
#'
#' # compute 90% and 95% confidence intervals
#' g <- gstudy(adhoc3, alpha = c(.05, 0.025))
#' @export
#' @importFrom stats quantile
gstudy <- function(data, ensurePositive = TRUE, alpha = 0.025, drop = 0) {
  if(drop < 0 || drop >= 1)
    stop("Fraction of worst systems to drop must be in [0,1)")
  if(!is.data.frame(data) && !is.matrix(data))
    stop("Data must be a data.frame or matrix (columns for systems and rows for queries)")
  if(!is.null(alpha) && any(alpha <= 0 | alpha >= 0.5))
    stop("Significance level must be in (0,0.5)")

  # drop bottom results as indicated
  data <- data[, colMeans(data) >= quantile(colMeans(data), drop)]

  # sizes and means
  n.s <- ncol(data)
  means.s <- colMeans(data)
  n.q <- nrow(data)
  means.q <- rowMeans(data)
  means.all <- mean(means.s)

  # sums of squares and mean squares
  ss.s <- sum((means.s - means.all)^2) * n.q
  ss.q <- sum((means.q - means.all)^2) * n.s
  ss.e <- sum((data - means.all)^2) - ss.s - ss.q
  em.s <- ss.s / (n.s - 1)
  em.q <- ss.q / (n.q - 1)
  em.e <- ss.e / (n.s - 1) / (n.q - 1)

  # variance components
  var.s <- (em.s - em.e) / n.q
  var.q <- (em.q - em.e) / n.s
  var.e <- em.e
  if(var.s < 0)
    warning("Estimate of system variance is negative")
  if(var.q < 0)
    warning("Estimate of query variance is negative")
  if(var.e < 0)
    warning("Estimate of interaction variance is negative")

  # interval estimates: 6.1.2 and 6.2.4 in Brennan 2001.
  if(!is.null(alpha)) {
    theta.s <- matrix(NA, ncol = n.s, nrow = n.q)
    theta.q <- matrix(NA, ncol = n.s, nrow = n.q)
    theta.e <- matrix(NA, ncol = n.s, nrow = n.q)

    suppressWarnings(theta.00 <- gstudy(data, ensurePositive = FALSE, alpha = NULL))
    for(s in 1:n.s) {
      suppressWarnings(theta.s0 <- gstudy(data[,-s], ensurePositive = FALSE, alpha = NULL))
      for(q in 1:n.q) {
        suppressWarnings(theta.0q <- gstudy(data[-q,], ensurePositive = FALSE, alpha = NULL))
        suppressWarnings(theta.sq <- gstudy(data[-q, -s], ensurePositive = FALSE, alpha = NULL))

        theta.s[q, s] <- n.s * n.q * theta.00$var.s - (n.s - 1) * n.q * theta.s0$var.s -
          n.s * (n.q - 1) * theta.0q$var.s + (n.s - 1) * (n.q - 1) * theta.sq$var.s
        theta.q[q, s] <- n.s * n.q * theta.00$var.q - (n.s - 1) * n.q * theta.s0$var.q -
          n.s * (n.q - 1) * theta.0q$var.q + (n.s - 1) * (n.q - 1) * theta.sq$var.q
        theta.e[q, s] <- n.s * n.q * theta.00$var.e - (n.s - 1) * n.q * theta.s0$var.e -
          n.s * (n.q - 1) * theta.0q$var.e + (n.s - 1) * (n.q - 1) * theta.sq$var.e
      }
    }
    suppressWarnings(g <- gstudy(theta.s, drop = 0, ensurePositive = FALSE, alpha = NULL))
    se <- sqrt(g$var.s / n.s + g$var.q / n.q + g$var.e / n.s / n.q)
    var.s.lwr <- var.s - qt(1 - alpha, df = n.s * n.q - 1) * se
    var.s.upr <- var.s + qt(1 - alpha, df = n.s * n.q - 1) * se
    suppressWarnings(g <- gstudy(theta.q, drop = 0, ensurePositive = FALSE, alpha = NULL))
    se <- sqrt(g$var.s / n.s + g$var.q / n.q + g$var.e / n.s / n.q)
    var.q.lwr <- var.q - qt(1 - alpha, df = n.s * n.q - 1) * se
    var.q.upr <- var.q + qt(1 - alpha, df = n.s * n.q - 1) * se
    suppressWarnings(g <- gstudy(theta.e, drop = 0, ensurePositive = FALSE, alpha = NULL))
    se <- sqrt(g$var.s / n.s + g$var.q / n.q + g$var.e / n.s / n.q)
    var.e.lwr <- var.e - qt(1 - alpha, df = n.s * n.q - 1) * se
    var.e.upr <- var.e + qt(1 - alpha, df = n.s * n.q - 1) * se
  }

  if(ensurePositive) {
    var.s <- max(0, var.s)
    var.q <- max(0, var.q)
    var.e <- max(0, var.e)
    if(!is.null(alpha)) {
      var.s.lwr <- max(0, var.s.lwr)
      var.s.upr <- max(0, var.s.upr)
      var.q.lwr <- max(0, var.q.lwr)
      var.q.upr <- max(0, var.q.upr)
      var.e.lwr <- max(0, var.e.lwr)
      var.e.upr <- max(0, var.e.upr)
    }
  }

  res <- list(
    n.s = n.s, n.q = n.q,
    var.s = var.s, var.q = var.q, var.e = var.e,
    em.s = em.s, em.q = em.q, em.e = em.e,
    call = list(data = data, drop = drop, ensurePositive = ensurePositive)
  )
  if(!is.null(alpha)) {
    res$var.s.lwr = var.s.lwr
    res$var.s.upr = var.s.upr
    res$var.q.lwr = var.q.lwr
    res$var.q.upr = var.q.upr
    res$var.e.lwr = var.e.lwr
    res$var.e.upr = var.e.upr
  }
  class(res) <- "gstudy"
  return(res)
}

#' @export
print.gstudy <- function(x, ...) {
  cat("\nSummary of G-Study\n")
  cat("\n                 Systems     Queries Interaction\n")
  cat("             ----------- ----------- -----------\n")
  cat(sep = "", "Variance    ",
      sprintf("%*.5g", 12, x$var.s),
      sprintf("%*.5g", 12, x$var.q),
      sprintf("%*.5g", 12, x$var.e), "\n")
  varsum <- (x$var.s + x$var.q + x$var.e) / 100
  cat(sep = "", "Variance(%) ",
      sprintf("%*.5g", 12, x$var.s / varsum),
      sprintf("%*.5g", 12, x$var.q / varsum),
      sprintf("%*.5g", 12, x$var.e / varsum), "\n")
  cat("---\n")
  cat(sep = "", "Mean Sq.    ",
      sprintf("%*.5g", 12, x$em.s),
      sprintf("%*.5g", 12, x$em.q),
      sprintf("%*.5g", 12, x$em.e), "\n")
  cat(sep = "", "Sample size ",
      sprintf("%*.5g", 12, x$n.s),
      sprintf("%*.5g", 12, x$n.q),
      sprintf("%*.5g", 12, x$n.s * x$n.q), "\n\n")
}
