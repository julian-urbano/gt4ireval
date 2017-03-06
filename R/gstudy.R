#' G-study (Generalizability)
#'
#' \code{gstudy} runs a G-study with the given data, assuming a fully crossed design (all systems
#' evaluated on the same queries). It can be used to estimate variance components, which can further
#' be used to run a D-study with \code{\link{dstudy}}.
#'
#' @param data A data frame or matrix with the existing effectiveness scores. Systems are columns
#'   and queries are rows.
#' @param drop The fraction of worst-performing systems to drop from the data before analysis.
#'   Defaults to 0 (include all systems).
#'
#' @return An object of class \code{gstudy}, with the following components:
#' \tabular{ll}{
#' \code{n.s}, \code{n.q} \tab Number of systems and number of queries of the existing data. \cr
#' \code{var.s}, \code{var.q}, \code{var.e} \tab Variance of the system, query, and residual
#'   effects. \cr
#' \code{em.s}, \code{em.q}, \code{em.e} \tab Mean squares of the system, query and residual
#'   components. \cr
#' \code{call} \tab A list with the existing \code{data} and the percentage of systems to
#'   \code{drop}.\cr
#' }
#'
#' @seealso \code{\link{dstudy}}
#' @author Julián Urbano
#' @references R.L. Brennan (2001). Generalizability Theory. Springer.
#'
#'   J. Urbano, M. Marrero and D. Martín (2013). On the Measurement of Test Collection Reliability.
#'   ACM SIGIR, pp. 393-402.
#'
#' @examples
#' g <- gstudy(adhoc3)
#'
#' # same, but drop the 20% worst systems
#' g20 <- gstudy(adhoc3, drop = 0.2)
#' @export
#' @importFrom stats quantile
gstudy <- function(data, drop = 0) {
  if(drop < 0 || drop >= 1)
    stop("Fraction of worst systems to drop must be in [0,1)")
  if(!is.data.frame(data) && !is.matrix(data))
    stop("Data must be a data.frame or matrix (columns for systems and rows for queries)")

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
  if(var.s < 0){
    var.s <- 0
    warning("Estimate of system variance is negative")
  }
  if(var.q < 0){
    var.q <- 0
    warning("Estimate of query variance is negative")
  }
  if(var.e < 0){
    var.e <- 0
    warning("Estimate of interaction variance is negative")
  }

  res <- list(
    n.s = n.s, n.q = n.q,
    var.s = var.s, var.q = var.q, var.e = var.e,
    em.s = em.s, em.q = em.q, em.e = em.e,
    call = list(data = data, drop = drop)
  )
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
