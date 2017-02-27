## Copyright (C) 2013-2017  Julián Urbano <urbano.julian@gmail.com>
## Distributed under the terms of the MIT License.

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
