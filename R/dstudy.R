## Copyright (C) 2013-2017  Julián Urbano <urbano.julian@gmail.com>
## Distributed under the terms of the MIT License.

dstudy <- function(gdata, queries = gdata$n.q, stability = 0.95, alpha = 0.025) {
  if(!inherits(gdata, "gstudy"))
    stop("gdata is not a valid g-study object.")
  if(length(alpha) > 1 && length(queries) > 1)
    stop("Only one of 'alpha' and 'queries' may have multiple values")
  if(length(alpha) > 1 && length(stability) > 1)
    stop("Only one of 'alpha' and 'stability' may have multiple values")
  if(any(queries < 1))
    stop("Number of queries must be one or more")
  if(any(stability <= 0 | stability >= 1))
    stop("Target stability must be in (0,1)")
  if(any(alpha <= 0 | alpha >= 1))
    stop("Significance level must be in (0,1)")

  # Point estimates for the indicated n.q
  Erho2_ <- gdata$var.s / (gdata$var.s + gdata$var.e / queries)
  n.q_Erho2 <- ceiling((stability * gdata$var.e) / (gdata$var.s * (1 - stability)))

  Phi_ <- gdata$var.s / (gdata$var.s + (gdata$var.q + gdata$var.e) / queries)
  n.q_Phi <- ceiling((stability * (gdata$var.q + gdata$var.e)) / (gdata$var.s * (1 - stability)))

  # Interval estimates
  df.s <- gdata$n.s - 1
  df.q <- gdata$n.q - 1
  df.e <- df.s * df.q

  Erho2.lwr <- gdata$em.s / (gdata$em.e * qf(1-alpha, df.s, df.e))
  Erho2.lwr <- (Erho2.lwr - 1) / gdata$n.q
  n.q_Erho2.lwr <- ceiling(stability / (Erho2.lwr * (1 - stability)))
  Erho2.lwr <- queries * Erho2.lwr / (1 + queries * Erho2.lwr)

  Erho2.upr <- gdata$em.s / (gdata$em.e * qf(alpha, df.s, df.e))
  Erho2.upr <- (Erho2.upr - 1) / gdata$n.q
  n.q_Erho2.upr <- ceiling(stability / (Erho2.upr * (1 - stability)))
  Erho2.upr <- queries * Erho2.upr / (1 + queries * Erho2.upr)

  Phi.lwr <- gdata$em.s^2 - qf(1-alpha, df.s, Inf) * gdata$em.s * gdata$em.e
  Phi.lwr <- Phi.lwr + (qf(1-alpha, df.s, Inf) - qf(1-alpha, df.s, df.e)) *
    qf(1-alpha, df.s, df.e) * gdata$em.e^2
  Phi.lwr <- Phi.lwr / ( (gdata$n.s - 1) * qf(1-alpha, df.s, Inf) * gdata$em.s * gdata$em.e +
                           qf(1-alpha, df.s, df.q) * gdata$em.s * gdata$em.q )
  Phi.lwr <- gdata$n.s * Phi.lwr / (gdata$n.s * Phi.lwr + gdata$n.q)
  n.q_Phi.lwr <- ceiling(stability * (1 - Phi.lwr) / (Phi.lwr * (1 - stability)))
  Phi.lwr <- queries * Phi.lwr / (1 + (queries - 1) * Phi.lwr)

  Phi.upr <- gdata$em.s^2 - qf(alpha, df.s, Inf) * gdata$em.s * gdata$em.e
  Phi.upr <- Phi.upr + (qf(alpha, df.s, Inf) - qf(alpha, df.s, df.e)) *
    qf(alpha, df.s, df.e) * gdata$em.e^2
  Phi.upr <- Phi.upr / ( (gdata$n.s - 1) * qf(alpha, df.s, Inf) * gdata$em.s * gdata$em.e +
                           qf(alpha, df.s, df.q) * gdata$em.s * gdata$em.q )
  Phi.upr <- gdata$n.s * Phi.upr / (gdata$n.s * Phi.upr + gdata$n.q)
  n.q_Phi.upr <- ceiling(stability * (1 - Phi.upr) / (Phi.upr * (1 - stability)))
  Phi.upr <- queries * Phi.upr / (1 + (queries - 1) * Phi.upr)

  res <- list(
    Erho2 = Erho2_, Phi = Phi_,
    n.q_Erho2 = n.q_Erho2, n.q_Phi = n.q_Phi,
    Erho2.lwr = Erho2.lwr, Erho2.upr = Erho2.upr,
    Phi.lwr = Phi.lwr, Phi.upr = Phi.upr,
    n.q_Erho2.lwr = n.q_Erho2.lwr, n.q_Erho2.upr = n.q_Erho2.upr,
    n.q_Phi.lwr = n.q_Phi.lwr, n.q_Phi.upr = n.q_Phi.upr,
    call = list(gstudy = gdata,
                queries = queries, stability = stability, alpha = alpha
    )
  )
  class(res) <- "dstudy"
  return(res)
}

print.dstudy <- function(x, ...) {
  cat("\nSummary of D-Study\n")
  cat("\nCall:\n")
  cat("    queries =", x$call$queries, "\n")
  cat("  stability =", x$call$stability, "\n")
  cat("      alpha =", x$call$alpha, "\n")

  # Stability
  cat("\nStability:\n")
  if(length(x$call$alpha) > 1){
    cat("                                           Erho2                                   Phi\n")
    cat("             -----------------------------------   -----------------------------------\n")
    cat("       Alpha    Expected       Lower       Upper      Expected       Lower       Upper\n")
    cat(" ----------- ----------- ----------- -----------   ----------- ----------- -----------\n")
    for(i in seq_along(x$call$alpha))
      cat("", sprintf("%*.5g", 11, x$call$alpha[i]),
          sprintf("%*.5g", 11, x$Erho2[1]),
          sprintf("%*.5g", 11, x$Erho2.lwr[i]),
          sprintf("%*.5g", 11, x$Erho2.upr[i]),
          " ",
          sprintf("%*.5g", 11, x$Phi[1]),
          sprintf("%*.5g", 11, x$Phi.lwr[i]),
          sprintf("%*.5g", 11, x$Phi.upr[i]), "\n")
  }else{
    cat("                                           Erho2                                   Phi\n")
    cat("             -----------------------------------   -----------------------------------\n")
    cat("     Queries    Expected       Lower       Upper      Expected       Lower       Upper\n")
    cat(" ----------- ----------- ----------- -----------   ----------- ----------- -----------\n")
    for(i in seq_along(x$call$queries))
      cat("", sprintf("%*.5g", 11, x$call$queries[i]),
          sprintf("%*.5g", 11, x$Erho2[i]),
          sprintf("%*.5g", 11, x$Erho2.lwr[i]),
          sprintf("%*.5g", 11, x$Erho2.upr[i]),
          " ",
          sprintf("%*.5g", 11, x$Phi[i]),
          sprintf("%*.5g", 11, x$Phi.lwr[i]),
          sprintf("%*.5g", 11, x$Phi.upr[i]), "\n")
  }

  # Number of queries
  cat("\nRequired number of queries:\n")
  if(length(x$call$alpha) > 1){
    cat("                                           Erho2                                   Phi\n")
    cat("             -----------------------------------   -----------------------------------\n")
    cat("       Alpha    Expected       Lower       Upper      Expected       Lower       Upper\n")
    cat(" ----------- ----------- ----------- -----------   ----------- ----------- -----------\n")
    for(i in seq_along(x$call$alpha))
      cat("", sprintf("%*.5g", 11, x$call$alpha[i]),
          sprintf("%*.5g", 11, x$n.q_Erho2[1]),
          sprintf("%*.5g", 11, x$n.q_Erho2.upr[i]),
          sprintf("%*.5g", 11, x$n.q_Erho2.lwr[i]),
          " ",
          sprintf("%*.5g", 11, x$n.q_Phi[1]),
          sprintf("%*.5g", 11, x$n.q_Phi.upr[i]),
          sprintf("%*.5g", 11, x$n.q_Phi.lwr[i]), "\n")
  }else{
    cat("                                           Erho2                                   Phi\n")
    cat("             -----------------------------------   -----------------------------------\n")
    cat("   Stability    Expected       Lower       Upper      Expected       Lower       Upper\n")
    cat(" ----------- ----------- ----------- -----------   ----------- ----------- -----------\n")
    for(i in seq_along(x$call$stability))
      cat("", sprintf("%*.5g", 11, x$call$stability[i]),
          sprintf("%*.5g", 11, x$n.q_Erho2[i]),
          sprintf("%*.5g", 11, x$n.q_Erho2.upr[i]),
          sprintf("%*.5g", 11, x$n.q_Erho2.lwr[i]),
          " ",
          sprintf("%*.5g", 11, x$n.q_Phi[i]),
          sprintf("%*.5g", 11, x$n.q_Phi.upr[i]),
          sprintf("%*.5g", 11, x$n.q_Phi.lwr[i]), "\n")
  }
}
