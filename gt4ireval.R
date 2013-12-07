## Copyright (C) 2013  Julián Urbano <urbano.julian@gmail.com>
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see http://www.gnu.org/licenses/.

g.study <- function(data, drop = 0) {
	# drop bottom results as indicated
	t <- data[colMeans(data) >= quantile(colMeans(data), drop)]

	# sizes and means
	n.s <- length(t)
	means.s <- apply(t, 2, mean)
	n.q <- nrow(t)
	means.q <- apply(t, 1, mean)
	means.all <- mean(means.s)

	# sums of squares and mean squares
	ss.s <- sum((means.s - means.all)^2) * n.q
	ss.q <- sum((means.q - means.all)^2) * n.s
	ss.e <- sum((t - means.all)^2) - ss.s - ss.q
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
		em.s = em.s, em.q = em.q, em.e = em.e
	)
	class(res) <- "gstudy"
	return(res)
}

d.study <- function(gdata, queries = gdata$n.q, stability = 0.95, alpha = 0.025) {
	if(length(alpha) > 1 & length(queries) > 1)
		stop("Only one of \"alpha\" and \"queries\" may have multiple values")
	if(length(alpha) > 1 & length(stability) > 1)
		stop("Only one of \"alpha\" and \"stability\" may have multiple values")

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
	Phi.lwr <- Phi.lwr / ( (gdata$n.s - 1) * qf(1-alpha, df.s, Inf,) * gdata$em.s * gdata$em.e + 
		qf(1-alpha, df.s, df.q) * gdata$em.s * gdata$em.q )
	Phi.lwr <- gdata$n.s * Phi.lwr / (gdata$n.s * Phi.lwr + gdata$n.q)
	n.q_Phi.lwr <- ceiling(stability * (1 - Phi.lwr) / (Phi.lwr * (1 - stability)))
	Phi.lwr <- queries * Phi.lwr / (1 + (queries - 1) * Phi.lwr)
	
	Phi.upr <- gdata$em.s^2 - qf(alpha, df.s, Inf) * gdata$em.s * gdata$em.e
	Phi.upr <- Phi.upr + (qf(alpha, df.s, Inf) - qf(alpha, df.s, df.e)) *
		qf(alpha, df.s, df.e) * gdata$em.e^2
	Phi.upr <- Phi.upr / ( (gdata$n.s - 1) * qf(alpha, df.s, Inf,) * gdata$em.s * gdata$em.e + 
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

gt2data <- function(Erho2 = 0.95, Phi = 0.95) {
	as <- c(tau = 2.84729794002905, tau.ap = 3.98652984123827, power = 4.77902509574171, minor = 1.53337366741287, major = 2.62976839002005, asens = 1.54402996734738, rsens = 1.29759126030214, rmse = 3.27642726002903)

	res <- list(
		tau = Erho2^as["tau"], tau.ap = Erho2^as["tau.ap"],
		power = Erho2^as["power"], minor = (1-Erho2)^as["minor"], major = (1-Erho2)^as["major"],
		asens = (1-Erho2)^as["asens"], rsens = (1-Phi)^as["rsens"],
		rmse = (1-Phi)^as["rmse"],
		call = list(Erho2 = Erho2, Phi = Phi)
	)
	class(res) <- "gt2data"
	return(res)
}

print.gstudy <- function(x) {
	cat("\nSummary of G-Study\n")
	cat("\n                 Systems     Queries Interaction\n")
	cat("             ----------- ----------- -----------\n")
	cat(sep="", "Variance    ", sprintf("%*.5g",12,x$var.s), sprintf("%*.5g",12,x$var.q), sprintf("%*.5g",12,x$var.e),"\n")
	varsum <- (x$var.s + x$var.q + x$var.e) / 100
	cat(sep="", "Variance(%) ", sprintf("%*.5g",12,x$var.s/varsum), sprintf("%*.5g",12,x$var.q/varsum), sprintf("%*.5g",12,x$var.e/varsum),"\n")
	cat("---\n")
	cat(sep="", "Mean Sq.    ", sprintf("%*.5g",12,x$em.s), sprintf("%*.5g",12,x$em.q), sprintf("%*.5g",12,x$em.e),"\n")
	cat(sep="", "Sample size ", sprintf("%*.5g",12,x$n.s), sprintf("%*.5g",12,x$n.q), sprintf("%*.5g",12,x$n.s*x$n.q), "\n\n")
}

print.dstudy <- function(x) {
	cat("\nSummary of D-Study\n")
	cat("\nCall:\n")
	cat("    queries =", x$call$n.q, "\n")
	cat("  stability =", x$call$stability, "\n")
	cat("      alpha =", x$call$alpha, "\n")

	# Stability
	cat("\nStability:\n")	
	if(length(x$call$alpha) > 1){
		cat("                                           Erho2                                   Phi\n")
		cat("             -----------------------------------   -----------------------------------\n")  
		cat("       Alpha    Expected       Lower       Upper      Expected       Lower       Upper\n")
		cat(" ----------- ----------- ----------- -----------   ----------- ----------- -----------\n")
		for(i in 1:length(x$call$alpha))
			cat("", sprintf("%*.5g",11,x$call$alpha[i]), sprintf("%*.5g",11,x$Erho2[1]), sprintf("%*.5g",11,x$Erho2.lwr[i]), sprintf("%*.5g",11,x$Erho2.upr[i]), " ", sprintf("%*.5g",11,x$Phi[1]), sprintf("%*.5g",11,x$Phi.lwr[i]), sprintf("%*.5g",11,x$Phi.upr[i]), "\n")
	}else{
		cat("                                           Erho2                                   Phi\n")
		cat("             -----------------------------------   -----------------------------------\n")  
		cat("     Queries    Expected       Lower       Upper      Expected       Lower       Upper\n")
		cat(" ----------- ----------- ----------- -----------   ----------- ----------- -----------\n")
		for(i in 1:length(x$call$n.q))
			cat("", sprintf("%*.5g",11,x$call$n.q[i]), sprintf("%*.5g",11,x$Erho2[i]), sprintf("%*.5g",11,x$Erho2.lwr[i]), sprintf("%*.5g",11,x$Erho2.upr[i]), " ", sprintf("%*.5g",11,x$Phi[i]), sprintf("%*.5g",11,x$Phi.lwr[i]), sprintf("%*.5g",11,x$Phi.upr[i]), "\n")
	}

	# Number of queries
	cat("\nRequired number of queries:\n")
	if(length(x$call$alpha) > 1){
		cat("                                           Erho2                                   Phi\n")
		cat("             -----------------------------------   -----------------------------------\n")  
		cat("       Alpha    Expected       Lower       Upper      Expected       Lower       Upper\n")
		cat(" ----------- ----------- ----------- -----------   ----------- ----------- -----------\n")
		for(i in 1:length(x$call$alpha))
			cat("", sprintf("%*.5g",11,x$call$alpha[i]), sprintf("%*.5g",11,x$n.q_Erho2[1]), sprintf("%*.5g",11,x$n.q_Erho2.upr[i]), sprintf("%*.5g",11,x$n.q_Erho2.lwr[i]), " ", sprintf("%*.5g",11,x$n.q_Phi[1]), sprintf("%*.5g",11,x$n.q_Phi.upr[i]), sprintf("%*.5g",11,x$n.q_Phi.lwr[i]), "\n")
	}else{
		cat("                                           Erho2                                   Phi\n")
		cat("             -----------------------------------   -----------------------------------\n")  
		cat("   Stability    Expected       Lower       Upper      Expected       Lower       Upper\n")
		cat(" ----------- ----------- ----------- -----------   ----------- ----------- -----------\n")
		for(i in 1:length(x$call$stability))
			cat("", sprintf("%*.5g",11,x$call$stability[i]), sprintf("%*.5g",11,x$n.q_Erho2[i]), sprintf("%*.5g",11,x$n.q_Erho2.upr[i]), sprintf("%*.5g",11,x$n.q_Erho2.lwr[i]), " ", sprintf("%*.5g",11,x$n.q_Phi[i]), sprintf("%*.5g",11,x$n.q_Phi.upr[i]), sprintf("%*.5g",11,x$n.q_Phi.lwr[i]), "\n")
	}
}

print.gt2data <- function(x) {
	cat("\nEstimated indicators\n")
	cat("\n       Erho2         Tau       TauAP       Power Min. Confl. Maj. Confl.  Abs. Sens.\n")
	cat(" ----------- ----------- ----------- ----------- ----------- ----------- -----------\n")
	for(i in 1:length(x$call$Erho2))
		cat(sep=" ", sprintf("%*.5g",12,x$call$Erho2[i]), sprintf("%*.5g",11,x$tau[i]), sprintf("%*.5g",11,x$tau.ap[i]), sprintf("%*.5g",11,x$power[i]), sprintf("%*.5g",11,x$minor[i]), sprintf("%*.5g",11,x$major[i]), sprintf("%*.5g",11,x$asens[i]), "\n")
	cat("\n         Phi  Rel. Sens.       RMSE\n")
	cat(" ----------- ----------- -----------\n")
	for(i in 1:length(x$call$Phi))
		cat(sep=" ", sprintf("%*.5g",12,x$call$Phi[i]), sprintf("%*.5g",11,x$rsens[i]), sprintf("%*.5g",11,x$rmse[i]), "\n")
}
