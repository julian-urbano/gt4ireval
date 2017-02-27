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
