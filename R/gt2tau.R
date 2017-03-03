#' Map GT-based Indicators onto Data-based Indicators
#'
#' Maps Erho2 and Phi scores from Generalizability Theory onto traditional data-based scores like
#' the Kendall tau correlation, AP correlation, power, minor conflict rate and major conflict rate
#' with 2-tailed t-tests, absolute and relative sensitivity, and rooted mean squared error.
#'
#' Take these mappings with a grain of salt. See figure 3 in (Urbano, 20013).
#'
#' @param Erho2 Vector of generalizability coefficients to map from.
#' @param Phi Vector of indices of dependability to map from.
#'
#' @return A vector of data-based indicator values.
#' @seealso \code{\link{dstudy}}
#' @author Julián Urbano
#' @references J. Urbano, M. Marrero and D. Martín (2013). On the Measurement of Test Collection
#'   Reliability. ACM SIGIR, pp. 393-402.
#'
#' @examples
#' g <- gstudy(adhoc3)
#' d <- dstudy(g)
#' gt2tau(d$Erho2)
#' gt2rmse(d$Phi)
#'
#' @export
gt2tau <- function(Erho2) {
  if(any(Erho2 <= 0 | Erho2 >= 1))
    stop("Erho2 must be in (0,1]")
  return(Erho2 ^ 2.84729794002905)
}
#' @rdname gt2tau
#' @export
gt2tauAP <- function(Erho2) {
  if(any(Erho2 <= 0 | Erho2 >= 1))
    stop("Erho2 must be in (0,1]")
  return(Erho2 ^ 3.98652984123827)
}
#' @rdname gt2tau
#' @export
gt2power <- function(Erho2) {
  if(any(Erho2 <= 0 | Erho2 >= 1))
    stop("Erho2 must be in (0,1]")
  return(Erho2 ^ 4.77902509574171)
}
#' @rdname gt2tau
#' @export
gt2minor <- function(Erho2) {
  if(any(Erho2 <= 0 | Erho2 >= 1))
    stop("Erho2 must be in (0,1]")
  return((1-Erho2) ^ 1.53337366741287)
}
#' @rdname gt2tau
#' @export
gt2major <- function(Erho2) {
  if(any(Erho2 <= 0 | Erho2 >= 1))
    stop("Erho2 must be in (0,1]")
  return((1-Erho2) ^ 2.62976839002005)
}
#' @rdname gt2tau
#' @export
gt2asens <- function(Erho2) {
  if(any(Erho2 <= 0 | Erho2 >= 1))
    stop("Erho2 must be in (0,1]")
  return((1-Erho2) ^ 1.54402996734738)
}
#' @rdname gt2tau
#' @export
gt2rsens <- function(Phi) {
  if(any(Phi <= 0 | Phi >= 1))
    stop("Phi must be in (0,1]")
  return((1-Phi) ^ 1.29759126030214)
}
#' @rdname gt2tau
#' @export
gt2rmse <- function(Phi) {
  if(any(Phi <= 0 | Phi >= 1))
    stop("Phi must be in (0,1]")
  return((1-Phi) ^ 3.27642726002903)
}
