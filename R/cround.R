#' cround
#'new round function
#'
#' @export
cround <- function(x, n) {
  vorz <- sign(x)
  z <- abs(x) * 10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^n
  z * vorz
}
