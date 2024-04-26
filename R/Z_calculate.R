#' Z calculator
#'
#'
#' @export
Z_calculate <- function(score, mean, sd) {
  ((score - mean) / sd)
}
