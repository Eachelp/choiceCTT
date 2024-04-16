T_calculate <- function(score, mean, sd) {
  ((score - mean) / sd) * 10 + 50
}
