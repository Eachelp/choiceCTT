#'
#'
#'
#' @export
assign_label <- function(score, thresholds_df) {
  labels <- colnames(thresholds_df)
  for (label in labels) {
    range <- thresholds_df[[label]]
    if (score >= range[1] && score <= range[2]) {
      return(label)
    }
  }
  return(NA)
}
