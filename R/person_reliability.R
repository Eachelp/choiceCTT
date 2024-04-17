#' person reliability
#'
#'
#' @export
#'
person_reliability <- function(data, variance_threshold = 0.1, relMax = NULL) {
  # Set default value for relMax if not provided
  if (is.null(relMax)) {
    relMax <- ncol(data) / 3  # Default to one third of the number of columns
  }

  # Calculate variance, maximum consecutive run length, and count NA and empty strings
  df <- data.frame(
    Variance = apply(data, 1, function(row) var(row, na.rm = TRUE)),  # Calculate variance excluding NA values
    Max_Run = apply(data, 1, function(row) {
      non_na_row = row[!is.na(row) & row != ""]  # Remove NA and empty strings before calculating run length
      if (length(non_na_row) == 0) return(0)  # Return 0 if the row is empty after removing NA and ""
      max(rle(non_na_row)$lengths)  # Calculate maximum consecutive run length
    }),
    count_NA = apply(data, 1, function(row) sum(is.na(row) | row == "")),  # Count the number of NA and empty strings
    row.names = row.names(data)  # Preserve original row names
  )

  # Mark rows based on variance and run length thresholds
  df$Variance_Mark = ifelse(df$Variance <= variance_threshold, "O", "")  # Mark "O" if variance is below threshold
  df$Run_Mark = ifelse(df$Max_Run >= relMax, "O", "")  # Mark "O" if maximum run length is above the threshold

  return(df)
}


