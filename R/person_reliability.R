#' person reliability
#'
#'
#'
#' @export
person_reliability <- function(data, variance_threshold = 0.1, relMax = NULL,
                               careless_items = NULL, careless_response = c(3,4,5),
                               careless_MAX = 2) {
  # Set default value for relMax if not provided
  if (is.null(relMax)) {
    relMax <- ncol(data) / 3  # Default to one-third of the number of columns
  }

  # Convert all data to numeric, coercing errors to NA
  data_numeric <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))

  # Calculate variance, maximum run length, and count missing values including empty strings and "NA" texts
  df <- data.frame(
    Variance = apply(data_numeric, 1, function(row) {
      if (all(is.na(row))) return(NA)  # Return NA if all values are NA
      var(row, na.rm = TRUE)  # Calculate variance excluding NAs
    }),
    Max_Run = apply(data_numeric, 1, function(row) {
      non_na_row = row[!is.na(row)]
      if (length(non_na_row) == 0) return(0)  # Return 0 if the row is empty after removing NAs
      max(rle(non_na_row)$lengths)  # Calculate maximum consecutive run length
    }),
    count_NA = apply(data, 1, function(row) sum(is.na(row) | row == "" | row == "NA")),
    Response_Count = if (!is.null(careless_items) && all(careless_items %in% names(data))) {
      apply(data[, careless_items, drop = FALSE], 1, function(x) sum(x %in% careless_response, na.rm = TRUE))
    } else rep(NA, nrow(data)),  # Return NA for all rows if careless_items are null or missing
    row.names = row.names(data)
  )

  # Mark rows based on variance and run length thresholds
  df$Variance_Mark = ifelse(df$Variance <= variance_threshold, "O", "")
  df$Run_Mark = ifelse(df$Max_Run >= relMax, "O", "")
  df$careless_Mark = ifelse(df$Response_Count >= careless_MAX, "O", "")

  return(df)
}



