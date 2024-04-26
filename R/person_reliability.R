#' Person Reliability Analysis
#'
#' This function analyzes individual responses for signs of inconsistency or carelessness by measuring the variance, maximum run length of consecutive identical responses, and counting missing or careless responses.
#'
#' @param data A data frame containing numeric response data.
#' @param variance_threshold A numeric threshold below which a variance is considered too low, suggesting uniformity in responses that may indicate carelessness.
#' @param relMax The maximum allowable consecutive identical responses, suggesting potential automatic or careless responding.
#' @param careless_items A vector of column names to check for specific careless response patterns.
#' @param careless_response A vector of values considered to be careless if repeated too frequently.
#' @param careless_MAX The maximum number of careless responses allowed before flagging.
#' @return A data frame with original data analysis and additional flags for potential issues.
#' @export
#' @examples
person_reliability <- function(data, variance_threshold = 0.1, relMax = NULL,
                               careless_items = NULL, careless_response = c(3,4,5),
                               careless_MAX = 2) {
  # Set default value for relMax if not provided
  if (is.null(relMax)) {
    relMax <- ncol(data) / 3  # Default to one-third of the number of columns
  }

  # Check data
  if (!is.data.frame(data) || ncol(data) == 0) {
    stop("Input data must be a non-empty data frame.")
  }

  # Convert all data to numeric, coercing errors to NA
  data_numeric <- data.frame(lapply(data, function(x) {
    numeric_value <- as.numeric(as.character(x))
    if(any(is.na(numeric_value))) {
      warning("Non-numeric data found and set to NA")
    }
    return(numeric_value)
  }))

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
    Count_NA = apply(data_numeric, 1, function(row) sum(is.na(row) | row == "" | row == "NA")),
    row.names = row.names(data)
  )

  if(!is.null(careless_items) && all(careless_items %in% names(data))){
   df$Count_Careless =
      apply(data_numeric[, careless_items, drop = FALSE], 1,
            function(x) sum(x %in% careless_response, na.rm = TRUE))

   df$Mark_Careless = ifelse(df$Count_Careless >= careless_MAX, "O", "")
  }

  # Mark rows based on variance and run length thresholds
  df$Mark_Var = ifelse(df$Variance <= variance_threshold, "O", "")
  df$Mark_Run = ifelse(df$Max_Run >= relMax, "O", "")

  desired_order = c("Variance", "Max_Run", "Count_NA", "Count_Careless", "Mark_Careless", "Mark_Var", "Mark_Run")
  df <- df[, desired_order]

  return(df)
}




