#' filter colum
#'
#'
#' @export
filter_columns <- function(data, items, variable) {
  valid_columns <- items[[variable]][items[[variable]] != "" & !is.na(items[[variable]])]
  data[, valid_columns, drop = FALSE]
}
