calculate_sums <- function(data, items) {

  result_data <- data.frame(matrix(nrow = nrow(data), ncol = ncol(items)))
  names(result_data) <- names(items)

  for (i in 1:ncol(items)) {

    var_name <- names(items)[i]
    item_names <- unlist(items[i], use.names = FALSE)

    valid_items <- item_names[item_names != "" & !is.na(item_names)]
    valid_items <- valid_items[valid_items %in% names(data)]

    # 유효한 항목이 있는 경우에만 계산
    if (length(valid_items) > 0) {
      result_data[ ,var_name] <- rowSums(data[ ,valid_items], na.rm = TRUE)
    } else {
      result_data[ ,var_name] <- rep(NA, nrow(data))
    }
  }

  return(result_data)
}
