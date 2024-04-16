#'
#'
#'
#' @export
table_T <- function(data, stat, group, items, digit = 0, level = TRUE, smooth = FALSE, level_type) {

  sum_data <- calculate_sums(data = data, items = items)

  data <- cbind(data[, group, drop = FALSE], sum_data)

  colnames(data)[1] <- group

  variables <- names(items)

  data$original_order <- 1:nrow(data)

  data <- merge(x = data, y = stat, by = group, all.x = TRUE)

  data <- data[order(data$original_order), ]

  data$original_order <- NULL

  # make empty data frame

  if(level){
    result_data <- data.frame(matrix(nrow = nrow(data),
                                     ncol = 2 * length(variables)))
    names(result_data) <- c(sapply(variables, function(v) c(paste0(v, "_T"),
                                                            paste0(v, "_level"))))
  } else {
    result_data <- data.frame(matrix(nrow = nrow(data),
                                     ncol = length(variables)))
    names(result_data) <- c(sapply(variables, function(v) c(paste0(v, "_T"))))
  }


  for (i in 1:length(variables)) {

    var <- variables[i]
    mean_var <- paste0(var, "_M")
    sd_var <- paste0(var, "_SD")
    t_var <- paste0(var, "_T")

    result_data[[t_var]] <- cround(T_calculate(score = data[[var]],
                                               mean = data[[mean_var]],
                                               sd = data[[sd_var]]), digit)
    if(smooth){
      ifelse(result_data[[t_var]] > 80, result_data[[t_var]] <- 80,
             ifelse(result_data[[t_var]] < 20, result_data[[t_var]] <- 20,
                result_data[[t_var]] <- result_data[[t_var]]))
    }

    if(level){
      label_var <- paste0(var, "_level")
      result_data[[label_var]] <- sapply(result_data[[t_var]],
                                         function(x) assign_label(x, level_type))
    }
  }

  return(result_data)
}
