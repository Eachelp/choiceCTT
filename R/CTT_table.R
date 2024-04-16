#' This is a simple function that makes a table based on Classical Test Theory (CTT).
#' In this table you can show descriptive statistics(average and standard deviation) of each items
#' and can show Difficulty and Discrimination(item total correlation)
#'
#' The Difficulty of CTT is same with average when item is dichotomous item.
#' But when item is polytomous item, the Difficulty is same with average/L {L is categorical(dichotomous is 1; 0/1)}
#'
#' Discrimination is item total correlation but in this function, each item doesn't containe total score
#' (when you calculate Discrimination of item 1 in 10 items test, the total score is sum score, from 2 to 9 items)
#'
#' Also This function can calculate Reliability(Cronbach'α)
#' However, it should be noted that Cronbach'α is not, strictly speaking, an index of reliability.
#' And in this table can show a "Deleted alpha", The Cronbach'α that increases when one item is removed.
#'
#' in the R round is
#' @export
CTT_table <- function(data, categories = 0:7) {

  # check the data
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Input data must be a data frame or a matrix.")
  }
  if (ncol(data) < 1) {
    stop("Input data must have at least one column.")
  }

  # help function

  df <- data.frame(mean = rep(0, ncol(data)),
                   sd = rep(0, ncol(data)),
                   Difficulty = rep(0, ncol(data)),
                   Discrimination = rep(0, ncol(data)),
                   Deleted_alpha = rep(0, ncol(data)))

  # add column to df. using length of categories
  for (category in categories) {
    df[[as.character(category)]] <- rep(0, nrow(df))
  }

  MatrixRow <- function(rowData, n) {
    counts <- table(rowData)
    result <- sapply(categories, function(category) {
      if (as.character(category) %in% names(counts)) {
        cround(counts[as.character(category)] / n, 3)
      } else {
        0
      }
    })
    return(result)
  }

  coeff_alpha <- function(responses) {
    responses <- na.omit(responses)
    n_items <- ncol(responses)
    n_persons <- nrow(responses)
    x <- rowSums(responses)
    var_x <- var(x)
    var_y <- apply(responses, 2, var)
    alpha <- (n_items / (n_items - 1)) * (1 - sum(var_y) / var_x)
    alpha
  }

  # analysis
  df$mean = cround(apply(data, 2, mean, na.rm = TRUE), 3)

  df$sd = cround(apply(data, 2, sd, na.rm = TRUE), 3)

  df$Difficulty = cround(apply(data, 2, mean, na.rm = TRUE)/max(data, na.rm = T),3)

  for (i in 1:ncol(data)) {
    df$Discrimination[i] <- cround(cor(data[, i],
                                       rowSums(data[, -i]),
                                       use="complete.obs"),3)

    df$Deleted_alpha[i] <- cround(coeff_alpha(data[, -i]), 3)
  }

  for (i in 1:ncol(data)) {
    if (length(table(data[, i])) == 0) {
      df[i, 6:(5+length(categories))] <- rep(0, length(categories))
    } else {
      df[i, 6:(5+length(categories))] <- MatrixRow(data[, i], nrow(data))
    }
  }

  alpha <- coeff_alpha(data)
  sd_x <- sd(rowSums(data,na.rm = T))
  SE <- sd_x * sqrt(1 - alpha)


  list(ctt_table = df, alpha = alpha, se = SE)
}
