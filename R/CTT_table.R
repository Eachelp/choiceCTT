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
#
#' @export
CTT_table <- function(data, Poly = FALSE, missing = "omit") {

  data <- as.data.frame(data)

  categories <- sort(unique(unlist(data)))

  if (ncol(data) < 1 || nrow(data) < 1) {
    stop("Input data must have at least one row and one column.")
  }

  # 결측치 처리
  if (missing == "omit") {
    data <- na.omit(data)
  } else if (missing == "zero") {
    data <- data.frame(lapply(data, function(x) {
      x[is.na(x)] <- 0
      return(x)
      }))
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

  df <- data.frame(mean = numeric(ncol(data)),
                   sd = numeric(ncol(data)),
                   Diff = numeric(ncol(data)),
                   Disc = numeric(ncol(data)),
                   Del_Alpha = numeric(ncol(data)))

  for (category in categories) {
    df[[as.character(category)]] <- numeric(ncol(data))
  }

  coeff_alpha <- function(responses) {
    var_x <- var(rowSums(responses))
    var_y <- apply(responses, 2, var)
    n_items <- ncol(responses)
    alpha <- (n_items / (n_items - 1)) * (1 - sum(var_y) / var_x)
    return(alpha)
  }

  df$mean <- cround(apply(data, 2, mean), 3)
  df$sd <- cround(apply(data, 2, sd), 3)
  df$Diff <- cround(apply(data, 2, mean) / max(data), 3)

  for (i in 1:ncol(data)) {
    correlation_method <- ifelse(Poly, "spearman", "pearson")
    df$Disc[i] <- cround(cor(data[, i], rowSums(data[, -i]),
                             method = correlation_method), 3)
    df$Del_Alpha[i] <- cround(coeff_alpha(data[, -i]), 3)
  }

  for (i in 1:ncol(data)) {
    if (length(table(data[, i])) == 0) {
      df[i, 6:(5+length(categories))] <- rep(0, length(categories))
    } else {
      df[i, 6:(5+length(categories))] <- MatrixRow(data[, i], nrow(data))
    }
  }

  rownames(df) <- colnames(data)
  total_alpha <- coeff_alpha(data)
  sd_x <- sd(rowSums(data))
  se <- sd_x * sqrt(1 - total_alpha)

  return(list(ctt_table = df, alpha = total_alpha, se = se))
}
