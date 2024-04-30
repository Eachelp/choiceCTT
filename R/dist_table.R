#' Calculate distribution of categories per item and quantile grouping
#'
#' This function calculates the frequency distribution of response categories
#' for each item in the data and also groups the total scores into quantiles.
#' It returns a list containing the distribution tables for each item and a summary table.
#'
#' @param data A data frame with each column representing an item with responses.
#' @param categories A vector of the categories to consider for frequency calculation.
#' @param cutting The number of quantile groups to create.
#' @return A list containing distribution tables for each item and a summary.
#' @export
dist_table <- function(data, categories = 1:4, cutting = 4, digit = 3) {
  cut_start <- 1 / cutting
  probs <- seq(cut_start, 1, by = cut_start)
  df <- data.frame(data)
  df_2 <- df  # Clone the data frame to preserve the original data while adding quantile groups

  sumdata <- rowSums(df)

  quantiles <- quantile(sumdata, probs = probs)
  df_2$quantile_group <- cut(sumdata, breaks = c(-Inf, quantiles), include.lowest = TRUE, labels = paste0("Q", 1:cutting))

  distribution_list <- list(summary = NULL)
  # Initialize the summary table with zeros
  all_dist <- data.frame(matrix(0, ncol = length(categories), nrow = ncol(df)))
  colnames(all_dist) <- categories
  rownames(all_dist) <- colnames(df)

  for (i in 1:ncol(df)) {
    df[[i]] <- factor(df[[i]], levels = categories)
    member <- sum(table(df[[i]]))
    distribution <- table(df[[i]], df_2$quantile_group)

    # Normalize by total number of responses per item
    all_dist[i,] <- table(df[[i]]) / member
    dit <- distribution / member
    distribution_list[[colnames(df)[i]]] <- cround(dit, digit)
  }

  distribution_list[["summary"]] = cround(all_dist, digit)

  return(distribution_list)
}
