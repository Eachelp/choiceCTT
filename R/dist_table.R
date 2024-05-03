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
dist_table <- function(data, score = NULL, cutting = 4, digit = 3, missing = "omit") {

  cut_start <- 1 / cutting
  probs <- seq(cut_start, 1, by = cut_start)

  if(missing == "omit"){
    df <- data.frame(na.omit(data))
  } else {
    df <- data
    df[is.na(df)] <- missing
    df <- data.frame(df)
  }

  df_2 <- df

  categories <- sort(unique(unlist(df)))

  category_length <- length(categories)
  num_columns <- ncol(df)
  df_list <- list()
  correlation_list <- list()
  cor_df_list <- vector("list", num_columns)
  results_list <- vector("list", num_columns)

  df_2 <- df  # Clone the data frame to preserve the original data while adding quantile groups

  if(is.null(score)){
    sumdata <- rowSums(df)
  } else {
    sumdata <- score
  }

  for (j in 1:category_length) {
    df_name <- paste("Df_one_hot", j, sep = "_")
    df_list[[df_name]] <- data.frame(ifelse(df == categories[j], 1, 0))

    cor_list <- numeric(ncol(df))
    for (i in 1:ncol(df)) {

      if(is.null(score)){
        sum_other_columns <- rowSums(df[, -i, drop = FALSE])
      } else { sum_other_columns <- score}

      # df_list에서 i번째 열을 추출하여 상관계수를 계산
      cor_list[i] <- cor(df_list[[df_name]][,i], sum_other_columns)
    }
    correlation_list[[df_name]] <- cor_list
  }

  for (i in 1:num_columns) {
    # 각 문항별 결과를 저장할 데이터 프레임 생성
    results_df <- data.frame(Category = integer(), Mean = numeric(), SD = numeric())
    for (cat in categories) {
      selected_scores <- sumdata[df[, i] == cat]
      mean_score <- mean(selected_scores, na.rm = TRUE)
      sd_score <- sd(selected_scores, na.rm = TRUE)

      # 결과 데이터 프레임에 추가
      results_df <- rbind(results_df, data.frame(Category = cat, Mean = mean_score, SD = sd_score))
    }

    # 소수점 자릿수 반올림
    results_df$Mean <- results_df$Mean
    results_df$SD <- results_df$SD

    # 각 문항별 결과를 리스트에 저장
    results_list[[i]] <- results_df
  }


  for (i in 1:num_columns) {
    # 각 열에 대한 상관계수를 저장할 임시 데이터 프레임 생성
    temp_df <- data.frame(matrix(ncol = category_length, nrow = 1))
    colnames(temp_df) <- paste(categories)

    # 각 카테고리별 상관계수를 추출하여 임시 데이터 프레임에 저장
    for (j in 1:category_length) {
      df_name <- paste("Df_one_hot", categories[j], sep = "_")
      temp_df[1, j] <- correlation_list[[df_name]][i]
    }

    cor_df_list[[i]] <- t(temp_df)
  }

  quantiles <- quantile(sumdata, probs = probs)

  df_2$quantile_group <- cut(sumdata, breaks = c(-Inf, quantiles), include.lowest = TRUE, labels = cround(probs,3))

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
    Disc_cat <- cor_df_list[[i]]
    discric <- cbind(results_list[[i]]$Mean, results_list[[i]]$SD)
    colnames(Disc_cat) <- "Disc_cat"
    colnames(discric) <- c("mean", "SD")
    dit <- distribution / member
    distribution_list[[colnames(df)[i]]] <- cround(cbind(Disc_cat, discric, dit), digit)
  }

  distribution_list[["summary"]] = cround(all_dist, digit)

  return(distribution_list)
}

