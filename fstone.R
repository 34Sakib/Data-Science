data <- read.csv("C:/Users/HP/Downloads/2007-08.csv", header = TRUE)

print(str(data))

numeric_data <- data[, c("FTHG", "FTAG", "HF", "AF", "HY", "AY")]

numeric_data <- data.frame(lapply(numeric_data, as.numeric))

numeric_data <- na.omit(numeric_data)

calculate_pairwise_correlation <- function(data, method) {
  cat("\n", toupper(method), "Correlations:\n", sep = "")
  for (i in 1:(ncol(data) - 1)) {
    for (j in (i + 1):ncol(data)) {
      corr_value <- cor(data[[i]], data[[j]], method = method)
      cat(colnames(data)[i], "vs", colnames(data)[j], ":", corr_value, "\n")
    }
  }
}

calculate_pairwise_correlation(numeric_data, "pearson")
calculate_pairwise_correlation(numeric_data, "spearman")
calculate_pairwise_correlation(numeric_data, "kendall")
