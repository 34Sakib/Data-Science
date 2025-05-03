data <- read.csv("C:/Users/HP/Downloads/2007-08.csv", header = TRUE)

print(str(data))

numeric_data <- data[, c("FTHG", "FTAG", "HF", "AF", "HY", "AY")]

numeric_data <- data.frame(lapply(numeric_data, as.numeric))

numeric_data <- na.omit(numeric_data)

pearson_corr <- cor(numeric_data, method = "pearson")
cat("Pearson Correlation Matrix:\n")
print(pearson_corr)

spearman_corr <- cor(numeric_data, method = "spearman")
cat("Spearman Correlation Matrix:\n")
print(spearman_corr)

kendall_corr <- cor(numeric_data, method = "kendall")
cat("Kendall Correlation Matrix:\n")
print(kendall_corr)

