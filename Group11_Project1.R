install.packages("readxl")
library(readxl)
setwd("C:/Users/HP/Downloads")
data <- read_excel("fst.xlsx")
print(data)



missing_summary <- colSums(is.na(data))
print(missing_summary)


data_no_na <- na.omit(data)


data_mean_imputed <- data
numeric_cols <- sapply(data_mean_imputed, is.numeric)
data_mean_imputed[numeric_cols] <- lapply(data_mean_imputed[numeric_cols], function(col) {
  ifelse(is.na(col), mean(col, na.rm = TRUE), col)
})

print(data_mean_imputed$Age)


data_median_imputed <- data
data_median_imputed[numeric_cols] <- lapply(data_median_imputed[numeric_cols], function(col) {
  ifelse(is.na(col), median(col, na.rm = TRUE), col)
})


get_mode <- function(x) {
  unique_x <- unique(na.omit(x))
  unique_x[which.max(tabulate(match(x, unique_x)))]
}
categorical_cols <- !numeric_cols
data_mode_imputed <- data
data_mode_imputed[categorical_cols] <- lapply(data_mode_imputed[categorical_cols], function(col) {
  ifelse(is.na(col), get_mode(col), col)
})


data_mean_imputed[categorical_cols] <- lapply(data_mean_imputed[categorical_cols], function(col) {
  ifelse(is.na(col), get_mode(col), col)
})

print(data_mean_imputed)

print("Rows after removing NA:")
print(nrow(data_no_na))
print("Sample data with mean imputation:")
print(head(data_mean_imputed))
print("Sample data with median imputation:")
print(head(data_median_imputed))
print("Sample data with mode imputation:")
print(head(data_mode_imputed))





install.packages("ggplot2")
library(ggplot2)
install.packages("naniar")
library(naniar)


gg_miss_var(data) + 
  ggtitle("Missing Values Per Variable") + 
  theme_minimal()


vis_miss(data) +
  ggtitle("Heatmap of Missing Values") +
  theme_minimal()






gender_distribution <- table(data$Gender)
print(gender_distribution)


minority_label <- names(gender_distribution[which.min(gender_distribution)])
majority_label <- names(gender_distribution[which.max(gender_distribution)])


minority_class <- data %>% filter(Gender == minority_label)
majority_class <- data %>% filter(Gender == majority_label)


set.seed(20)
majority_sample <- majority_class %>% sample_n(nrow(minority_class))
balanced_data_undersampling <- rbind(majority_sample, minority_class)
cat("Undersampling results:\n")
print(table(balanced_data_undersampling$Gender))


set.seed(20)
oversampled_minority <- minority_class %>% sample_n(nrow(majority_class), replace = TRUE)
balanced_data_oversampling <- rbind(majority_class, oversampled_minority)
cat("Oversampling results:\n")
print(table(balanced_data_oversampling$Gender))





duplicate_rows <- data[duplicated(data), ]
print("Duplicate rows:")
print(duplicate_rows)


data_no_duplicates <- data[!duplicated(data), ]
print("Data without duplicates:")
print(head(data_no_duplicates))



filtered_data_gender <- data %>% filter(Gender == "Female")
print("Filtered data (Gender is Female):")
print(head(filtered_data_gender))





library(dplyr)

data_mean_imputed <- data_mean_imputed %>%
  mutate(
    Age_Group = case_when(
      Age < 18 ~ "Less than 18",
      Age >= 18 & Age <= 30 ~ "Between 18-30",
      Age > 30 & Age <= 40 ~ "Between 31-40",
      Age > 40 ~ "41 and up", 
      TRUE ~ NA_character_
    )
  )

print(data_mean_imputed$Age_Group)


data_mode_imputed <- data_mode_imputed %>%
  mutate(Sleep_Duration_Numeric = case_when(
    `Sleep Duration` == "Less than 5 hours" ~ 4,
    `Sleep Duration` == "5-6 hours" ~ 6,
    `Sleep Duration` == "7-8 hours" ~ 8,
    `Sleep Duration` == "More than 8 hours" ~ 10,
    TRUE ~ NA_real_  
  ))

print(data_mode_imputed$Sleep_Duration_Numeric)




min_age <- min(data_mean_imputed$Age, na.rm = TRUE)
max_age <- max(data_mean_imputed$Age, na.rm = TRUE)
data_mean_imputed$age_normalized <- ifelse(
  !is.na(data_mean_imputed$Age),
  (data_mean_imputed$Age - min_age) / (max_age - min_age),
  NA 
)
print("Data with normalized 'age' column:")
print(data_mean_imputed$age_normalized)




Q1 <- quantile(data_mean_imputed$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(data_mean_imputed$Age, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - (1.5 * IQR_value)
upper_bound <- Q3 + (1.5 * IQR_value)

print(lower_bound)
print(upper_bound)

data_no_outliers <- data_mean_imputed[data_mean_imputed$Age >= lower_bound & data_mean_imputed$Age <= upper_bound, ]

print("Data after removing outliers:")
print(data_no_outliers)



