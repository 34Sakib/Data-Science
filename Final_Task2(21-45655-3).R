data <- read.csv("C:/Users/HP/Downloads/2007-08.csv", header = TRUE)

print(str(data))

numeric_data <- data[, c("FTHG", "FTAG", "HF", "AF", "HY", "AY")]

numeric_data <- data.frame(lapply(numeric_data, as.numeric))
numeric_data <- na.omit(numeric_data)

install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

ggplot(numeric_data, aes(x = FTHG)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Full-Time Home Goals (FTHG)", x = "Goals", y = "Frequency") +
  theme_minimal()

ggplot(numeric_data, aes(x = FTHG)) +
  geom_freqpoly(binwidth = 1, color = "blue", size = 1.2) +
  labs(title = "Line Histogram of Full-Time Home Goals (FTHG)", x = "Goals", y = "Frequency") +
  theme_minimal()

ggplot(numeric_data, aes(x = 1:nrow(numeric_data), y = FTHG)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Line Plot of Full-Time Home Goals (FTHG)", x = "Match Index", y = "Home Goals") +
  theme_minimal()

ggplot(numeric_data, aes(x = "FTHG", y = FTHG)) +
  geom_violin(fill = "lightblue", color = "black") +
  labs(title = "Violin Plot of Full-Time Home Goals (FTHG)", x = "Category", y = "Goals") +
  theme_minimal()

ggplot(numeric_data, aes(x = FTHG, y = FTAG)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatterplot of Full-Time Home Goals vs Away Goals", x = "Home Goals (FTHG)", y = "Away Goals (FTAG)") +
  theme_minimal()

