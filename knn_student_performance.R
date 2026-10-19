# Install required packages 
install.packages("tidyverse")
install.packages("caret")
install.packages("class")
install.packages("pROC")

# Load packages
library(tidyverse)
library(caret)
library(class)
library(pROC)

# DATA LOADING

# Read the csv file 
students_performance <- read_csv("StudentsPerformance.csv")

# Display structure
str(students_performance)

# View first few rows
head(students_performance)

# Statistical summary
summary(students_performance)

# Column names
print(names(students_performance))

# DATA CLEANING AND PRE-PROCESSING

# Create cleaned version
students_cleaned <- students_performance

# Check for duplicates
duplicate_count <- sum(duplicated(students_cleaned))

# Count missing values
colSums(is.na(students_cleaned))

# Check unique values in categorical columns
col_categorical_vars <- c("gender", "race/ethnicity", "parental level of education", 
                          "lunch", "test preparation course")
for (var in col_categorical_vars) {
  cat(var, ":", toString(unique(students_cleaned[[var]])), "\n")
}

# Handling Incorrect Data: standardize education levels
students_cleaned$`parental level of education` <- ifelse(
  students_cleaned$`parental level of education` == "some college", "college",
  ifelse(
    students_cleaned$`parental level of education` == "some high school", "high school",
    students_cleaned$`parental level of education`
  )
)

# Create average score column
students_cleaned <- students_cleaned %>%
  mutate(
    average_score = round((`math score` + `reading score` + `writing score`) / 3)
  )

# Create BINARY outcome: Pass (>= 60) or Fail (< 60)
students_cleaned <- students_cleaned %>%
  mutate(
    pass_fail = factor(ifelse(average_score >= 60, "Pass", "Fail"),
                      levels = c("Fail", "Pass"))
  )

# EXPLORATORY DATA ANALYSIS (EDA)

# 1. PASS/FAIL DISTRIBUTION
print(table(students_cleaned$pass_fail))
pass_rate <- mean(students_cleaned$pass_fail == "Pass") * 100
cat("Pass Rate:", round(pass_rate, 1), "%\n")
cat("Fail Rate:", round(100 - pass_rate, 1), "%\n\n")

p1 <- ggplot(students_cleaned, aes(x = pass_fail, fill = pass_fail)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Fail" = "#FF6B6B", "Pass" = "#4ECDC4")) +
  labs(title = "Distribution of Students by Pass/Fail Status",
       subtitle = paste("Pass Rate:", round(pass_rate, 1), "%"),
       x = "Status", 
       y = "Number of Students")

print(p1)

# 2. GENDER ANALYSIS
p2 <- ggplot(students_cleaned, aes(x = gender, fill = gender)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("female" = "#FF6B9D", "male" = "#4169E1")) +
  labs(title = "Distribution of Students by Gender",
       x = "Gender", 
       y = "Count") +
  theme(legend.position = "none")

print(p2)

# 3. GENDER VS PASS/FAIL
p3 <- ggplot(students_cleaned, aes(x = gender, fill = pass_fail)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Fail" = "#FF6B6B", "Pass" = "#4ECDC4")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Pass Rate by Gender",
       x = "Gender",
       y = "Percentage",
       fill = "Status")

print(p3)

# 4. RACE/ETHNICITY DISTRIBUTION
p4 <- ggplot(students_cleaned, aes(x = `race/ethnicity`, fill = `race/ethnicity`)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3")) +
  labs(title = "Distribution by Race/Ethnicity",
       x = "Race/Ethnicity",
       y = "Count")

print(p4)

# 5. RACE/ETHNICITY VS PASS/FAIL
p5 <- ggplot(students_cleaned, aes(x = `race/ethnicity`, fill = pass_fail)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Fail" = "#FF6B6B", "Pass" = "#4ECDC4")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Pass Rate by Race/Ethnicity",
       x = "Race/Ethnicity",
       y = "Percentage",
       fill = "Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)

# 6. PARENTAL EDUCATION LEVEL
p6 <- ggplot(students_cleaned, aes(x = `parental level of education`, 
                                    fill = `parental level of education`)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#deebf7", "#9ecae1", "#6baed6", "#4292c6", "#08519c")) +
  labs(title = "Distribution by Parental Education Level",
       x = "Parental Education Level",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p6)

# 7. PARENTAL EDUCATION VS PASS/FAIL
p7 <- ggplot(students_cleaned, aes(x = `parental level of education`, fill = pass_fail)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Fail" = "#FF6B6B", "Pass" = "#4ECDC4")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Pass Rate by Parental Education Level",
       x = "Parental Education Level",
       y = "Percentage",
       fill = "Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p7)

# 8. LUNCH TYPE ANALYSIS
p8 <- ggplot(students_cleaned, aes(x = lunch, fill = lunch)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("standard" = "#90EE90", "free/reduced" = "#FFB6C1")) +
  labs(title = "Distribution by Lunch Type",
       x = "Lunch Type",
       y = "Count")

print(p8)

# 9. LUNCH TYPE VS PASS/FAIL
p9 <- ggplot(students_cleaned, aes(x = lunch, fill = pass_fail)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Fail" = "#FF6B6B", "Pass" = "#4ECDC4")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Pass Rate by Lunch Type",
       subtitle = "Shows impact of socioeconomic status",
       x = "Lunch Type",
       y = "Percentage",
       fill = "Status")

print(p9)

# 10. TEST PREPARATION COURSE
p10 <- ggplot(students_cleaned, aes(x = `test preparation course`, 
                                    fill = `test preparation course`)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("completed" = "#87CEEB", "none" = "#FFA07A")) +
  labs(title = "Test Preparation Course Completion",
       x = "Test Preparation Course",
       y = "Count")

print(p10)

# 11. TEST PREP VS PASS/FAIL
p11 <- ggplot(students_cleaned, aes(x = `test preparation course`, fill = pass_fail)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Fail" = "#FF6B6B", "Pass" = "#4ECDC4")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Pass Rate by Test Preparation Status",
       x = "Test Preparation Course",
       y = "Percentage",
       fill = "Status")

print(p11)
 
# 12. COMBINED FACTORS HEATMAP
combined_summary <- students_cleaned %>%
  group_by(gender, lunch) %>%
  summarise(
    pass_rate = mean(pass_fail == "Pass") * 100,
    .groups = "drop"
  )

p12 <- ggplot(combined_summary, aes(x = gender, y = lunch, fill = pass_rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pass_rate, 1), "%")), color = "black", size = 5) +
  scale_fill_gradient(low = "#FFF5BA", high = "#4ECDC4") +
  labs(title = "Pass Rate Heatmap: Gender vs Lunch Type",
       x = "Gender",
       y = "Lunch Type",
       fill = "Pass Rate (%)")

print(p12)

# DATA PREPARATION FOR MACHINE LEARNING

cat("\n PREPARING DATA FOR KNN CLASSIFICATION \n")

# Prepare features
model_data <- students_cleaned %>%
  mutate(
    gender_num = as.numeric(factor(gender)),
    race_num = as.numeric(factor(`race/ethnicity`)),
    parent_edu_num = as.numeric(factor(`parental level of education`, 
                                       levels = c("high school", "college",
                                                  "associate's degree", 
                                                  "bachelor's degree", 
                                                  "master's degree"),
                                       ordered = TRUE)),
    lunch_num = as.numeric(factor(lunch)),
    test_prep_num = as.numeric(factor(`test preparation course`))
  ) %>%
  select(gender_num, race_num, parent_edu_num, lunch_num, test_prep_num, pass_fail)

# Normalize function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

model_data_n <- as.data.frame(lapply(model_data[, 1:5], normalize))

# TRAIN-TEST SPLIT

set.seed(123)
train_idx <- sample(1:nrow(model_data_n), size = 0.7 * nrow(model_data_n),replace = FALSE)

train_data <- model_data_n[train_idx, ]
test_data <- model_data_n[-train_idx, ]
train_labels <- model_data$pass_fail[train_idx]
test_labels <- model_data$pass_fail[-train_idx]

# KNN CLASSIFICATION

NROW(train_labels)

#The square root of 700 is around 26.45, therefore we’ll create two models. One with ‘K’ value as 26 and the other model with a‘K’ value as 27.

# Run KNN with k=26 and k=27
knn.26 <- knn(train = train_data, test = test_data, cl = train_labels, k = 26)
knn.27 <- knn(train = train_data, test = test_data, cl = train_labels, k = 27)

# Calculate accuracy
ACC.26 <- 100 * sum(test_labels == knn.26) / NROW(test_labels)
ACC.27 <- 100 * sum(test_labels == knn.27) / NROW(test_labels)

ACC.26

ACC.27

# Detailed evaluation with confusion matrix
cat("=== CONFUSION MATRIX FOR K=26 ===\n")
cm_26 <- confusionMatrix(knn.26, test_labels)
print(cm_26)

cat("\n=== CONFUSION MATRIX FOR K=27 ===\n")
cm_27 <- confusionMatrix(knn.27, test_labels)
print(cm_27)

# Optimization
i = 1
k.optm = 1
for (i in 1:28){knn.mod <-knn(train=train_data, test=test_data, cl=train_labels,k=i)
k.optm[i] <- 100 * sum(test_labels == knn.mod)/NROW(test_labels)
k=i
cat(k,'=',k.optm[i],'
    ')}


plot(k.optm, type="b", xlab="K-value",ylab="Accuracy Level")

# Build final model with k=8
knn.8 <- knn(train = train_data, test = test_data, cl = train_labels, k = 8)

# Calculate accuracy
ACC.8 <- 100 * sum(test_labels == knn.8) / NROW(test_labels)

# Confusion matrix for k=8
cat("=== CONFUSION MATRIX FOR K=8 ===\n")
cm_8 <- confusionMatrix(knn.8, test_labels)
print(cm_8)

#PLOT ROC CURVE AND CALCULATE AUC

# Get probabilities
knn.8.prob <- knn(train = train_data, test = test_data, cl = train_labels, 
                  k = 8, prob = TRUE)
prob_values <- attr(knn.8.prob, "prob")
prob_pass <- ifelse(knn.8.prob == "Pass", prob_values, 1 - prob_values)

# Create ROC object
roc_curve <- roc(response = test_labels, predictor = prob_pass, 
                 levels = c("Fail", "Pass"))

# Calculate AUC
auc_value <- auc(roc_curve)

# Plot
plot(roc_curve, main = "ROC Curve for KNN Model (k=8)", 
     col = "blue", lwd = 2, print.auc = TRUE)
abline(a = 0, b = 1, lty = 2, col = "red")


