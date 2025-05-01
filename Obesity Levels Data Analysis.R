# ----------------------------
# CSP571 OBESITY RISK PREDICTION PROJECT
# Final Code?
# ----------------------------

# Load required libraries
library(tidyverse)
library(caret)
library(randomForest)
library(shiny)
library(corrplot)
library(ggpubr)
library(DALEX)
library(nnet)

# 1. DATA ACQUISITION & CLEANING
# Load dataset from UC Irvine's repository
obesity_data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")

#Check structure
str(obesity_data)
summary(obesity_data)

# Clean column names and make all the letters lowercase 
names(obesity_data) <- gsub(" ", "_", tolower(names(obesity_data)))

# Convert ordinal features to factors
obesity_data <- obesity_data %>%
  mutate(
    obesity_level = factor(nobeyesdad, 
                           levels <- c("Insufficient_Weight", "Normal_Weight",
                                      "Overweight_Level_I", "Overweight_Level_II",
                                      "Obesity_Type_I", "Obesity_Type_II", 
                                      "Obesity_Type_III")),
    gender = factor(gender),
    family_history = factor(family_history_with_overweight),
    favc = factor(favc),
    caec = factor(caec, levels = c("no", "Sometimes", "Frequently", "Always")),
    smoke = factor(smoke),
    scc = factor(scc),
    calc = factor(calc, levels = c("no", "Sometimes", "Frequently", "Always")),
    mtrans = factor(mtrans)
  ) %>%
  select(-nobeyesdad)

# Handle missing values
sum(is.na(obesity_data)) # should be 0; this dataset is clean

# 2. EXPLORATORY DATA ANALYSIS
# Distribution of obesity levels
ggplot(obesity_data, aes(obesity_level, fill = obesity_level)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Obesity Levels")

# Age vs Obesity Level
ggplot(obesity_data, aes(age, fill = obesity_level)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~gender) +
  labs(title = "Age Distribution by Obesity Level and Gender")

# Correlation matrix
num_vars <- obesity_data %>% 
  select(where(is.numeric), -age)  # Age is already captured in density plot
cor_matrix <- cor(num_vars)
corrplot(cor_matrix, method = "color", type = "upper")

# 3. FEATURE ENGINEERING
# Create BMI feature (example calculation - verify with actual formula)
obesity_data <- obesity_data %>%
  mutate(bmi = weight / (height^2)) # Weight is in kilograms and the height is in meters

# Select final features
final_features <- c("gender", "age", "bmi", "family_history", "favc", 
                    "fcvc", "ncp", "caec", "smoke", "ch2o", "scc", "faf", 
                    "tue", "calc", "mtrans")

# 4. MODEL BUILDING
# Split data
set.seed(123)
train_index <- createDataPartition(obesity_data$obesity_level, p = 0.8, list = FALSE)
train_data <- obesity_data[train_index, ]
test_data <- obesity_data[-train_index, ]

# Logistic Regression Model (Baseline)
log_model <- multinom(obesity_level ~ ., data = train_data)
log_pred <- predict(log_model, test_data)
confusionMatrix(log_pred, test_data$obesity_level)

# Random Forest
set.seed(123)
rf_model <- randomForest(obesity_level ~ ., data = train_data, ntree = 100)
rf_pred <- predict(rf_model, test_data)
confusionMatrix(rf_pred, test_data$obesity_level)

# 5. MODEL EVALUATION
# Predictions
log_pred <- predict(log_model, test_data)
rf_pred <- predict(rf_model, test_data)

# Confusion Matrix
confusionMatrix(log_pred, test_data$obesity_level)
confusionMatrix(rf_pred, test_data$obesity_level)

# Feature Importance
rf_importance <- varImp(rf_model)
plot(rf_importance, main = "Random Forest Feature Importance")

