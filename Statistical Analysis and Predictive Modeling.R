setwd("C:\\Users\\USER\\OneDrive\\Documents\\Rstudio\\Airline\\Week3_Statistical_Modeling")
dir.exists("C:\\Users\\USER\\OneDrive\\Documents\\Rstudio\\Airline")
dir.create(
  "C:\\Users\\USER\\OneDrive\\Documents\\Rstudio\\Airline\\Week3_Statistical_Modeling",
  showWarnings = FALSE
)
setwd("C:\\Users\\USER\\OneDrive\\Documents\\Rstudio\\Airline\\Week3_Statistical_Modeling")

dir.create("Dataset")
dir.create("Graphs")
dir.create("Report")
dir.create("Screenshots")
getwd()


dir.exists("C:\\Users\\USER\\OneDrive\\Documents\\Rstudio\\Airline\\airline_cleaned_dataset.csv")
getwd()
dir.exists("C:/Users/USER/OneDrive/Documents/Rstudio/Airline/Week3_Statistical_Modeling")
setwd("C:/Users/USER/OneDrive/Documents/Rstudio/Airline/Week3_Statistical_Modeling")

getwd()
# Check whether the Week 3 folder exists
dir.exists("C:/Users/USER/OneDrive/Documents/Rstudio/Airline/Week3_Statistical_Modeling")

# Set working directory
setwd("C:/Users/USER/OneDrive/Documents/Rstudio/Airline/Week3_Statistical_Modeling")

# Confirm working directory
getwd()
list.files()
list.files("Dataset")
list.files(
  "C:/Users/USER/OneDrive/Documents/Rstudio/Airline",
  recursive = TRUE,
  pattern = "\\.csv$"
)
file.copy(
  "C:/Users/USER/OneDrive/Documents/Rstudio/Airline/airline_cleaned_dataset.csv",
  "Dataset/airline_cleaned_dataset.csv",
  overwrite = TRUE
)
list.files("Dataset")
library(ggplot2)
library(dplyr)

raw_data <- read.csv("Dataset/airline_cleaned_dataset.csv")

# Check dataset dimensions
dim(raw_data)

# View first 6 rows
head(raw_data)

# Check column names
names(raw_data)
# Check number of rows and columns
dim(raw_data)

# Check passenger satisfaction counts
table(raw_data$satisfaction)

# Check data structure
str(raw_data)
dim(raw_data)

table(raw_data$satisfaction)
# Create a frequency table
class_table <- table(raw_data$class, raw_data$satisfaction)

# Display the table
class_table

# Perform Chi-square test
class_test <- chisq.test(class_table)

# Display test results
class_test
class_table <- table(raw_data$class, raw_data$satisfaction)
class_table
class_test <- chisq.test(class_table)
print(class_test)
# Step 1: Create the frequency table
class_table <- table(raw_data$class, raw_data$satisfaction)

# Step 2: Display the frequency table
print(class_table)

# Step 3: Perform the Chi-square test directly
chisq.test(class_table)
class_table <- table(raw_data$class, raw_data$satisfaction)

print(class_table)

chisq.test(class_table)
class_table <- table(raw_data$class, raw_data$satisfaction)
print(class_table)
chisq.test(class_table)


wilcox.test(
  arrival_delay_in_minutes ~ satisfaction,
  data = raw_data,
  na.action = na.omit
)

raw_data %>%
  group_by(satisfaction) %>%
  summarise(
    mean_delay = mean(arrival_delay_in_minutes, na.rm = TRUE),
    median_delay = median(arrival_delay_in_minutes, na.rm = TRUE),
    passenger_count = n()
  )
library(dplyr)
raw_data %>%
  group_by(satisfaction) %>%
  summarise(
    mean_delay = mean(arrival_delay_in_minutes, na.rm = TRUE),
    median_delay = median(arrival_delay_in_minutes, na.rm = TRUE),
    passenger_count = n()
  )
# Create binary target variable
model_data <- raw_data %>%
  mutate(
    satisfaction_binary = ifelse(
      satisfaction == "satisfied", 1, 0
    )
  )

# Check target variable
table(model_data$satisfaction_binary)

# Check dataset dimensions
dim(model_data)
library(dplyr)

model_data <- raw_data %>%
  mutate(
    satisfaction_binary = ifelse(
      satisfaction == "satisfied", 1, 0
    )
  )
head(model_data$satisfaction_binary)

table(model_data$satisfaction_binary)

dim(model_data)
model_data <- model_data %>%
  select(
    satisfaction_binary,
    age,
    gender,
    customer_type,
    type_of_travel,
    class,
    flight_distance,
    inflight_wifi_service,
    online_boarding,
    seat_comfort,
    inflight_entertainment,
    cleanliness,
    departure_delay_in_minutes,
    arrival_delay_in_minutes
  )

# Convert categorical variables into factors
model_data <- model_data %>%
  mutate(
    gender = as.factor(gender),
    customer_type = as.factor(customer_type),
    type_of_travel = as.factor(type_of_travel),
    class = as.factor(class)
  )

# Check missing values
sum(is.na(model_data))

# Check dimensions
dim(model_data)



model_data <- model_data %>%
  select(
    satisfaction_binary,
    age,
    gender,
    customer_type,
    type_of_travel,
    class,
    flight_distance,
    inflight_wifi_service,
    online_boarding,
    seat_comfort,
    inflight_entertainment,
    cleanliness,
    departure_delay_in_minutes,
    arrival_delay_in_minutes
  )
dim(model_data)
names(model_data)

model_data <- model_data %>%
  mutate(
    gender = as.factor(gender),
    customer_type = as.factor(customer_type),
    type_of_travel = as.factor(type_of_travel),
    class = as.factor(class)
  )
str(model_data)
sum(is.na(model_data))
set.seed(123)

# Select 80% of rows for training
train_index <- sample(
  seq_len(nrow(model_data)),
  size = floor(0.8 * nrow(model_data))
)

# Create training dataset
train_data <- model_data[train_index, ]

# Create testing dataset
test_data <- model_data[-train_index, ]

# Check dimensions
dim(train_data)
dim(test_data)
# Set random seed
set.seed(123)

# Select 80% of the rows
train_index <- sample(
  seq_len(nrow(model_data)),
  size = floor(0.8 * nrow(model_data))
)

# Create training data
train_data <- model_data[train_index, ]

# Create testing data
test_data <- model_data[-train_index, ]

# Display dimensions
dim(train_data)
dim(test_data)


set.seed(123)
train_index <- sample(
  seq_len(nrow(model_data)),
  size = floor(0.8 * nrow(model_data))
)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]
dim(train_data)
dim(test_data)
logistic_model <- glm(
  satisfaction_binary ~ age + gender + customer_type +
    type_of_travel + class + flight_distance +
    inflight_wifi_service + online_boarding +
    seat_comfort + inflight_entertainment +
    cleanliness + departure_delay_in_minutes +
    arrival_delay_in_minutes,
  data = train_data,
  family = binomial
)
summary(logistic_model)

# Predict probabilities
predicted_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

# Convert probabilities into classes
predicted_class <- ifelse(
  predicted_prob >= 0.5, 1, 0
)

# Create confusion matrix
confusion_matrix <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = predicted_class
)

print(confusion_matrix)

predicted_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)
predicted_class <- ifelse(predicted_prob >= 0.5, 1, 0)
confusion_matrix <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = predicted_class
)

print(confusion_matrix)
length(predicted_prob)
length(predicted_class)
confusion_matrix <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = predicted_class
)
print(confusion_matrix)


# Extract confusion matrix values
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]
TP <- confusion_matrix[2, 2]

# Calculate performance metrics
accuracy <- (TP + TN) / sum(confusion_matrix)

precision <- TP / (TP + FP)

recall <- TP / (TP + FN)

specificity <- TN / (TN + FP)

f1_score <- 2 * (precision * recall) / (precision + recall)

# Display results
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
cat("Precision:", round(precision * 100, 2), "%\n")
cat("Recall:", round(recall * 100, 2), "%\n")
cat("Specificity:", round(specificity * 100, 2), "%\n")
cat("F1 Score:", round(f1_score * 100, 2), "%\n")


TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]
TP <- confusion_matrix[2, 2]

accuracy <- (TP + TN) / sum(confusion_matrix)

precision <- TP / (TP + FP)

recall <- TP / (TP + FN)

specificity <- TN / (TN + FP)

f1_score <- 2 * (precision * recall) / (precision + recall)
print(accuracy * 100)
print(precision * 100)
print(recall * 100)
print(specificity * 100)
print(f1_score * 100)
confusion_matrix
precision <- 7465 / (7465 + 1356)
recall <- 7465 / (7465 + 1592)
f1_score <- 2 * (precision * recall) / (precision + recall)
print(precision)
print(recall)
print(f1_score)


# Extract values from confusion matrix
TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]

# Calculate Precision
precision <- TP / (TP + FP)

# Calculate Recall
recall <- TP / (TP + FN)

# Calculate F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Display results
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")


print(confusion_matrix)
TP <- 7465
TN <- 10368
FP <- 1356
FN <- 1592

precision <- TP / (TP + FP)

recall <- TP / (TP + FN)

f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("F1 Score:", f1_score, "\n")
2 * (7465 / (7465 + 1356)) * (7465 / (7465 + 1592)) / ((7465 / (7465 + 1356)) + (7465 / (7465 + 1592)))


accuracy <- (10368 + 7465) /
  (10368 + 1356 + 1592 + 7465)

print(accuracy)
TP <- 7465
TN <- 10368
FP <- 1356
FN <- 1592

accuracy <- (TP + TN) / (TP + TN + FP + FN)

print(accuracy)

cat("Accuracy:", accuracy * 100, "%")



install.packages("pROC")
library(pROC)
predicted_probability <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)
roc_curve <- roc(
  test_data$satisfaction_binary,
  predicted_probability
)
auc_score <- auc(roc_curve)

print(auc_score)
library(pROC)
ls()

auc_score <- auc(roc_curve)

print(auc_score)
pROC::auc(roc_curve)
auc_score <- as.numeric(pROC::auc(roc_curve))

print(auc_score)
pROC::auc(roc_curve)
auc_score <- as.numeric(pROC::auc(roc_curve))
print(auc_score)
class(roc_curve)
pROC::auc(roc_curve)
print(auc_score)
plot(roc_curve,
     main = "ROC Curve - Airline Passenger Satisfaction",
     col = "blue",
     lwd = 2)

abline(a = 0, b = 1, lty = 2)
graphics.off()
plot(roc_curve,
     main = "ROC Curve - Airline Passenger Satisfaction",
     col = "blue",
     lwd = 2)
abline(a = 0, b = 1, lty = 2)
auc_score <- as.numeric(pROC::auc(roc_curve))

print(auc_score)
install.packages("caret")
library(caret)
set.seed(123)

train_control <- trainControl(
  method = "cv",
  number = 5
)
cv_model <- train(
  satisfaction_binary ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = train_control
)
print(cv_model)
install.packages("car")
library(car)
vif(logistic_model)


cor(
  model_data$departure_delay_in_minutes,
  model_data$arrival_delay_in_minutes,
  use = "complete.obs"
)
logistic_model2 <- glm(
  satisfaction_binary ~ . - departure_delay_in_minutes,
  data = model_data,
  family = binomial
)
car::vif(logistic_model2)
AIC(logistic_model, logistic_model2)

nobs(logistic_model)
nobs(logistic_model2)

common_data <- model_data[
  complete.cases(model_data),
]

model1 <- glm(
  satisfaction_binary ~ .,
  data = common_data,
  family = binomial
)

model2 <- glm(
  satisfaction_binary ~ . - departure_delay_in_minutes,
  data = common_data,
  family = binomial
)

AIC(model1, model2)

common_data <- model_data[
  complete.cases(model_data),
]
model1 <- glm(
  satisfaction_binary ~ .,
  data = common_data,
  family = binomial
)
model2 <- glm(
  satisfaction_binary ~ . - departure_delay_in_minutes,
  data = common_data,
  family = binomial
)

AIC(model1, model2)
car::vif(model2)

AIC(model1, model2)

prob1 <- predict(
  model1,
  newdata = test_data,
  type = "response"
)

prob2 <- predict(
  model2,
  newdata = test_data,
  type = "response"
)

roc1 <- pROC::roc(
  test_data$satisfaction_binary,
  prob1
)

roc2 <- pROC::roc(
  test_data$satisfaction_binary,
  prob2
)

cat("Model 1 AUC:", as.numeric(pROC::auc(roc1)), "\n")

cat("Model 2 AUC:", as.numeric(pROC::auc(roc2)), "\n")
head(prob2)
prob2 <- predict(
  model2,
  newdata = test_data,
  type = "response"
)

roc2 <- pROC::roc(
  test_data$satisfaction_binary,
  prob2
)
pROC::auc(roc2)
roc1 <- pROC::roc(
  test_data$satisfaction_binary,
  prob1
)

cat("Model 1 AUC:", as.numeric(pROC::auc(roc1)), "\n")

cat("Model 2 AUC:", as.numeric(pROC::auc(roc2)), "\n")
plot(roc1,
     col = "blue",
     lwd = 2,
     main = "ROC Curve Comparison")

plot(roc2,
     col = "red",
     lwd = 2,
     add = TRUE)

legend("bottomright",
       legend = c("Model 1", "Model 2"),
       col = c("blue", "red"),
       lwd = 2)

plot(roc1,
     col = "blue",
     lwd = 2,
     main = "ROC Curve Comparison")

plot(roc2,
     col = "red",
     lwd = 2,
     add = TRUE)

legend("bottomright",
       legend = c("Model 1", "Model 2"),
       col = c("blue", "red"),
       lwd = 2)
# Confusion matrices
cm1 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred1
)

cm2 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred2
)

# Function to calculate metrics
calculate_metrics <- function(cm) {
  
  TN <- cm["0", "0"]
  FP <- cm["0", "1"]
  FN <- cm["1", "0"]
  TP <- cm["1", "1"]
  
  accuracy <- (TP + TN) / sum(cm)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1 <- 2 * precision * recall / (precision + recall)
  
  return(c(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1
  ))
}

# Compare both models
results <- rbind(
  Model_1 = calculate_metrics(cm1),
  Model_2 = calculate_metrics(cm2)
)

print(round(results * 100, 2))

print(cm1)
print(cm2)

# Model 1
prob1 <- predict(model1, newdata = test_data, type = "response")
pred1 <- ifelse(prob1 >= 0.5, 1, 0)

cm1 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred1
)

# Model 2
prob2 <- predict(model2, newdata = test_data, type = "response")
pred2 <- ifelse(prob2 >= 0.5, 1, 0)

cm2 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred2
)

# Display both
print(cm1)
print(cm2)
calculate_metrics <- function(cm) {
  
  TN <- cm["0", "0"]
  FP <- cm["0", "1"]
  FN <- cm["1", "0"]
  TP <- cm["1", "1"]
  
  accuracy <- (TP + TN) / sum(cm)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1 <- 2 * precision * recall / (precision + recall)
  
  c(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1
  )
}

results <- rbind(
  Model_1 = calculate_metrics(cm1),
  Model_2 = calculate_metrics(cm2)
)

print(round(results * 100, 2))

# Generate predictions
prob1 <- predict(model1, newdata = test_data, type = "response")
prob2 <- predict(model2, newdata = test_data, type = "response")

pred1 <- ifelse(prob1 >= 0.5, 1, 0)
pred2 <- ifelse(prob2 >= 0.5, 1, 0)

# Create confusion matrices
cm1 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred1
)

cm2 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred2
)

# Calculate metrics
get_metrics <- function(cm) {
  TN <- cm["0", "0"]
  FP <- cm["0", "1"]
  FN <- cm["1", "0"]
  TP <- cm["1", "1"]
  
  c(
    Accuracy = (TP + TN) / sum(cm),
    Precision = TP / (TP + FP),
    Recall = TP / (TP + FN),
    F1_Score = 2 * TP / (2 * TP + FP + FN)
  )
}

# Create final comparison table
results <- rbind(
  Model_1 = get_metrics(cm1),
  Model_2 = get_metrics(cm2)
)

# Display results as percentages
round(results * 100, 2)
exists("model1")
exists("model2")
exists("test_data")
exists("cm1")
exists("cm2")

prob2 <- predict(model2, newdata = test_data, type = "response")
pred2 <- ifelse(prob2 >= 0.5, 1, 0)
cm2 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred2
)

print(cm2)
prob2 <- predict(model2, newdata = test_data, type = "response")
head(prob2)

pred2 <- ifelse(prob2 >= 0.5, 1, 0)

cm2 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred2
)

print(cm2)
pred2 <- ifelse(prob2 >= 0.5, 1, 0)
head(pred2)
head(test_data$satisfaction_binary)
cm2 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred2
)
print(cm2)

TN <- cm2["0", "0"]
FP <- cm2["0", "1"]
FN <- cm2["1", "0"]
TP <- cm2["1", "1"]

accuracy2 <- (TP + TN) / sum(cm2)
precision2 <- TP / (TP + FP)
recall2 <- TP / (TP + FN)
f1_2 <- 2 * precision2 * recall2 / (precision2 + recall2)

cat("Accuracy:", round(accuracy2 * 100, 2), "%\n")
cat("Precision:", round(precision2 * 100, 2), "%\n")
cat("Recall:", round(recall2 * 100, 2), "%\n")
cat("F1 Score:", round(f1_2 * 100, 2), "%\n")


TN <- cm2["0", "0"]
FP <- cm2["0", "1"]
FN <- cm2["1", "0"]
TP <- cm2["1", "1"]

accuracy2 <- (TP + TN) / sum(cm2)
precision2 <- TP / (TP + FP)
recall2 <- TP / (TP + FN)

f1_2 <- 2 * precision2 * recall2 /
  (precision2 + recall2)

print(accuracy2)
print(precision2)
print(recall2)
print(f1_2)
f1_2 <- (2 * 7473) / (2 * 7473 + 1355 + 1584)
print(f1_2)
print(cm1)


prob1 <- predict(model1, newdata = test_data, type = "response")

pred1 <- ifelse(prob1 >= 0.5, 1, 0)
cm1 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred1
)

print(cm1)
prob1 <- predict(model1, newdata = test_data, type = "response")
head(prob1)
pred1 <- ifelse(prob1 >= 0.5, 1, 0)
head(pred1)
head(test_data$satisfaction_binary)
cm1 <- table(
  Actual = test_data$satisfaction_binary,
  Predicted = pred1
)
print(cm1)


TN1 <- cm1["0", "0"]
FP1 <- cm1["0", "1"]
FN1 <- cm1["1", "0"]
TP1 <- cm1["1", "1"]

accuracy1 <- (TP1 + TN1) / sum(cm1)
precision1 <- TP1 / (TP1 + FP1)
recall1 <- TP1 / (TP1 + FN1)
f1_1 <- 2 * precision1 * recall1 /
  (precision1 + recall1)

round(c(
  Accuracy = accuracy1,
  Precision = precision1,
  Recall = recall1,
  F1_Score = f1_1
) * 100, 2)
accuracy1 <- (10358 + 7471) / (10358 + 1366 + 1586 + 7471)
print(accuracy1 * 100)
TN1 <- 10358
FP1 <- 1366
FN1 <- 1586
TP1 <- 7471

accuracy1 <- (TP1 + TN1) / (TP1 + TN1 + FP1 + FN1)
precision1 <- TP1 / (TP1 + FP1)
recall1 <- TP1 / (TP1 + FN1)
f1_1 <- 2 * precision1 * recall1 / (precision1 + recall1)

round(c(
  Accuracy = accuracy1,
  Precision = precision1,
  Recall = recall1,
  F1_Score = f1_1
) * 100, 2)
TN1 <- 10358
FP1 <- 1366
FN1 <- 1586
TP1 <- 7471

accuracy1 <- (TP1 + TN1) / (TP1 + TN1 + FP1 + FN1)

precision1 <- TP1 / (TP1 + FP1)

recall1 <- TP1 / (TP1 + FN1)

f1_1 <- 2 * TP1 / (2 * TP1 + FP1 + FN1)

model1_results <- c(
  Accuracy = accuracy1,
  Precision = precision1,
  Recall = recall1,
  F1_Score = f1_1
)

print(round(model1_results * 100, 2))
round(c(
  Accuracy = (7471 + 10358) / 20781,
  Precision = 7471 / (7471 + 1366),
  Recall = 7471 / (7471 + 1586),
  F1_Score = (2 * 7471) / (2 * 7471 + 1366 + 1586)
) * 100, 2)

# Model 2 diagnostic plots
par(mfrow = c(2, 2))

plot(model2)

par(mfrow = c(1, 1))

# Cook's distance
cooks_values <- cooks.distance(model2)

plot(cooks_values,
     type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance")

abline(h = 4/length(cooks_values), col = "red")
cooks_values <- cooks.distance(model2)
plot(cooks_values,
     type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance",
     xlab = "Observation")

abline(h = 4 / length(cooks_values), col = "red")

summary(cooks_values)
summary(model2)
