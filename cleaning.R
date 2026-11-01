# Install the packages (only required the first time)
install.packages("tidyverse")
install.packages("janitor")

# Load packages whenever you start the project
library(tidyverse)
library(janitor)

install.packages("devtools")
devtools::install_github("r-lib/conflicted")

library(conflicted)
library(dplyr)

filter(mtcars, cyl == 8)

df <- read.csv("")
# Import the original dataset
raw_data <- read.csv(
  "",
  stringsAsFactors = FALSE
)

# Clean the column names
raw_data <- janitor::clean_names(raw_data)

# Display the first six rows
head(raw_data)

# Display the number of rows and columns
dim(raw_data)

# Examine the structure of the dataset
str(raw_data)

# Generate summary statistics
summary(raw_data)

# Display column names
names(raw_data)

# Count observations and variables
nrow(raw_data)
ncol(raw_data)


# Count missing values in every column
missing_count <- colSums(is.na(raw_data))

# Display columns with missing values first
missing_count <- sort(
  missing_count,
  decreasing = TRUE
)

print(missing_count)

sum(is.na(raw_data))
# Count missing values in each column
colSums(is.na(raw_data))
# Display only columns containing missing values
colSums(is.na(raw_data))[colSums(is.na(raw_data)) > 0]
mean(raw_data$arrival_delay_in_minutes, na.rm = TRUE)
raw_data$arrival_delay_in_minutes[
  is.na(raw_data$arrival_delay_in_minutes)
] <- mean(raw_data$arrival_delay_in_minutes, na.rm = TRUE)

sum(is.na(raw_data))

# Count duplicate rows
sum(duplicated(raw_data))
# Display duplicate records
raw_data[duplicated(raw_data), ]
# Boxplot for age
boxplot(raw_data$age,
        main = "Outlier Detection in Age",
        ylab = "Age",
        col = "lightblue")


boxplot(raw_data$flight_distance,
        main = "Outlier Detection in Flight Distance",
        ylab = "Flight Distance",
        col = "lightgreen")
boxplot(raw_data$arrival_delay_in_minutes,
        main = "Outlier Detection in Arrival Delay",
        ylab = "Arrival Delay (Minutes)",
        col = "lightpink")

# Age
boxplot.stats(raw_data$age)$out

# Flight distance
boxplot.stats(raw_data$flight_distance)$out

# Arrival delay
boxplot.stats(raw_data$arrival_delay_in_minutes)$out




# Calculate the percentage of missing values
missing_percentage <- colMeans(is.na(raw_data)) * 100

print(round(missing_percentage, 2))
length(boxplot.stats(raw_data$age)$out)

length(boxplot.stats(raw_data$flight_distance)$out)

length(boxplot.stats(raw_data$arrival_delay_in_minutes)$out)

cat("Age:", length(boxplot.stats(raw_data$age)$out), "\n")

cat("Flight Distance:", length(boxplot.stats(raw_data$flight_distance)$out), "\n")

cat("Arrival Delay:", length(boxplot.stats(raw_data$arrival_delay_in_minutes)$out), "\n")


length(boxplot.stats(raw_data$age)$out)
length(boxplot.stats(raw_data$flight_distance)$out)
length(boxplot.stats(raw_data$arrival_delay_in_minutes)$out)

summary(raw_data$flight_distance)

summary(raw_data$arrival_delay_in_minutes)
quantile(raw_data$arrival_delay_in_minutes,
         probs = c(0.25, 0.75),
         na.rm = TRUE)
Q1 <- quantile(raw_data$arrival_delay_in_minutes, 0.25)

Q3 <- quantile(raw_data$arrival_delay_in_minutes, 0.75)

IQR_value <- IQR(raw_data$arrival_delay_in_minutes)

lower_limit <- Q1 - 1.5 * IQR_value

upper_limit <- Q3 + 1.5 * IQR_value

lower_limit
upper_limit

# Min-Max normalization

raw_data$age_normalized <-
  (raw_data$age - min(raw_data$age)) /
  (max(raw_data$age) - min(raw_data$age))

raw_data$flight_distance_normalized <-
  (raw_data$flight_distance - min(raw_data$flight_distance)) /
  (max(raw_data$flight_distance) - min(raw_data$flight_distance))

raw_data$arrival_delay_normalized <-
  (raw_data$arrival_delay_in_minutes -
     min(raw_data$arrival_delay_in_minutes)) /
  (max(raw_data$arrival_delay_in_minutes) -
     min(raw_data$arrival_delay_in_minutes))
summary(raw_data$age_normalized)

summary(raw_data$flight_distance_normalized)

summary(raw_data$arrival_delay_normalized)

summary(raw_data$age_normalized)

summary(raw_data$flight_distance_normalized)

class(raw_data$age)
class(raw_data$flight_distance)
raw_data$age_normalized <-
  (raw_data$age - min(raw_data$age, na.rm = TRUE)) /
  (max(raw_data$age, na.rm = TRUE) -
     min(raw_data$age, na.rm = TRUE))

raw_data$flight_distance_normalized <-
  (raw_data$flight_distance -
     min(raw_data$flight_distance, na.rm = TRUE)) /
  (max(raw_data$flight_distance, na.rm = TRUE) -
     min(raw_data$flight_distance, na.rm = TRUE))
summary(raw_data$age_normalized)

summary(raw_data$flight_distance_normalized)
summary(raw_data$age_normalized)



raw_data$age_normalized <-
  (raw_data$age - min(raw_data$age, na.rm = TRUE)) /
  (max(raw_data$age, na.rm = TRUE) -
     min(raw_data$age, na.rm = TRUE))

"age_normalized" %in% names(raw_data)

summary(raw_data$age_normalized)

unique(raw_data$gender)

unique(raw_data$customer_type)

unique(raw_data$type_of_travel)

unique(raw_data$class)

unique(raw_data$satisfaction)

raw_data$satisfaction_encoded <- ifelse(
  raw_data$satisfaction == "satisfied", 1, 0
)

table(raw_data$satisfaction,
      raw_data$satisfaction_encoded)
head(raw_data[, c("satisfaction",
                  "satisfaction_encoded")])


unique(raw_data$gender)

unique(raw_data$customer_type)

unique(raw_data$type_of_travel)

unique(raw_data$class)

raw_data$class_encoded <- as.numeric(
  factor(raw_data$class,
         levels = c("Eco", "Eco Plus", "Business"))
) - 1

table(raw_data$class, raw_data$class_encoded)
unique(raw_data$gender)
unique(raw_data$customer_type)
unique(raw_data$type_of_travel)

raw_data$travel_type_encoded <- ifelse(
  raw_data$type_of_travel == "Business travel",
  1, 0
)
table(raw_data$type_of_travel,
      raw_data$travel_type_encoded)
unique(raw_data$gender)
unique(raw_data$customer_type)

raw_data$customer_type_encoded <- ifelse(
  raw_data$customer_type == "Loyal Customer",
  1, 0
)
table(raw_data$customer_type,
      raw_data$customer_type_encoded)
unique(raw_data$gender)
raw_data$gender_encoded <- ifelse(
  raw_data$gender == "Male",
  1, 0
)
table(raw_data$gender, raw_data$gender_encoded)
head(raw_data[, c(
  "gender", "gender_encoded",
  "customer_type", "customer_type_encoded",
  "type_of_travel", "travel_type_encoded",
  "class", "class_encoded",
  "satisfaction", "satisfaction_encoded"
)])


dim(raw_data)
sum(is.na(raw_data))
sum(duplicated(raw_data))
str(raw_data)
summary(raw_data)
write.csv(raw_data,
          "airline_cleaned_dataset.csv",
          row.names = FALSE)

( View(raw_data)
head(raw_data, 10)
View(raw_data[, c(
  "age_normalized",
  "flight_distance_normalized",
  "arrival_delay_normalized",
  "gender_encoded",
  "customer_type_encoded",
  "travel_type_encoded",
  "class_encoded",
  "satisfaction_encoded"
)])
write.csv(raw_data,
          "airline_cleaned_dataset.csv",
          row.names = FALSE)
getwd()

raw_data$arrival_delay_in_minutes[
  is.na(raw_data$arrival_delay_in_minutes)
] <- mean(
  raw_data$arrival_delay_in_minutes,
  na.rm = TRUE
)

sum(is.na(raw_data))
