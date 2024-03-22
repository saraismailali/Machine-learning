# Install packages
install.packages(“e1071”)
install.packages(“dplyr”)
install.packages(“ggplot2”)

# Load libraries
library(e1071)
library(dplyr)
library(ggplot2)

# Load data
train <- read.csv("C:/Users/ismai/Downloads/train (1).csv", stringsAsFactors = FALSE)
test <- read.csv("C:/Users/ismai/Downloads/test (1).csv", stringsAsFactors = FALSE)

# Fill missing values in Age and Fare
train$Age[is.na(train$Age)] <- median(train$Age, na.rm = TRUE)
train$Fare[is.na(train$Fare)] <- median(train$Fare, na.rm = TRUE)

test$Age[is.na(test$age)] <- median(test$age, na.rm = TRUE)
test$Fare[is.na(test$Fare)] <- median(test$Fare, na.rm = TRUE)

# Keep only the first character of the Cabin and fill missing values with “U” (for Unknown)
train$Cabin <- substr(train$Cabin, 1, 1)
train$Cabin[is.na(train$Cabin)] <- "U"

test$Cabin <- substr(test$Cabin, 1, 1)
test$Cabin[is.na(test$Cabin)] <- "U"

# Convert categorical variables to factors
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked[is.na(train$Embarked)] <- "S"
train$Embarked <- as.factor(train$Embarked)
train$Cabin <- as.factor(train$Cabin)

test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked[is.na(test$Embarked)] <- "S"
test$Embarked <- as.factor(test$Embarked)
test$Cabin <- as.factor(test$Cabin)

# Train the Naive Bayes model
naive_bayes_model <- naiveBayes(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked, data = train)

# Make predictions on the test data
test_pred <- predict(naive_bayes_model, test)
# Create a confusion matrix for training data
train_pred <- predict(naive_bayes_model, train)
confusion_matrix <- table(Original = train$Survived, Predicted = train_pred)
print(confusion_matrix)
#Append predictions to the test data
test$Predicted <- as.factor(test_pred)

#Prepare the data for bar plots.

# Create frequency tables for train and test data
train_tab <- table(train$Survived)
test_tab <- table(test$Predicted)

# Preparing data for the bar plots
bar_train <- data.frame(Category = "Training", Class = factor(names(train_tab), levels = c("0", "1")), Count = as.vector(train_tab))
bar_test <- data.frame(Category = "Predicted", Class = factor(names(test_tab), levels = c("0", "1")), Count = as.vector(test_tab))

# Combine the data frames
bar_data <- rbind(bar_train, bar_test)

#Create bar plots and display them.
# Bar plot
ggplot(bar_data, aes(x = Category, y = Count, fill = Class, label = Count)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(vjust = -1, position = position_dodge(width = 1), size = 5) +
  labs(title = "survived vs predicted", x = 'category', y = 'frequency', fill = "survived") +
  theme_minimal()

