transactions <- read.csv("C:/Downloads/creditcard.csv/creditcard.csv") 
library(caTools)

# Split the data into training and testing sets
split <- sample.split(transactions$Class, SplitRatio = 0.7)
train <- transactions[split, ]
test <- transactions[!split, ]

library(rstanarm)

# Define the formula for the Bayesian logistic regression model
formula <- Class ~ Amount

# Fit the Bayesian logistic regression model
model <- stan_glm(formula, data = train, family = binomial(), chains = 4, iter = 1000)

# Extract the posterior samples from the model
posterior_samples <- posterior_predict(model, newdata = test, draws = 100)

# Calculate the predicted fraud probabilities using the posterior samples
predicted_probs <- colMeans(1 / (1 + exp(-posterior_samples)), na.rm = TRUE)

# Classify transactions as fraud or non-fraud based on a threshold
threshold <- 0.5
test$predicted_fraud <- ifelse(predicted_probs >= threshold, 1, 0)

# Calculate performance metrics
accuracy <- (TP + TN) / nrow(test)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
F1_score <- 2 * precision * recall / (precision + recall)

cat(sprintf("Accuracy: %.2f%%\n", accuracy * 100))
cat(sprintf("Precision: %.2f%%\n", precision * 100))
cat(sprintf("Recall: %.2f%%\n", recall * 100))
cat(sprintf("F1-score: %.2f%%\n", F1_score * 100))


# Bayesian test
#null hypothesis= There is no difference in the probability of fraud between the two groups.
#alt hypothesis= there is a difference in the probability of fraud.

# Calculate the posterior probability of fraud for each transaction
posterior_probs_fraud <- posterior_samples[, 1]

# Calculate the Bayesian p-value for each transaction
p_values <- 2 * pnorm(-abs(posterior_probs_fraud))
print(p_values)

# Calculate the test statistic
test_statistic <- t.test(predicted_probs ~ test$Class, var.equal = TRUE)$statistic

# Calculate the p-value
p_value <- 2 * pnorm(-abs(test_statistic))

print(p_value)

if (p_value < 0.05) {
  cat("We reject the null hypothesis")
} else {
  cat("We fail to reject the null hypothesis")
}


