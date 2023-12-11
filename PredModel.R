library(lubridate)
library(tidyverse)
library(MASS)

# --------------------
# PREDICTIVE MODELING
# --------------------
# X -> train set (without answer feature 'account')
# Y -> test set (feature 'account')
X <- dados[, -which(colnames(dados) == "account")]
Y <- dados$account

# Identify numeric columns
numeric_columns <- sapply(X, is.numeric)

# Remove constant variables
constant_columns <- colnames(X)[apply(X[, numeric_columns], 2, function(x) length(unique(x)) == 1)]
X <- X[, !colnames(X) %in% constant_columns]

# Scale numeric columns individually
X[, numeric_columns] <- lapply(X[, numeric_columns], scale)

# Training and testing values
set.seed(123)
train.length <- round(nrow(X) * 0.7)
rd <- sample(1:nrow(X))
train <- rd[1:train.length]
test <- rd[(train.length + 1):nrow(X)]

# Create training and testing sets
X_train <- X[train, ]
Y_train <- Y[train]
X_test <- X[test, ]
Y_test <- Y[test]

# Check unique values in the grouping variable
unique_values_Y_train <- unique(Y_train)
print("Unique values in Y_train:")
print(unique_values_Y_train)

# Convert 'account' to a data frame or tibble
target_df <- data.frame(account = Y_train$account)

# Combine target variable with the selected features
data_subset <- cbind(X_train[, c(33, 118, 189, 258, 259, 260, 262, 265, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282)], target = target_df$account)

# Create a pair plot
library(GGally)
ggpairs(data_subset)



# --------------------
# LINEAR DISCRIMINANT ANALYSIS (LDA)
# --------------------
# Identify constant variables
constant_columns <- sapply(X_train, function(x) length(unique(x, na.rm = TRUE)) == 1)

# Remove constant variables
X_train <- X_train[, !constant_columns]
X_test <- X_test[, !constant_columns]

# Check if any constant variables remain
constant_variables_remaining <- colnames(X_train)[apply(X_train, 2, function(x) length(unique(x, na.rm = TRUE)) == 1)]

if (length(constant_variables_remaining) > 0) {
  print("Constant variables remaining:")
  print(constant_variables_remaining)
  stop("Please handle the remaining constant variables.")
}

# Perform LDA
lda_model <- lda(Y_train ~ ., data = X_train)

# Make predictions on the test set
lda_pred <- predict(lda_model, newdata = X_test)

# Evaluate LDA performance (you can replace this with your own evaluation metrics)
confusion_matrix_lda <- table(lda_pred$class, Y_test)
print("Confusion Matrix for LDA:")
print(confusion_matrix_lda)

# --------------------
# QUADRATIC DISCRIMINANT ANALYSIS (QDA)
# --------------------
qda_model <- qda(Y_train ~ ., data = X_train)

# Make predictions on the test set
qda_pred <- predict(qda_model, newdata = X_test)

# Evaluate QDA performance (you can replace this with your own evaluation metrics)
confusion_matrix_qda <- table(qda_pred$class, Y_test)
print("Confusion Matrix for QDA:")
print(confusion_matrix_qda)


