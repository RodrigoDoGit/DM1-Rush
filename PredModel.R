library(lubridate)
library(tidyverse)
library(MASS)
library(caret)

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

# Remove constant variables from training and test sets
X_train <- X_train[, !colnames(X_train) %in% constant_columns]
X_test <- X_test[, !colnames(X_test) %in% constant_columns]
