library(tidyverse)

# ------------------------------
# CORRELATION MATRIX EVALUATION
# ------------------------------

# Identify numeric columns in the dataset
numeric_columns <- sapply(dados, is.numeric)

# Create a numeric matrix from numeric columns
numeric_matrix <- dados[, numeric_columns]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_matrix)

# Find significant positive correlations
positive_correlations <- which(cor_matrix >= 0.5 & cor_matrix != 1, arr.ind = TRUE)

# Find significant negative correlations
negative_correlations <- which(cor_matrix <= -0.5 & cor_matrix != 1, arr.ind = TRUE)

# Filter out correlations with the same prefix up until the underscore
filter_correlations <- function(correlation_data) {
  correlation_data <- data.frame(
    Variable1 = colnames(cor_matrix)[correlation_data[, 1]],
    Variable2 = colnames(cor_matrix)[correlation_data[, 2]],
    Correlation = cor_matrix[correlation_data],
    stringsAsFactors = FALSE
  )
  
  # Ensure only one of each pair is included
  correlation_data <- subset(correlation_data, Variable1 < Variable2)
  
  # Filter out correlations with the same prefix
  correlation_data <- correlation_data[!grepl("^\\w+_", correlation_data$Variable1) |
                                         !grepl("^\\w+_", correlation_data$Variable2), ]
  
  return(correlation_data)
}

# Create filtered positive and negative correlation data
filtered_positive_correlation_data <- filter_correlations(positive_correlations)
filtered_negative_correlation_data <- filter_correlations(negative_correlations)

# Combine filtered positive and negative correlation data
filtered_combined_correlation_data <- rbind(filtered_positive_correlation_data, filtered_negative_correlation_data)

# Order the data frame by the absolute value of correlation in descending order
filtered_combined_correlation_data <- filtered_combined_correlation_data[order(-abs(filtered_combined_correlation_data$Correlation)), ]

# Get correlations
top_30_correlations <- head(filtered_combined_correlation_data, 30)
print(top_30_correlations)

# ------------------------------
# PRINCIPAL COMPONENT ANALYSIS
# ------------------------------

# ------------------------
# Ajusting size of dataset
# ------------------------
# Set seed for reproducibility
set.seed(123) 
# Adjust the size to 25%, since the dataset is huge and it crashes when we handle it at full size
sampled_data <- dados[sample(nrow(dados), size = (length(dados)/100)*25), ]  
numeric_matrix <- sampled_data[, sapply(sampled_data, is.numeric)]
# ------------------------

# Remove possible constant columns
non_constant_columns <- sapply(numeric_matrix, function(col) length(unique(col)) > 1)
numeric_matrix <- numeric_matrix[, non_constant_columns]

# Empty dataframe to to store the clustering algos in the future
df <- data.frame()

# Check if there are still numeric columns remaining
if (sum(non_constant_columns) > 0) {
  # Handle missing values by imputing with column means
  col_means <- colMeans(numeric_matrix, na.rm = TRUE)
  for (col in colnames(numeric_matrix)) {
    missing_values <- is.na(numeric_matrix[[col]])
    numeric_matrix[[col]][missing_values] <- col_means[col]
  }
  
  # Apply PCA
  pc <- prcomp(numeric_matrix, scale = TRUE)
  
  # Display PCA results
  print(summary(pc)$importance)
  
  # Extract cumulative variance of the first 10 components
  cumulative_variance <- cumsum(pc$sdev^2) / sum(pc$sdev^2)
  cumulative_variance_10 <- cumulative_variance[10]
  print(paste0("Cumulative Variance of the first 10 components: ", round(cumulative_variance_10 * 100, 2), "%"))
  
  # Plot scree plot
  plot(pc)
  
  # --------------
  # CLUSTER GEN
  # --------------
  
  df <- as.data.frame(pc$x)
  df <- scale(df)
  
} else {
  warning("No non-constant numeric columns remaining after removing constant columns.")
}

# The clustering Algos Dataframe!
# print(df)


