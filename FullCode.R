# ----------------
# DATA HANDLING
# ----------------

library(tidyverse)
library(dplyr)
library(writexl)
library(openxlsx)
library(ggplot2)
library(lubridate)

# Set the path for datasets
datasets_path = "datasets/"

# Read datasets into separate data frames
account <- read.csv(paste0(datasets_path, "account.csv"), sep = ";")
card_dev <- read.csv(paste0(datasets_path, "card_dev.csv"), sep = ";")
client <- read.csv(paste0(datasets_path, "client.csv"), sep = ";")
disp <- read.csv(paste0(datasets_path, "disp.csv"), sep = ";")
district <- read.csv(paste0(datasets_path, "district.csv"), sep = ";")
loan_dev <- read.csv(paste0(datasets_path, "loan_dev.csv"), sep = ";")
trans_dev <- read.csv(paste0(datasets_path, "trans_dev.csv"), sep = ";")

ggplot(trans_dev, aes(x = amount)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Transaction Amounts (before outlier removal)",
       x = "Transaction Amount",
       y = "Frequency")

Q1 <- quantile(trans_dev$amount, 0.25)
Q3 <- quantile(trans_dev$amount, 0.75)
IQR <- Q3 - Q1
upper_limit <- Q3 + 1.5 * IQR
trans_dev <- trans_dev[trans_dev$amount <= upper_limit, ]

missing_values <- sapply(list(account, card_dev, client, disp, district, loan_dev, trans_dev), function(df) sum(is.na(df)))
cat("Missing Values Summary:\n")
print(missing_values)

# Convert date columns to Date format
account$date <- as.Date(sprintf("19%s", account$date), format = "%Y%m%d")
loan_dev$date <- as.Date(sprintf("19%s", loan_dev$date), format = "%Y%m%d")
trans_dev$date <- as.Date(sprintf("19%s", trans_dev$date), format = "%Y%m%d")
card_dev$date <- as.Date(sprintf("19%s", card_dev$issued), format = "%Y%m%d")
client$date <- as.Date(sprintf("19%s", client$birth_number), format = "%Y%m%d")

# Display summary statistics for each dataset
summary(account)
summary(card_dev)
summary(client)
summary(disp)
summary(district)
summary(loan_dev)
summary(trans_dev)

ggplot(trans_dev, aes(x = amount)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Transaction Amounts (Outliers Removed)",
       x = "Transaction Amount",
       y = "Frequency")

# Remove duplicate rows in disp and trans_dev datasets
disp <- unique(disp)
trans_dev <- unique(trans_dev)

# Merge datasets step by step using left joins
merged_data_step1 <- account %>%
  left_join(card_dev, by = c("account_id" = "disp_id"))

merged_data_step2 <- merged_data_step1 %>%
  left_join(disp, by = "account_id")

merged_data_step3 <- merged_data_step2 %>%
  left_join(client, by = "client_id")

# Rename the "code" column in district to avoid naming conflicts
colnames(district)[colnames(district) == "code"] <- "district_id_district"

merged_data_step4 <- merged_data_step3 %>%
  left_join(district, by = c("district_id.x" = "district_id_district"))

merged_data_step5 <- merged_data_step4 %>%
  left_join(loan_dev, by = "account_id")

# Merge the final dataset with trans_dev using many-to-many relationship
dados <- merged_data_step5 %>%
  left_join(trans_dev, by = "account_id", suffix = c("_merged", "_trans"), relationship = "many-to-many")

# Display the structure of the final merged dataset
str(dados)

# Eliminated this column because it was the same than client_id but less general
dados <- dados[,-c(8)]

# Remove the columns that have a NAs percentage above the threshold
remove_high_missing_features <- function(data, threshold) {
  # Calculate the threshold for missing values
  max_missing_values <- nrow(data) * threshold
  
  # Identify columns with more missing values than the threshold
  high_missing_columns <- colnames(data)[apply(data, 2, function(x) sum(is.na(x)) > max_missing_values)]
  
  # Remove high missing value columns from the dataset
  data <- data[, setdiff(colnames(data), high_missing_columns)]
  
  return(data)
}

dados <- remove_high_missing_features(dados, 0.75)

# Exportar dados para Excel
# write_xlsx(dados, path = paste0(datasets_path,"dados_finais.xlsx"))

# Remove duplicate rows based on the account_id column
cleaned_data <- dados %>%
  distinct(account_id, .keep_all = TRUE)

# Check for duplicate values in the account_id column of the cleaned data frame
duplicated_cleaned_data <- cleaned_data$account_id[!is.na(cleaned_data$account_id) & duplicated(cleaned_data$account_id)]

if (length(duplicated_cleaned_data) > 0) {
  cat("Duplicate values found in account_id column of the cleaned data frame.\n")
  print(duplicated_cleaned_data)
  stop("Please handle the duplicate values in the account_id column.")
} else {
  cat("No duplicate values found in account_id column of the cleaned data frame.\n")
}

#Function to convert categorical collumns into numeric
convertToNumeric <- function(data, columns) {
  for (col in columns) {
    data[[col]] <- as.numeric(as.character(data[[col]]))
  }
  return(data)
}
# Numeric variables assigned as categorical
cat_to_num <- c('unemploymant.rate..95', 'no..of.commited.crimes..95')
# Convert them to numeric
dados <- convertToNumeric(dados, cat_to_num)

# -------------------
# BINARIZATION
# -------------------

# Binarization Function
convert_to_binary_columns <- function(data, cat_col) {
  # Make sure the specified column exists in the data frame
  if (!(cat_col %in% names(data))) {
    stop("Specified column does not exist in the data frame.")
  }
  
  # Get unique values in the specified column
  unique_values <- unique(data[[cat_col]])
  
  # Create a placeholder for new columns
  new_columns <- data.frame(matrix(NA, ncol = 0, nrow = nrow(data)))
  
  # Check if unique_values is empty
  if (length(unique_values) == 0) {
    warning("No unique values found in ", cat_col)
    return(data)
  }
  
  # Iterate over unique values and create binary columns
  for (value in unique_values) {
    # Create a binary column for each unique value
    binary_column_name <- paste(cat_col, value, sep = "_")
    binary_column_name <- gsub(" ", "_", binary_column_name)  # Replace spaces with underscores
    binary_column_name <- gsub(" - ", "-", binary_column_name)  # Replace " - " with "-"
    new_columns[binary_column_name] <- as.integer(data[[cat_col]] == value)
  }
  
  # Remove the original categorical column
  data <- data[, !(names(data) %in% cat_col)]
  
  # Insert new columns at the end of the data frame
  data <- cbind(data, new_columns)
  
  return(data)
}

# Apply binarization to relevant categorical columns
columns_to_binarize <- colnames(dados)[sapply(dados, is.character)]
columns_to_binarize <- columns_to_binarize[sapply(columns_to_binarize, function(col) n_distinct(dados[[col]]) > 1)]

# Execute the linearization
for (col in columns_to_binarize) {
  dados <- convert_to_binary_columns(dados, col)
}

# Clean features generated with solely Missing Values or of Data variables, 
# since they will not influence the predictions and pattern finding of the study of the dataset
for (col in colnames(dados)){
  if(class(dados$col)=="Data" || all(is.na(dados[[col]]))){
    dados <- dados[, !colnames(dados) %in% col]
  }
}

# Identify the position of the "account" column and move it to the end, since it is the evaluation feature 
account_col_index <- which(names(dados) == "account")
dados <- dados[, c(setdiff(seq_along(dados), account_col_index), account_col_index)]

# Remove date columns
date_columns <- sapply(dados, is.Date)
dados <- dados[, !date_columns]

# Remove rows with missing values
dados <- na.omit(dados)

# Identify constant columns
constant_columns <- colnames(dados)[apply(dados, 2, function(col) length(unique(col)) == 1)]

# Remove constant columns from the dataset
dados <- dados[, -which(colnames(dados) %in% constant_columns)]

# --------------------------
# CLEAN TEMPORARY VARIABLES
# --------------------------
# Since the main goal of the last procedures was to generate the dataframe,
# it can be erased everything temporary past this, for efficiency sake
rm(list=setdiff(ls(), "dados"))
# Clear Unused R Memory
gc()

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
top_25_correlations <- head(filtered_combined_correlation_data, 25)
print(top_25_correlations)

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

# -------------
# CLUSTERING MODELS
# ---------------

# Load necessary libraries
library(cluster)
library(fpc)
library(ggplot2)

# Remove rows with missing values
df <- na.omit(df)

# Function to calculate total within-cluster sum of squares (WCSS)
calculate_wcss <- function(data, k) {
  kmeans_model <- kmeans(data, centers = k)
  return(kmeans_model$tot.withinss)
}

# Range of k values to explore
k_values <- 2:10  # You can adjust the range as needed

# Calculate WCSS for each k
wcss_values <- sapply(k_values, function(k) calculate_wcss(df, k))

# Plot the elbow curve
elbow_plot <- ggplot() +
  geom_line(aes(x = k_values, y = wcss_values), color = "blue") +
  geom_point(aes(x = k_values, y = wcss_values), color = "red", size = 2) +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal()

print(elbow_plot)

# Identify the optimal k (elbow point)
optimal_k <- k_values[which.min(diff(wcss_values))]
cat("Optimal k:", optimal_k, "\n")

# K-means clustering
k_means <- kmeans(df, centers = optimal_k)
silhouette_kmeans <- silhouette(k_means$cluster, dist(df))

# PAM (Partitioning Around Medoids)
pam_result <- pam(df, k = optimal_k)
silhouette_pam <- silhouette(pam_result$cluster, dist(df))

# DBSCAN
dbscan_result <- dbscan(df, eps = 0.5, MinPts = 5)
silhouette_dbscan <- silhouette(dbscan_result$cluster, dist(df))

# Hierarchical clustering (single link, complete link, average link)
single_link <- hclust(dist(df), method = "single")
complete_link <- hclust(dist(df), method = "complete")
average_link <- hclust(dist(df), method = "average")

cutree_single <- cutree(single_link, k = optimal_k)
cutree_complete <- cutree(complete_link, k = optimal_k)
cutree_average <- cutree(average_link, k = optimal_k)

silhouette_single <- silhouette(cutree_single, dist(df))
silhouette_complete <- silhouette(cutree_complete, dist(df))
silhouette_average <- silhouette(cutree_average, dist(df))

# Print silhouette scores
cat("Silhouette Score - K-means:", mean(silhouette_kmeans[, optimal_k]), "\n")
cat("Silhouette Score - PAM:", mean(silhouette_pam[, ]), "\n")
# Not applicable
#cat("Silhouette Score - DBSCAN:", mean_silhouette_dbscan, "\n")
cat("Silhouette Score - Hierarchical (Single Link):", mean(silhouette_single[, optimal_k]), "\n")
cat("Silhouette Score - Hierarchical (Complete Link):", mean(silhouette_complete[, optimal_k]), "\n")
cat("Silhouette Score - Hierarchical (Average Link):", mean(silhouette_average[, optimal_k]), "\n")

# Visualize the silhouette plots 
plot(silhouette_kmeans, main = "Silhouette Plot - K-means")
plot(silhouette_pam, main = "Silhouette Plot - PAM")
# Not applicable
#plot(silhouette_dbscan, main = "Silhouette Plot - DBSCAN")
plot(silhouette_single, main = "Silhouette Plot - Hierarchical (Single Link)")
plot(silhouette_complete, main = "Silhouette Plot - Hierarchical (Complete Link)")
plot(silhouette_average, main = "Silhouette Plot - Hierarchical (Average Link)")

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

