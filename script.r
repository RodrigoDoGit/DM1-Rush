# Load necessary libraries
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
