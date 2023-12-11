library(dplyr)
library(ggplot2)

account <- read.csv("account.csv", sep = ";")
card_dev <- read.csv("card_dev.csv", sep = ";")
client <- read.csv("client.csv", sep = ";")
disp <- read.csv("disp.csv", sep = ";")
district <- read.csv("district.csv", sep = ";")
loan_dev <- read.csv("loan_dev.csv", sep = ";")
trans_dev <- read.csv("trans_dev.csv", sep = ";")

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

account$date <- as.Date(sprintf("19%s", account$date), format = "%Y%m%d")
loan_dev$date <- as.Date(sprintf("19%s", loan_dev$date), format = "%Y%m%d")
trans_dev$date <- as.Date(sprintf("19%s", trans_dev$date), format = "%Y%m%d")
card_dev$date <- as.Date(sprintf("19%s", card_dev$issued), format = "%Y%m%d")
client$date <- as.Date(sprintf("19%s", client$birth_number), format = "%Y%m%d")

ggplot(trans_dev, aes(x = amount)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Transaction Amounts (Outliers Removed)",
       x = "Transaction Amount",
       y = "Frequency")

summary(account)
ggplot(account, aes(x = frequency)) + geom_bar() + ggtitle("Distribution of Frequency")

summary(card_dev)
ggplot(card_dev, aes(x = type)) + geom_bar() + ggtitle("Distribution of Card Types")

summary(client)
ggplot(client, aes(x = district_id)) + geom_bar() + ggtitle("Distribution of Clients across Districts")

summary(disp)
ggplot(disp, aes(x = type)) + geom_bar() + ggtitle("Distribution of Disposition Types")

summary(district)
ggplot(district, aes(x = region)) + geom_bar() + ggtitle("Distribution of Regions")

summary(loan_dev)
ggplot(loan_dev, aes(x = duration)) + geom_bar() + ggtitle("Distribution of Loan Durations")

disp <- unique(disp)
trans_dev <- unique(trans_dev)

merged_data_step1 <- account %>%
  left_join(card_dev, by = c("account_id" = "disp_id"))

merged_data_step2 <- merged_data_step1 %>%
  left_join(disp, by = "account_id")

merged_data_step3 <- merged_data_step2 %>%
  left_join(client, by = "client_id")

colnames(district)[colnames(district) == "code"] <- "district_id_district"

merged_data_step4 <- merged_data_step3 %>%
  left_join(district, by = c("district_id.x" = "district_id_district"))

merged_data_step5 <- merged_data_step4 %>%
  left_join(loan_dev, by = "account_id")

merged_data <- merged_data_step5 %>%
  left_join(trans_dev, by = "account_id", suffix = c("_merged", "_trans"), relationship = "many-to-many")

cleaned_data <- merged_data %>%
  distinct(account_id, .keep_all = TRUE)

Q1 <- quantile(cleaned_data$amount_trans, 0.25, na.rm = TRUE)
Q3 <- quantile(cleaned_data$amount_trans, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
upper_limit <- Q3 + 1.5 * IQR
cleaned_data <- cleaned_data[cleaned_data$amount_trans <= upper_limit, ]

missing_data_plot <- ggplot(data = cleaned_data) +
  geom_bar(aes(x = "", y = (sum(is.na(cleaned_data)) / length(cleaned_data))), fill = "coral", stat = "identity") +
  labs(title = "Proportion of Missing Data",
       x = "",
       y = "Proportion") +
  theme_minimal()

print(missing_data_plot)

duplicated_cleaned_data <- cleaned_data$account_id[!is.na(cleaned_data$account_id) & duplicated(cleaned_data$account_id)]

if (length(duplicated_cleaned_data) > 0) {
  cat("Duplicate values found in account_id column of the cleaned data frame.\n")
  print(duplicated_cleaned_data)
  stop("Please handle the duplicate values in the account_id column.")
} else {
  cat("No duplicate values found in account_id column of the cleaned data frame.\n")
}

