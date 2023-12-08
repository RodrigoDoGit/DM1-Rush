```r
library(dplyr)

account <- read.csv("account.csv", sep = ";")
card_dev <- read.csv("card_dev.csv", sep = ";")
client <- read.csv("client.csv", sep = ";")
disp <- read.csv("disp.csv", sep = ";")
district <- read.csv("district.csv", sep = ";")
loan_dev <- read.csv("loan_dev.csv", sep = ";")
trans_dev <- read.csv("trans_dev.csv", sep = ";")

missing_values <- sapply(list(account, card_dev, client, disp, district, loan_dev, trans_dev), function(df) sum(is.na(df)))

cat("Missing Values Summary:\n")
print(missing_values)

account$date <- as.Date(account$date, format = "%y%m%d")
loan_dev$date <- as.Date(loan_dev$date, format = "%y%m%d")
trans_dev$date <- as.Date(trans_dev$date, format = "%y%m%d")

summary(account)
summary(card_dev)
summary(client)
summary(disp)
summary(district)
summary(loan_dev)
summary(trans_dev)

disp <- unique(disp)
trans_dev <- unique(trans_dev)

merged_data_step1 <- account %>%
  left_join(card_dev, by = c("account_id" = "disp_id"))

str(merged_data_step1)

merged_data_step2 <- merged_data_step1 %>%
  left_join(disp, by = "account_id")

str(merged_data_step2)


merged_data_step3 <- merged_data_step2 %>%
  left_join(client, by = "client_id")

str(merged_data_step3)
colnames(district)[colnames(district) == "code"] <- "district_id_district"

merged_data_step4 <- merged_data_step3 %>%
  left_join(district, by = c("district_id.x" = "district_id_district"))

str(merged_data_step4)

merged_data_step5 <- merged_data_step4 %>%
  left_join(loan_dev, by = "account_id")

str(merged_data_step5)

merged_data <- merged_data_step5 %>%
  left_join(trans_dev, by = "account_id", suffix = c("_merged", "_trans"), relationship = "many-to-many")



str(merged_data)


cleaned_data <- merged_data %>%
  distinct(account_id, .keep_all = TRUE)

duplicated_cleaned_data <- cleaned_data$account_id[duplicated(cleaned_data$account_id)]
if (length(duplicated_cleaned_data) > 0) {
  cat("Duplicate values found in account_id column of the cleaned data frame.\n")
  print(duplicated_cleaned_data)
  stop("Please handle the duplicate values in the account_id column.")
} else {
  cat("No duplicate values found in account_id column of the cleaned data frame.\n")
}

```