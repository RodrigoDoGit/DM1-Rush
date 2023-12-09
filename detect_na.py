import pandas as pd

# Load the CSV file into a pandas DataFrame
df = pd.read_csv('datasets/account.csv')

# Detect NA values
missing_values = df.isna()

# Count the number of missing values in each column
missing_count = missing_values.sum()

# Display the columns with missing values and their counts
print("Columns with missing values:")
print(missing_count[missing_count > 0])
