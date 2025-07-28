library(readr)
library(dplyr)
library(ggplot2)

# Load cleaned datasets
house_data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleaned_house_prices.csv")
broadband_data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleanBroadband.csv")

# Diagnostic: Print column names to check for 'County' or 'Region'
cat("House data columns:\n")
print(colnames(house_data))
cat("Broadband data columns:\n")
print(colnames(broadband_data))

# Standardize Postcodes and Region column
house_data <- house_data %>%
  rename(Region = any_of(c("County", "Region", "county", "region"))) %>%
  mutate(Postcode = toupper(str_replace_all(Postcode, " ", "")))

broadband_data <- broadband_data %>%
  rename(Region = any_of(c("County", "Region", "county", "region"))) %>%
  mutate(Postcode = toupper(str_replace_all(Postcode, " ", "")))

# Check if Region column exists
if (!"Region" %in% colnames(house_data) || !"Region" %in% colnames(broadband_data)) {
  stop("Region column not found in one or both datasets. Check column names above.")
}

# Aggregate by Postcode and Region
house_avg <- house_data %>%
  group_by(Postcode, Region) %>%
  summarise(Avg_House_Value = mean(Price, na.rm = TRUE), .groups = "drop")

broadband_avg <- broadband_data %>%
  group_by(Postcode, Region) %>%
  summarise(Avg_Download_Rate = mean(`Average download speed (Mbit/s)`, na.rm = TRUE), .groups = "drop")

# Merge datasets
combined_data <- inner_join(house_avg, broadband_avg, by = c("Postcode", "Region"))

# Filter for Yorkshire regions
yorkshire_data <- combined_data %>%
  filter(Region %in% c("South Yorkshire", "West Yorkshire"))

# Linear Model
lm_result <- lm(Avg_House_Value ~ Avg_Download_Rate, data = yorkshire_data)
summary(lm_result)

# Correlation
corr_value <- cor(yorkshire_data$Avg_House_Value, yorkshire_data$Avg_Download_Rate, use = "complete.obs")
cat("Correlation between Avg House Value and Download Rate:", round(corr_value, 3), "\n")

# Chart for House Price vs Download Speed