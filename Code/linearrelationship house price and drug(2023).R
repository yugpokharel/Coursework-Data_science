library(readr)
library(dplyr)
library(ggplot2)

# Load cleaned datasets
drug_rates <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/drug_rates_2023.csv")
house_prices <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleaned_house_prices.csv")

# Diagnostic: Print column names to check for 'County' or 'county'
cat("House prices columns:\n")
print(colnames(house_prices))
cat("Drug rates columns:\n")
print(colnames(drug_rates))

# Standardize region column names
house_prices <- house_prices %>%
  rename(region = any_of(c("County", "county", "Region", "region"))) %>%
  mutate(region = tolower(trimws(region)))

drug_rates <- drug_rates %>%
  rename(region = any_of(c("County", "county", "Region", "region"))) %>%
  mutate(region = tolower(trimws(region)))

# Check if region column exists
if (!"region" %in% colnames(house_prices)) {
  stop("Region column not found in house_prices. Check column names above.")
}
if (!"region" %in% colnames(drug_rates)) {
  stop("Region column not found in drug_rates. Check column names above.")
}

# Check for missing regions
missing_regions <- setdiff(unique(drug_rates$region), unique(house_prices$region))
if (length(missing_regions) > 0) {
  warning("Regions missing in house_prices: ", paste(missing_regions, collapse = ", "))
} else {
  message("All regions in drug_rates found in house_prices.")
}

# Aggregate house prices by region
house_agg <- house_prices %>%
  group_by(region) %>%
  summarise(
    avg_house_price = mean(Price, na.rm = TRUE),
    median_house_price = median(Price, na.rm = TRUE),
    transaction_count = n(),
    .groups = "drop"
  )

# Merge with drug rates
merged_data <- inner_join(drug_rates, house_agg, by = "region")

# Display merged data
print(merged_data)

# Save merged data
write_csv(merged_data, "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/drug_rates_2023.csv")

# Chart: Scatterplot of House Prices vs Drug Crime Rate