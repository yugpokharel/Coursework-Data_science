library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Define file paths
data_files <- list(
  hp_2021 = "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/House_Prices/houseprice2021.csv",
  hp_2022 = "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/House_Prices/houseprice2022.csv",
  hp_2023 = "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/House_Prices/houseprice2023.csv",
  hp_2024 = "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/House_Prices/houseprice2024.csv"
)

# Set column names
col_names <- c("Transaction_ID", "Price", "Sale_Date", "Postcode", "Property_Type", 
               "New_Build", "Tenure", "Primary_Address", "Secondary_Address", 
               "Street", "Locality", "City", "District", "County", "Extra_A", "Extra_B")

# Function to process and clean house price data
process_data <- function(file_path, year_ref) {
  df <- read_csv(file_path, col_names = FALSE, show_col_types = FALSE)
  colnames(df) <- col_names
  
  df <- df %>%
    mutate(
      Postcode = str_to_upper(str_trim(Postcode)),
      County = str_to_title(str_trim(County)),
      Transaction_Year = case_when(
        year_ref == 2021 ~ as.character(mdy_hms(Sale_Date)),
        TRUE ~ as.character(parse_date_time(Sale_Date, orders = c("ymd HMS", "mdy HMS", "ymd", "mdy")))
      )
    ) %>%
    filter(!(is.na(Price) & is.na(Transaction_Year) & is.na(Postcode) & is.na(County))) %>%
    select(-Extra_A, -Extra_B, -Sale_Date)
  
  return(df)
}

# Process each year's data
hp_data_2021 <- process_data(data_files$hp_2021, 2021)
hp_data_2022 <- process_data(data_files$hp_2022, 2022)
hp_data_2023 <- process_data(data_files$hp_2023, 2023)
hp_data_2024 <- process_data(data_files$hp_2024, 2024)

# Combine datasets
all_house_data <- bind_rows(hp_data_2021, hp_data_2022, hp_data_2023, hp_data_2024)

# Filter for specific regions and sort
all_house_data <- all_house_data %>%
  filter(County %in% c("West Yorkshire", "South Yorkshire")) %>%
  arrange(desc(Price))

# Save cleaned data
output_dir <- "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data"
if (!dir.exists(output_dir)) dir.create(output_dir)

write_csv(all_house_data, file.path(output_dir, "cleaned_house_prices.csv"))

# Generate year summary
year_counts <- all_house_data %>%
  mutate(Year = year(ymd_hms(Transaction_Year))) %>%
  count(Year)

print(year_counts)

# Chart for House Price Distribution by Region