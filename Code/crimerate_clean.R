library(tidyverse)
library(lubridate)

# Load crime datasets
sy_crime <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/crimerate/2025-05-south-yorkshire-street.csv")
wy_crime <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/crimerate/2025-05-west-yorkshire-street.csv")

# Inspect data structure
glimpse(sy_crime)
glimpse(wy_crime)

# Add Region column
sy_crime <- sy_crime %>% mutate(Region = "South Yorkshire")
wy_crime <- wy_crime %>% mutate(Region = "West Yorkshire")

# Extract Local Authority from LSOA name
sy_crime <- sy_crime %>%
  mutate(Local_Authority = str_extract(`LSOA name`, "^[A-Za-z\\s\\-]+") %>% str_trim())

wy_crime <- wy_crime %>%
  mutate(Local_Authority = str_extract(`LSOA name`, "^[A-Za-z\\s\\-]+") %>% str_trim())

# Combine datasets
crime_data <- bind_rows(sy_crime, wy_crime)

# Filter out missing values
crime_data_clean <- crime_data %>%
  filter(!is.na(`Crime type`), !is.na(Location))

# Extract unique local authorities
authority_list <- crime_data_clean %>%
  distinct(Region, Local_Authority) %>%
  arrange(Region, Local_Authority)

print(authority_list)

# Summarize crime data by region and authority
crime_stats <- crime_data_clean %>%
  group_by(Region, Local_Authority, `Crime type`) %>%
  summarise(Total = n(), .groups = "drop") %>%
  arrange(Region, Local_Authority, desc(Total))

print(crime_stats)

# Save cleaned and summarized data
output_dir <- "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data"
write_csv(crime_data_clean, file.path(output_dir, "cleaned_crime.csv"))
write_csv(crime_stats, file.path(output_dir, "crime_summary.csv"))
write_csv(authority_list, file.path(output_dir, "unique_districts.csv"))

# Chart for Crime Counts by Region