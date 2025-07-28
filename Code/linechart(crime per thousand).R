library(tidyverse)
library(stringr)

# Load datasets
pop_data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/population/Population.csv")
lsoa_data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/Postcode_to_LSOA_clean.csv") %>%
  select(Postcode, LSOA_Code, District)
crime_data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleaned_crime.csv")

# Diagnostic: Print column names to check for 'District'
cat("Crime data columns:\n")
print(colnames(crime_data))
cat("LSOA data columns:\n")
print(colnames(lsoa_data))

# Standardize District column name in crime_data
crime_data <- crime_data %>%
  rename(District = any_of(c("District", "district", "Local_Authority", "local_authority")))

# Check if District column exists
if (!"District" %in% colnames(crime_data)) {
  stop("District column not found in crime_data. Check column names above.")
}
if (!"District" %in% colnames(lsoa_data)) {
  stop("District column not found in lsoa_data. Check column names above.")
}

# Clean and standardize postcodes
pop_data <- pop_data %>%
  mutate(Postcode = str_to_upper(str_replace_all(Postcode, "\\s+", "")))

lsoa_data <- lsoa_data %>%
  mutate(Postcode = str_to_upper(str_replace_all(Postcode, "\\s+", "")))

# Extract outward code
lsoa_data <- lsoa_data %>%
  mutate(Outward_Code = str_extract(Postcode, "^[A-Z]{1,2}[0-9][A-Z0-9]?"))

# Rename Postcode to Outward_Code in pop_data
pop_data <- pop_data %>%
  rename(Outward_Code = Postcode)

# Collapse lsoa_data to one row per Outward_Code
lsoa_summary <- lsoa_data %>%
  group_by(Outward_Code) %>%
  summarise(District = first(District))  # Use first District for simplicity

# Join population with lsoa_summary
pop_district <- pop_data %>%
  left_join(lsoa_summary, by = "Outward_Code") %>%
  filter(!is.na(District))

# Summarize population by District
pop_summary <- pop_district %>%
  group_by(District) %>%
  summarise(Total_Population = sum(Population, na.rm = TRUE)) %>%
  arrange(desc(Total_Population))

# Display top 10 districts by population
print(head(pop_summary, 10))

# Clean and summarize crime data
crime_clean <- crime_data %>%
  filter(!is.na(District))

crime_agg <- crime_clean %>%
  group_by(District) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(desc(Total_Crimes))

# Display top 10 districts by crime count
print(head(crime_agg, 10))

# Calculate crime rate per 1,000 people
crime_rate <- crime_agg %>%
  left_join(pop_summary, by = "District") %>%
  mutate(Crimes_Per_1000 = (Total_Crimes / Total_Population) * 1000)

# Display top 10 districts by crime rate
print(crime_rate %>%
        arrange(desc(Crimes_Per_1000)) %>%
        select(District, Total_Crimes, Total_Population, Crimes_Per_1000) %>%
        head(10))
