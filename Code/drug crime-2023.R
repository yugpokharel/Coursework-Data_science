library(readr)
library(dplyr)

# Set folder path
data_path <- "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/crimerate/"

# Load all CSVs recursively
csv_files <- list.files(path = data_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Read and extract relevant columns
crime_list <- lapply(csv_files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  if (all(c("Reported by", "Crime type") %in% colnames(df))) {
    df %>% select(`Reported by`, `Crime type`)
  } else {
    NULL
  }
})

# Combine non-null data
crime_data <- bind_rows(crime_list[!sapply(crime_list, is.null)])

# Filter drug crimes and standardize names
drug_data <- crime_data %>%
  filter(!is.na(`Reported by`), !is.na(`Crime type`)) %>%
  rename(region = `Reported by`, crime_type = `Crime type`) %>%
  mutate(
    region = trimws(tolower(region)),
    crime_type = trimws(tolower(crime_type))
  ) %>%
  filter(crime_type == "drugs")

# Summarize drug crime counts by region
drug_crime_summary <- drug_data %>%
  group_by(region) %>%
  summarise(drug_crimes_2023 = n()) %>%
  filter(region %in% c("south yorkshire", "west yorkshire"))

# Save summary data
write_csv(drug_crime_summary, "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/drug_rates_2023.csv")

# Display summary
print(drug_crime_summary)

# Process drug summary data
drug_summary <- data.frame(
  region = c("south yorkshire police", "west yorkshire police"),
  drug_crime_count = c(4689, 9065)
) %>%
  mutate(region = gsub(" police", "", tolower(region)))

# Add population data
population_data <- data.frame(
  region = c("south yorkshire", "west yorkshire"),
  population = c(1416000, 2334000)
)

# Join and calculate drug crime rate
drug_summary <- drug_summary %>%
  left_join(population_data, by = "region") %>%
  mutate(rate_per_10000 = round((drug_crime_count / population) * 10000, 2))

# Save final data
write_csv(drug_summary, "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/drug_rates_2023.csv")

# Display final table
print(drug_summary)

