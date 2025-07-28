library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load cleaned school data
school_data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleaned_school_data.csv")

# Diagnostic: Print column names and sample data
cat("School data columns:\n")
print(colnames(school_data))
cat("First few rows of school_data:\n")
print(head(school_data))

# Define LEA to district mapping
lea_mapping <- tibble(
  lea = c(370, 371, 372, 373, 380, 381, 382, 383, 384),
  district = c("Barnsley", "Doncaster", "Rotherham", "Sheffield", 
               "Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield")
)

# Standardize LEA and year columns
school_data <- school_data %>%
  mutate(
    lea = as.numeric(lea),
    year = if_else("academic_year" %in% colnames(.), as.numeric(academic_year), 
                   if_else("Year" %in% colnames(.), as.numeric(Year), 
                           if_else("year" %in% colnames(.), as.numeric(year), NA_real_)))
  )

# Check if year column was created successfully
if (all(is.na(school_data$year))) {
  stop("Year column could not be created. Check if 'academic_year', 'Year', or 'year' exists in school_data.")
}

# Check for missing LEA codes
missing_leas <- setdiff(unique(school_data$lea), lea_mapping$lea)
if (length(missing_leas) > 0) {
  warning("LEA codes not found in mapping: ", paste(missing_leas, collapse = ", "))
} else {
  message("All LEA codes in school_data found in mapping.")
}

# Prepare data for plotting
chart_data <- school_data %>%
  filter(lea %in% lea_mapping$lea) %>%
  left_join(lea_mapping, by = "lea") %>%
  group_by(year, district) %>%
  summarise(avg_attainment = mean(att8scr, na.rm = TRUE), .groups = "drop")

# Save summarized data
write_csv(chart_data, "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/attainment_by_district.csv")

