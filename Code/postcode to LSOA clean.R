library(tidyverse)

# Load raw postcode-to-LSOA mapping
raw_lsoa <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/LSOA_data/Postcode to LSOA 1.csv")

# Select and rename columns
cleaned_lsoa <- raw_lsoa %>%
  select(
    pcds, lsoa11cd, lsoa11nm, ladnm, msoa11nm, ladcd
  ) %>%
  rename(
    Postcode       = pcds,
    LSOA_Code      = lsoa11cd,
    District       = lsoa11nm,
    Town           = ladnm,
    MSOA_Name      = msoa11nm,
    District_Code  = ladcd
  )

# Drop incomplete rows
cleaned_lsoa <- cleaned_lsoa %>%
  filter(!is.na(Postcode), !is.na(LSOA_Code), !is.na(Town))

# Define districts for Yorkshire counties
south_yorkshire <- c("Barnsley", "Doncaster", "Rotherham", "Sheffield")
west_yorkshire  <- c("Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield")

# Filter for those two counties only
cleaned_lsoa <- cleaned_lsoa %>%
  filter(Town %in% c(south_yorkshire, west_yorkshire)) %>%
  mutate(
    County = case_when(
      Town %in% south_yorkshire ~ "South Yorkshire",
      Town %in% west_yorkshire  ~ "West Yorkshire",
      TRUE ~ NA_character_
    ),
    Postcode = toupper(str_trim(Postcode))
  ) %>%
  distinct()

# Save cleaned version
write_csv(cleaned_lsoa, "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/Postcode_to_LSOA_clean.csv")

# Optional: View in RStudio Viewer
View(cleaned_lsoa)
