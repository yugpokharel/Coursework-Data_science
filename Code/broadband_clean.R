library(tidyverse)

# Read broadband datasets
perf_data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/broadband/fixed_postcode_performance.csv")
cov_data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/broadband/fixed_postcode_coverage.csv")

# Standardize postcode column
perf_data <- perf_data %>% rename(Postcode = postcode)
cov_data <- cov_data %>% rename(Postcode = postcode)

# Merge datasets
broadband_data <- left_join(perf_data, cov_data, by = "Postcode")

# Filter valid postcodes
broadband_data <- broadband_data %>%
  filter(!is.na(Postcode), Postcode != "")

# Assign counties based on postcode
broadband_data <- broadband_data %>%
  mutate(
    Region = case_when(
      str_starts(Postcode, "S")  ~ "South Yorkshire",
      str_starts(Postcode, "DN") ~ "South Yorkshire",
      str_starts(Postcode, "LS") ~ "West Yorkshire",
      str_starts(Postcode, "WF") ~ "West Yorkshire",
      str_starts(Postcode, "HD") ~ "West Yorkshire",
      str_starts(Postcode, "BD") ~ "West Yorkshire",
      str_starts(Postcode, "HX") ~ "West Yorkshire"
    )
  ) %>%
  filter(!is.na(Region))

# Calculate low-speed connections
broadband_data <- broadband_data %>%
  mutate(
    Low_Speed_Connections = 
      `Number of connections < 2 Mbit/s (number of lines)` +
      `Number of connections 2<5 Mbit/s (number of lines)` +
      `Number of connections 5<10 Mbit/s (number of lines)`
  )

# Select relevant columns
broadband_select <- broadband_data %>%
  select(
    Postcode, Region,
    `Average download speed (Mbit/s)`,
    `Average upload speed (Mbit/s)`,
    `Median download speed (Mbit/s)`,
    `Median upload speed (Mbit/s)`,
    Low_Speed_Connections,
    `% of premises unable to receive 2Mbit/s`,
    `% of premises unable to receive 5Mbit/s`,
    `% of premises unable to receive 10Mbit/s`,
    `% of premises unable to receive 30Mbit/s`,
    `FTTP availability (% premises)`
  )

# Remove rows with all NA values (except Postcode and Region)
broadband_select <- broadband_select %>%
  filter(rowSums(is.na(select(., -Postcode, -Region))) < ncol(select(., -Postcode, -Region)))

# Remove entirely NA columns
broadband_final <- broadband_select %>%
  select(where(~ !all(is.na(.))))

# Save cleaned dataset
write_csv(broadband_final, "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleanBroadband.csv")

# Summary of missing data
colSums(is.na(broadband_data))
colSums(broadband_data == "", na.rm = TRUE)