# Load libraries
library(readr)
library(dplyr)
library(lubridate)
library(fmsb)

# Load the cleaned crime data
crime_data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleaned_crime.csv")

# Define South Yorkshire districts (used instead of missing 'County' column)
sy_districts <- c("Barnsley", "Doncaster", "Rotherham", "Sheffield")

# Filter for vehicle crime in May 2025 for South Yorkshire districts
vehicle_crime_sy <- crime_data %>%
  filter(
    Local_Authority %in% sy_districts,
    `Crime type` == "Vehicle crime",
    Month == "2025-05"
  )

# Count vehicle crimes per district
district_summary <- vehicle_crime_sy %>%
  count(Local_Authority, name = "Count") %>%
  arrange(desc(Count))

# Prepare data for radar chart
max_val <- max(district_summary$Count) + 5
min_val <- 0

radar_data <- rbind(
  rep(max_val, nrow(district_summary)),
  rep(min_val, nrow(district_summary)),
  district_summary$Count
)

colnames(radar_data) <- district_summary$Local_Authority
rownames(radar_data) <- c("Max", "Min", "Actual")

# Plot the radar chart
radarchart(
  as.data.frame(radar_data),
  axistype = 1,
  pcol     = "#B22222",
  pfcol    = rgb(0.9, 0.2, 0.2, 0.4),
  plwd     = 2,
  cglcol   = "grey50",
  cglty    = 1,
  cglwd    = 0.8,
  axislabcol = "black",
  vlcex    = 0.9,
  title    = "Vehicle Crime by District — South Yorkshire (May 2025)"
)
