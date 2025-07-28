library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load cleaned house price data
house_data <- read_csv(
  "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleaned_house_prices.csv",
  show_col_types = FALSE
)

# Check the structure of your data
glimpse(house_data)

# Extract year from the Transaction_Year column (which is already a Date type)
house_data <- house_data %>%
  mutate(Year = year(Transaction_Year))

# Check available years
unique(house_data$Year)

# Filter for available years (adjust based on what you see above)
filtered_data <- house_data %>%
  filter(Year %in% 2022:2024)  # Changed from 2021:2024 since your data starts at 2022

# Check if we have data
nrow(filtered_data)

# Check for missing County/District values
sum(is.na(filtered_data$County))
sum(is.na(filtered_data$District))

# Compute average price by district per year
district_trends <- filtered_data %>%
  group_by(Year, County, District) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE), .groups = "drop")

# Check the summary data
head(district_trends)

# Line plot of trends
ggplot(district_trends, aes(x = Year, y = Average_Price, color = District, group = District)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ County, scales = "free_y") +  # Free y-scale for better comparison
  labs(
    title = "House Price Trends by District (2022-2024)",
    subtitle = "Average prices grouped by district and county",
    x = "Year",
    y = "Avg House Price (£)",
    color = "District"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")  # Bold facet titles
  ) +
  scale_y_continuous(labels = scales::comma) +  # Better number formatting
  guides(color = guide_legend(nrow = 3))  # Better legend layout for many districts