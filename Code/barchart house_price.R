library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

data <- read_csv("/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleaned_house_prices.csv", show_col_types = FALSE)

data <- data %>%
  mutate(
    ParsedDate = ymd(as.character(Years)),
    Yr = year(ParsedDate)
  )

recent <- data %>%
  filter(Yr == 2023)

avg_prices <- recent %>%
  group_by(County, District) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

ggplot(avg_prices, aes(x = fct_reorder(District, AvgPrice), y = AvgPrice, fill = County)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "2023 District-wise Avg House Prices",
    x = NULL,
    y = "Avg Price",
    fill = NULL
  ) +
  theme_light(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
