library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)

crime_data <- read_csv(
  "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/crime_summary.csv"
)

target_crime <- "Drugs"

filtered <- crime_data %>%
  filter(`Crime type` == target_crime)

south <- filtered %>% filter(County == "South Yorkshire")
west  <- filtered %>% filter(County == "West Yorkshire")

plot_south <- ggplot(south, aes(x = District, y = Count)) +
  geom_boxplot(fill = "#4A90E2", color = "#1B4F72", alpha = 0.9, outlier.shape = 18) +
  labs(
    title = "South Yorkshire — Drug Crime Distribution",
    x = NULL,
    y = "Reported Cases"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

plot_west <- ggplot(west, aes(x = District, y = Count)) +
  geom_boxplot(fill = "#58D68D", color = "#1D8348", alpha = 0.9, outlier.shape = 17) +
  labs(
    title = "West Yorkshire — Drug Crime Distribution",
    x = NULL,
    y = "Reported Cases"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

grid.arrange(plot_south, plot_west, ncol = 2)
