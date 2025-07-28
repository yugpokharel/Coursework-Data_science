library(tidyverse)

edu_data <- read_csv(
  "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/school_clean_data.csv",
  col_types = cols(
    urn     = col_double(),
    lea     = col_double(),
    laname  = col_character(),
    att8scr = col_double()
  )
)

wy_lea_codes <- c(380, 381, 382, 383, 384)

wy_schools <- edu_data %>%
  filter(lea %in% wy_lea_codes, !is.na(att8scr)) %>%
  mutate(
    District = case_when(
      lea == 380 ~ "Bradford",
      lea == 381 ~ "Calderdale",
      lea == 382 ~ "Kirklees",
      lea == 383 ~ "Leeds",
      lea == 384 ~ "Wakefield",
      TRUE       ~ NA_character_
    )
  ) %>%
  filter(!is.na(District))

ggplot(wy_schools, aes(x = fct_reorder(District, att8scr, median), y = att8scr, fill = District)) +
  geom_boxplot(alpha = 0.85, width = 0.65, outlier.shape = 18, outlier.color = "firebrick") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.2, fill = "white") +
  labs(
    title    = "Attainment 8 Scores — West Yorkshire Districts (2021/22)",
    subtitle = "Boxplots with Mean Indicators for Each Local Authority",
    x        = NULL,
    y        = "Attainment 8 Score",
    caption  = "Source: Department for Education • White diamond = district mean"
  ) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_light(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5),
    axis.text       = element_text(size = 11)
  )
