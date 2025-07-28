library(tidyverse)

school_data <- read_csv(
  "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/school_clean_data.csv",
  col_types = cols(
    urn = col_double(),
    lea = col_double(),
    laname = col_character(),
    att8scr = col_double()
  )
) %>% 
  select(urn, lea, laname, att8scr)

south_yorks_codes <- c(370, 371, 372, 373)
school_data_filtered <- school_data %>%
  filter(lea %in% south_yorks_codes, !is.na(att8scr)) %>%
  mutate(
    authority = case_when(
      lea == 370 ~ "Barnsley",
      lea == 371 ~ "Doncaster",
      lea == 372 ~ "Rotherham",
      lea == 373 ~ "Sheffield"
    )
  ) %>%
  filter(!is.na(authority))

if(nrow(school_data_filtered) > 0) {
  ggplot(school_data_filtered, aes(x = fct_reorder(authority, att8scr, .fun = median), 
                                   y = att8scr, 
                                   color = authority)) +
    geom_violin(trim = FALSE, alpha = 0.7, scale = "width") +
    geom_point(position = position_jitter(width = 0.15), size = 1, alpha = 0.4) +
    stat_summary(fun = mean, geom = "point", shape = 22, size = 4, fill = "black", color = "black") +
    labs(title = "South Yorkshire Secondary School Attainment 8 Scores (2021/22)",
         subtitle = paste("Performance across", nrow(school_data_filtered), "schools"),
         x = "Local Authority",
         y = "Attainment 8 Score",
         caption = "Source: Department for Education\nBlack squares indicate mean scores") +
    scale_color_brewer(palette = "Dark2") +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major.y = element_line(color = "gray90")
    ) +
    coord_flip()
}

school_data_filtered %>%
  group_by(authority) %>%
  summarise(
    School_Count = n(),
    Mean_Score = mean(att8scr, na.rm = TRUE),
    Median_Score = median(att8scr, na.rm = TRUE)
  ) %>%
  print()