library(tidyverse)
library(stringr)
library(patchwork)

bb_data <- read_csv(
  "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleanBroadband.csv",
  show_col_types = FALSE
) %>%
  mutate(
    PC       = toupper(str_trim(Postcode)),
    PC_fixed = str_replace(PC, "(.+)(.{3})$", "\\1 \\2"),
    Speed_Mbps = `Average download speed (Mbit/s)`
  )

pc_meta <- read_csv(
  "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/Postcode_to_LSOA_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(Postcode = toupper(str_trim(Postcode))) %>%
  select(Postcode, Town, District, County)

final_data <- bb_data %>%
  inner_join(pc_meta, by = c("PC_fixed" = "Postcode")) %>%
  transmute(
    PC    = PC_fixed,
    Cnty  = County.y,
    Dist  = District,
    Town  = Town,
    Speed = Speed_Mbps
  ) %>%
  filter(!is.na(Speed), is.finite(Speed)) %>%
  group_by(PC, Cnty, Dist, Town) %>%
  summarise(Speed = mean(Speed), .groups = "drop")

town_avgs <- final_data %>%
  group_by(Cnty, Town) %>%
  summarise(Avg = mean(Speed), .groups = "drop")

south_y <- town_avgs %>% filter(Cnty == "South Yorkshire")
west_y  <- town_avgs %>% filter(Cnty == "West Yorkshire")

plot_sy <- ggplot(south_y, aes(x = fct_reorder(Town, Avg), y = Avg, fill = Avg)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "South Yorkshire: Download Speed by Town",
    x = NULL,
    y = "Mbps"
  ) +
  scale_fill_viridis_c() +
  theme_light(base_size = 12)

plot_wy <- ggplot(west_y, aes(x = fct_reorder(Town, Avg), y = Avg, fill = Avg)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "West Yorkshire: Download Speed by Town",
    x = NULL,
    y = "Mbps"
  ) +
  scale_fill_viridis_c() +
  theme_light(base_size = 12)

(plot_sy | plot_wy) +
  plot_annotation(
    title   = "Town-Level Average Broadband Speeds",
    caption = "Speeds averaged from postcode data"
  )

