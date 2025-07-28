# Define West Yorkshire districts (Local Authorities)
wy_districts <- c("Bradford", "Leeds", "Kirklees", "Calderdale", "Wakefield")

# Filter robbery data for May 2025 in West Yorkshire
robbery_wy <- crimes %>%
  filter(
    Local_Authority %in% wy_districts,
    `Crime type` == "Robbery",
    Month == "2025-05"
  )

# Summarise by district
district_summary <- robbery_wy %>%
  count(Local_Authority, name = "Count") %>%
  mutate(
    Share = Count / sum(Count),
    Label = paste0(Local_Authority, "\n", round(Share * 100, 1), "%")
  )

# Pie chart
ggplot(district_summary, aes(x = "", y = Count, fill = Local_Authority)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = Label),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  labs(
    title = "Robbery Distribution by District – West Yorkshire (May 2025)",
    fill  = "District"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  ) +
  scale_fill_brewer(palette = "Set2")
