#------------------------------------------------------------------------------#
#  TITLE: Create figure of changes in key waste composition categories
#   DATE: 20190421
#   PROG: B Saul
#   DESC:
#------------------------------------------------------------------------------#

library(dplyr)
library(ggplot2)
library(ocWaste)

## Prepare data ####

demographics <- tribble(
    ~year, ~pop, ~waste, ~per_capita, ~waste_src, ~waste_src_note,
    1995, 105000, 126309, NA, "https://bit.ly/2Wpsi8K", "Appendix B-2",
    1996, 105000, 90397, 0.85, "https://bit.ly/2Wpsi8K", "Appendix B-2",
    2017, 143000, 72604, 0.52, "https://bit.ly/2CF53jL", "page 3"
)

prop_material_1995 <- tribble(
  ~year, ~material_category, ~type, ~value,
  1995, "Paper", "com", 0.48,
  1995, "Plastic", "com", 0.14,
  1995, "Organic", "com", 0.208,
  1995, "Ferrous Metal", "com", 0.048,
  1995, "Non-Ferrous Metal", "com", 0.007,
  1995, "Glass", "com", 0.044,
  1995, "Wood", "com", 0.028,
  1995, "Special Waste", "com", 0.022
)

prop_material_2017 <- oc_waste_2017 %>%
  group_by(pdf_source, type, jurisdiction, sample_id, day, material_category) %>%
  summarise(
    value = sum(value)
  )  %>%
  group_by(
    type, material_category
  ) %>%
  summarise(
    year  = 2017,
    value = mean(value)/100
  )


prop_material <- bind_rows(prop_material_1995, prop_material_2017)

## Create plotting data ####

plot_dt <- prop_material %>%
  filter(type == "com") %>%
  left_join(demographics, by = "year") %>%
  mutate(
    per_capita = if_else(is.na(per_capita), waste/pop, per_capita),
    p          = per_capita * value
  ) %>%
  filter(
    material_category %in% c("Organic", "Plastic", "Paper", "Glass")
  ) %>%
  group_by(material_category, type) %>%
  mutate(
    p_change = (p[year == 2017] - p[year == 1995])/p[year == 1995],
    label    = sprintf("%s (%s%%)", material_category, round(p_change * 100))
  )

## Generate plot ####

ggplot(
  data = plot_dt,
  aes(x = year, y = p, color = material_category, group = material_category)
) +
  geom_point() +
  geom_line() +
  geom_label(
    data = plot_dt %>% filter(year == 2017),
    aes(label = label),
    fill = NA,
    label.size = 0,
    hjust   = 0,
    vjust   = 0.1
    # nudge_x = 2
  ) +
  scale_x_continuous(
    "",
    minor_breaks = seq(2000, 2015, by = 5),
    breaks = c(1995, 2017),
    limits = c(1995, 2025)
  ) +
  scale_y_continuous(
    "Annual tons per person",
    limits = c(0, 0.6),
    expand = c(0, 0)
  ) +
  scale_color_brewer(
    palette = "Set1",
    guide = FALSE
  ) +
  labs(
    title   = "Compostables and Recyclables\nin Orange County Waste",
    caption = "Data from the 2017 Orange County Waste Composition Study, Census estimates, and NCDEQ annual reports"
  ) +
  theme(
    plot.caption = element_text(size = 5),
    axis.title.x = element_blank()
  ) ->
  p

## Save plot ####

ggsave(
  "inst/land_solid_waste_fig2.pdf",
  plot = p, width = 5, height = 4,
  dpi = 300
)

