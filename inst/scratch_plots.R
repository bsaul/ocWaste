library(ggplot2)
library(ggbeeswarm)

x <- nc_waste_rates %>%
  select(county, msw_cd_1617, pop_072017, is_orange) %>%
  mutate(
    lbs_msw_cd_1617 = msw_cd_1617 * 2000
  )
x <- x %>%
  left_join(
    ncrecycling, by = 'county'
  ) %>%
  mutate(
    total_recycling_lbs = total_recycling_per_person * pop_072017,
    total_output_lbs    = lbs_msw_cd_1617 + total_recycling_lbs
  ) %>%
  select(
    county, lbs_msw_cd_1617, total_recycling_lbs, total_output_lbs, pop_072017, is_orange
  )

plotdt <- x %>%
  tidyr::gather(key = "key", value = "value", -county, -pop_072017, -is_orange) %>%
  mutate(per_person = value/pop_072017) %>%
  tidyr::gather(key = "measure", value = "value", -county, -pop_072017, -is_orange, -key) %>%
  mutate(
    measure = if_else(measure == "per_person", "Per Person", "Total Pounds"),
    key     = case_when(
      key == "total_recycling_lbs" ~ "Recycling",
      key == "lbs_msw_cd_1617"     ~ "MSW + CD",
      key == "total_output_lbs"    ~ "MSW + CD + Recycling",
    ),
    key     = factor(key, levels = c("Recycling", "MSW + CD", "MSW + CD + Recycling"), ordered = TRUE)
  ) %>%
  filter(key != "MSW + CD + Recycling")

ggplot(
  data = plotdt,
  aes(x = key,y = log10(value), color = is_orange)
) +
  geom_beeswarm(shape = 1, size = 0.5) +
  scale_color_manual(
    guide  = FALSE,
    values = c("black", "orange")
  ) +
  coord_flip() +
  facet_grid(~ measure, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
  )

