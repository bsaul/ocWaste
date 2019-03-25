#------------------------------------------------------------------------------#
#
#
#
#
#------------------------------------------------------------------------------#

library(dplyr)
library(ggplot2)
library(ocWaste)
library(cowplot)

## Prepare data ####

county_waste <- nc_msw_cd_1617_report %>%
  select(county, msw_cd_1617, pop_07_2016, is_orange) %>%
  mutate(
    lbs_msw_cd_1617 = msw_cd_1617 * 2000
  ) %>%
  left_join(
    nc_recycling_1617_report, by = 'county'
  ) %>%
  mutate(
    total_recycling_lbs = total_recycling_per_person * pop_07_2016,
    total_output_lbs    = lbs_msw_cd_1617 + total_recycling_lbs,
    prop_recycling      = total_recycling_lbs/total_output_lbs
  ) %>%
  select(
    county, lbs_msw_cd_1617, total_recycling_lbs, total_output_lbs,
    pop_07_2016, prop_recycling, is_orange
  )

plotdt <- county_waste %>%
  tidyr::gather(key = "key", value = "value", -county, -pop_07_2016, -is_orange) %>%
  mutate(per_person = value/pop_07_2016) %>%
  tidyr::gather(key = "measure", value = "value", -county, -pop_07_2016, -is_orange, -key) %>%
  mutate(
    measure = if_else(measure == "per_person", "Per Person", "Total Pounds"),
    key     = factor(key, levels = c("total_recycling_lbs", "lbs_msw_cd_1617", "total_output_lbs", "prop_recycling"),
                     labels = c("Recycling", "Municipal Solid Waste +\\nConstruction & Demolition", "MSW + CD + Recycling", "Proportion Recycling"),
                     ordered = TRUE)
  ) %>%
  filter(!(key %in% c("MSW + CD + Recycling", "Proportion Recycling"))) %>%
  group_by(
    key, measure
  ) %>%
  mutate(
    rank = case_when(
      key == "Recycling" ~ rank(value * -1, ties.method = "random"),
      TRUE ~ rank(value, ties.method = "random"))
  )


## Create plot ###

plot_it <- function(dt, colors){

  orange_rank <- dt$rank[dt$is_orange]

  ggplot(
    data = dt,
    aes(x = rank, y = value, fill = is_orange)
  ) +
    scale_x_continuous(
      breaks = orange_rank,
      # limits = c(0, nrow(dt)),
      expand = c(0, 0),
      trans  = "reverse"
    ) +
    scale_y_continuous(
      expand = c(0, 0)
    ) +
    geom_col(width = 0.5) +
    coord_flip() +
    scale_fill_manual(
      guide  = FALSE,
      values = colors
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}

hold <- plotdt %>%
  group_by(measure, key) %>%
  tidyr::nest() %>%
  mutate(
    p = purrr::map(data, ~ plot_it(.x, c("#4daf4a", "#ff7f00")))
  )

"#984ea3"

hold

library(cowplot)
plot_grid(
  hold$p[[3]],
  hold$p[[4]],
  hold$p[[1]],
  hold$p[[2]]
)

