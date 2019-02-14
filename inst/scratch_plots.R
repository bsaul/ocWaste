library(ggplot2)
library(ggbeeswarm)
library(ggiraph)
library(dplyr)
library(ocwaste)

x <- nc_msw_cd_1617_report %>%
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

plotdt <- x %>%
  tidyr::gather(key = "key", value = "value", -county, -pop_07_2016, -is_orange) %>%
  mutate(per_person = value/pop_07_2016) %>%
  tidyr::gather(key = "measure", value = "value", -county, -pop_07_2016, -is_orange, -key) %>%
  mutate(
    measure = if_else(measure == "per_person", "Per Person", "Total Pounds"),
    # key     = case_when(
    #   key == "total_recycling_lbs" ~ "Recycling",
    #   key == "lbs_msw_cd_1617"     ~ "MSW + CD",
    #   key == "total_output_lbs"    ~ "MSW + CD + Recycling",
    #   key == "prop_recycling"      ~ "Proportion Recycling"
    # ),
    key     = factor(key, levels = c("total_recycling_lbs", "lbs_msw_cd_1617", "total_output_lbs", "prop_recycling"),
                     labels = c("Recycling", "Municipal Solid Waste +\\nConstruction & Demolition", "MSW + CD + Recycling", "Proportion Recycling"),
                     ordered = TRUE)
  ) %>%
  filter(!(key %in% c("MSW + CD + Recycling", "Proportion Recycling")))

plotdt <- plotdt %>%
  mutate(
    value = value/2000,
     measure = case_when(
       measure == "Per Person" ~ "Tons/Person",
       measure == "Total Pounds" ~ "Total Tons"
         ),
    tooltip = paste0(county, "\n", round(value, 2))
    )

geom_quasirandom_interactive <- function(
  mapping = NULL,
  data = NULL,
  width = NULL,
  varwidth = FALSE,
  bandwidth=.5,
  nbins=NULL,
  method='quasirandom',
  groupOnX=NULL,
  dodge.width=0,
  stat='identity',
  position = "quasirandom",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  position <- position_quasirandom(width = width, varwidth = varwidth, bandwidth=bandwidth,nbins=nbins,method=method,groupOnX=groupOnX,dodge.width=dodge.width)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggiraph::GeomInteractivePoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

g <- ggplot(
  data = plotdt,
  aes(x =value, y = key, color = is_orange, size = is_orange, shape = is_orange,
      data_id = county, tooltip = tooltip)
) +
  geom_quasirandom_interactive(groupOnX = FALSE) +
  scale_x_log10() +
  scale_color_manual(
    guide  = FALSE,
    values = c("black", "orange")
  ) +
  scale_size_manual(
    guide = FALSE,
    values = c(0.75, 1.5)
  ) +
  scale_shape_manual(
    guide = FALSE,
    values = c(1, 16)
  ) +
  facet_grid(~ measure, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

z <- girafe( code = print(g), width_svg = 8, height_svg = 4)
girafe_options(z, opts_hover(css = "fill:red;r:100pt;") )
z


oc_waste_2017 %>%

