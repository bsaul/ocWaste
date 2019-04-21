#------------------------------------------------------------------------------#
#  TITLE: Create figure waste and recycling rates in NC counties
#   DATE: 20190421
#   PROG: B Saul
#   DESC:
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
      TRUE ~ rank(value, ties.method = "random")),
    feature = case_when(
      is_orange  ~ TRUE,
      county %in% c("Durham", "Wake", "Chatham", "Alamance", "Person") ~ TRUE,
      rank   %in% c(1, max(rank)) ~ TRUE,
      TRUE ~ FALSE
    ),
    value = value/2000,
    value = if_else(
      measure == "Total Pounds",
      value/1000,
      value
    ),
    label = sprintf("%s (%s)", county, round(value, ifelse(value > 1000, 0, 2)))
  )

## Fill in data in text ####

oc <- filter(plotdt, is_orange)

cat(
  "In that year, Orange County reported",
  oc %>% filter(key == "Municipal Solid Waste +\\nConstruction & Demolition", measure == "Per Person") %>%
    pull(value) %>% round(2),
  "tons of waste per person",
  "and",
  oc %>% filter(key == "Recycling", measure == "Per Person") %>%
    pull(value) %>% round(2),
  "tons of recycling per person.",
  "Recycling accounted for",
  county_waste %>% filter(county == "Orange") %>%
    pull(prop_recycling) %>% round(2) %>% {. * 100} %>%
    paste0("%"),
  "of all reported waste and recycling in Orange County.",
  "Out of all NC counties, Orange ranked",
  oc %>% filter(key == "Recycling", measure == "Per Person") %>%
    pull(rank) %>% paste0("th"),
  "highest in per person recycling rates and",
  oc %>% filter(key == "Municipal Solid Waste +\\nConstruction & Demolition",
                measure == "Per Person") %>%
    pull(rank) %>% paste0("th"),
  "lowest in waste rates."
)


## Create plot ####

plot_it <- function(dt, colors, title){

  dt <- filter(dt, !is.na(value))
  orange_rank <- dt$rank[dt$is_orange]
  breaks      <- dt$rank[dt$feature]

  ggplot(
    data = dt,
    aes(x = rank, y = value, fill = is_orange)
  ) +
    geom_vline(
      xintercept = nrow(dt) + 1, color = "grey50"
    ) +
    scale_x_continuous(
      breaks = breaks,
      limits = c(nrow(dt) + 2, -4),
      expand = c(0, 0),
      trans  = "reverse"
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, max(dt$value) * 1.4)
    ) +
    geom_col(width = 0.5) +
    geom_label(
      data = dt %>% filter(feature),
      aes(label = label, fill = NA),
      label.size =  0,
      size   = 2.5,
      hjust  = 0,
      vjust  = 0.2
    ) +
    coord_flip() +
    scale_fill_manual(
      guide  = FALSE,
      values = colors
    ) +
    ggtitle(title) +
    theme_minimal() +
    theme(
      title               = element_text(size = 8),
      axis.text.x         = element_text(size = 6),
      axis.text.y         = element_text(size = 6),
      axis.title.x        = element_blank(),
      axis.title.y        = element_blank(),
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.x  = element_blank()
    )
}

hold <- plotdt %>%
  group_by(measure, key) %>%
  tidyr::nest() %>%
  mutate(
    color = case_when(
      key == "Recycling" ~ list(c("#4daf4a", "#ff7f00")),
      TRUE ~ list(c("#377eb8", "#ff7f00"))
    ),
    title = case_when(
      key == "Recycling" & measure == "Per Person" ~ "Recycling Tons Per Person",
      key == "Recycling" & measure == "Total Pounds" ~ "Recycling Total Tons (x1000)",
      key == "Municipal Solid Waste +\\nConstruction & Demolition" & measure == "Per Person" ~ "Solid Waste Tons Per Person",
      key == "Municipal Solid Waste +\\nConstruction & Demolition" & measure == "Total Pounds" ~ "Solid Waste Total Tons (x1000)"
    ),
    p = purrr::pmap(
      .l = list(d = data, c = color, t = title),
      .f = function(d, c, t) plot_it(d, c, t))
  )

p <- plot_grid(
  hold$p[[3]] + labs(caption = " "),
  hold$p[[4]] + labs(caption = " "),
  hold$p[[1]] + labs(caption = " "),
  hold$p[[2]] + labs(caption = "Based on 2016-17 NCDEQ annual reports")
)

## Save figure ####

ggsave(
  "inst/land_solid_waste_fig1.pdf",
  plot = p, width = 7, height = 7
)
