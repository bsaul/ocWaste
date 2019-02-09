#------------------------------------------------------------------------------#
# TITLE: Scrape data from NC reycling rates report
#  DATE: 20190207
#  PROG: B. Saul
#  DESC: Grabs data from 2017_nc_recycling_rates.pdf
#
#------------------------------------------------------------------------------#

library(dplyr)

ncrecycle <- pdftools::pdf_text("inst/extdata/2017_nc_recycling_rates.pdf")
pages     <- purrr::map(ncrecycle, ~ strsplit(.x, "\n")[[1]])

# Drop intro text
pages[[1]] <- pages[[1]][28:37]
pages <- trimws(unlist(pages))

# Clean up
pages <- stringr::str_replace_all(pages, "COUNTY", "")
pages <- stringr::str_replace_all(pages, "NEW HANOVER", "NEW_HANOVER")
pdfdata <- purrr::map_dfr(
  .x = pages,
  .f = ~ as.data.frame(t(strsplit(.x, "\\s+")[[1]]), stringsAsFactors = FALSE)) %>%
  select(-V1, -V4)

total_recycling <- pdfdata %>%
  select(county = V2, lbs_person = V3) %>%
  mutate(recovery_type = "total_recycling_per_person")

common_recycling <- pdfdata %>%
  select(county = V5, lbs_person = V6) %>%
  mutate(recovery_type = "common_household_recycling_per_person")

nc_recycling_1617_report <- bind_rows(total_recycling, common_recycling) %>%
  mutate(
    county = lettercase::str_title_case(tolower(county)),
    lbs_person = as.numeric(lbs_person)
  ) %>%
  tidyr::spread(
    key = recovery_type, value = lbs_person
  )

save(nc_recycling_1617_report, file = "data/nc_recycling_1617_report.rda")

rm(list = ls())

