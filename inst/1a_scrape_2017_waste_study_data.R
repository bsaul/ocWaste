#------------------------------------------------------------------------------#
# TITLE: Scrape data from ORANGE COUNTY WASTE COMPOSITION STUDY June 2017
#  DATE: 20190207
#  PROG: B. Saul
#  DESC: Grabs data from
#         * each sample (Appendices B, C, D)
#         * TODO: Table A: Waste Composition Study Material Categories
#         * TODO: Table 3-4 Comparison of Single-Family Residential MSW to Previous WCS Results
#         * TODO: Table 3-6 Comparison of Multi-Family Residential MSW to Previous WCS Results
#         * TODO: Table 3-8: Comparison of Commercial MSW to Previous WCS Results
#
#------------------------------------------------------------------------------#


library(dplyr)
library(readxl)
wastepdf <- pdftools::pdf_text("inst/extdata/2017ocwastestudy.pdf")
pages <- purrr::map(wastepdf, ~strsplit(.x, "\n")[[1]])

## Sample Data ####
## Sample Results are contained in Appendices B, C, and D which are pages 37-58
sfr <- pages[39:46] # Single family residential
mfr <- pages[49:50] # Multi family residential
com <- pages[53:58] # Commercial

## Function that creates a data.frame from an appendix page
process_appendix_data_page <- function(x){
  # Get the source
  src  <- stringr::str_extract(x[4], "Table .{3}")

  # Get the timing of the sample
  time <- stringr::str_extract(x[4], "(Fall|Spring).*$")
  time <- stringr::str_remove(time, "\\(cont\\.\\)")

  # Get the jurisdiction
  # juris <- trimws(x[5])

  # Find the Load info row
  load_row <- which(grepl("Load information", x))
  # Find totals row
  tot_row <- which(grepl("TOTALS", x))

  # Get the data
  sample_ids <- strsplit(x[(load_row + 1)], "\\s\\s+")[[1]]
  sample_ids <- sample_ids[grepl("[0-9]+", sample_ids)]
  table_data <- x[(load_row + 2):(tot_row - 1)]
  table_data <- stringr::str_replace(table_data, "^\\s{0,2}", "   ")
  table_data <- purrr::map_dfr(
    .x = table_data,
    .f = ~ strsplit(.x, "\\s\\s+") %>%
      purrr::map_dfr(~ as.data.frame(t(.x), stringsAsFactors = FALSE)))

  names(table_data)[(ncol(table_data) - length(sample_ids) + 1):ncol(table_data)] <- sample_ids

  # Remove the number categories or all blank
  all_blank <- apply(table_data, 2, function(x) all(x == ""))
  # all_nums  <- apply(table_data, 2, function(x) all(grepl("^[0-9][A-Z]?$", x)))
  table_data <- table_data[ , -which(all_blank)]

  # browser()
  table_data <- table_data %>%
    select(-1L) %>%
    select(material = 1L, everything()) %>%
    tidyr::gather(
      key = "sample_id", value = "value", -material
    ) %>%
    filter(value != '') %>%
    mutate(
      sample_id    = trimws(sample_id),
      value        = as.numeric(stringr::str_remove(value, "%")),
      # jurisdiction = juris,
      year         = stringr::str_extract(time, "2016|2017"),
      month        = stringr::str_extract(time, "Fall|Spring"),
      pdf_source   = src
    )
  table_data
  # TODO: Get the sample/load information
  # load_info <- x[6:load_row]
  # load_info
  # strsplit(load_info, "\\s\\s+")
}

sampleinfo <- read_excel("inst/extdata/2017ocwastestudy_sampleinfo.xlsx") %>%
  mutate(
    sample_id = as.character(sample_id)
  )


oc_waste_2017 <- Map(
  function(x){ purrr::map_dfr(x, process_appendix_data_page) },
  list(sfr, mfr, com)) %>%
  bind_rows() %>%
  left_join(
    sampleinfo,
    by = c("sample_id", "pdf_source")
  ) %>%
  mutate(
    material_category = case_when(
      material %in% c("Newspaper", "Glossy Magazines", "Cardboard", "Waxy Cardboard",
                      "Phone Books", "Paperboard", "Other Books", "White Ledger",
                      "Mixed Recyclable Paper", "Low-Grade Paper", "Aseptic Containers") ~
        "Paper",
      material %in% c("All Plastic Bottles", "Plastic Film", "All Cups & Tubs",
                      "Dairy Plastic Containers", "Mixed Plastic Containers",
                      "Retail Bags & Stretch Film",
                      "All Other Plastics") ~ "Plastic",
      material %in% c("Food Waste", "Textiles/Leather", "Diapers", "Other Organics/Rubber") ~ "Organic",
      material %in% c("Tin/Steel Cans", "Aerosol Cans", "Other Ferrous") ~ "Ferrous Metal",
      material %in% c("Aluminum Cans", "Aluminum Foil", "Other Non-Ferrous") ~ "Non-Ferrous Metal",
      material %in% c("Glass Bottles and Jars", "Other Glass") ~ "Glass",
      material %in% c("Wood Pallets", "Wood Lumber", "Painted/Treated Wood", "Stumps/Branches") ~ "Wood",
      material %in% c("Brick/Concrete/Dirt", "Yard Waste", "Lead Acid Batteries", "Dry Cell Batteries",
                      "Oil Filters", "Other Hazardous Waste", "Infectious Waste",
                      "Reusable Waste") ~ "Special Waste",
      material %in% c("Electronic Waste") ~ "Electronic Waste"
    )
  )




save(oc_waste_2017, file = "data/oc_waste_composition_2017_report.rda")




