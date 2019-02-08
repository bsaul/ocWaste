#------------------------------------------------------------------------------#
# TITLE: Scrape data from ORANGE COUNTY WASTE COMPOSITION STUDY June 2017
#  DATE: 20190207
#  PROG: B. Saul
#  DESC: Grabs data from
#
#------------------------------------------------------------------------------#


library(dplyr)

ncwaste <- pdftools::pdf_text("inst/extdata/2017_nc_waste_rates.pdf")
pages   <- lapply(ncwaste, strsplit, "\n")
pdfdata <-  lapply(pages, function(x) x[[1]][-c(1,2, length(x[[1]]))])
pdfdata[[4]] <- pdfdata[[4]][-c((length(pdfdata[[4]])-3) : length(pdfdata[[4]]))]
pdfdata <- purrr::map_dfr(
  .x = pdfdata,
  .f = ~ strsplit(.x[5:length(.x)], "\\s\\s+") %>%
    purrr::map_dfr(~ as.data.frame(t(.x), stringsAsFactors = FALSE)))

names(pdfdata) <- c("county", "pop_072017", "tons_managed_9192", "msw_cd_1314",
                    "msw_cd_1415", "msw_cd_1516", "msw_cd_1617", "msw_cd_1718",
                    "percap_9192", "percap_1718", "percap_change_9192_1718")


text_to_numeric <- function(x){
  as.numeric(stringr::str_remove_all(x, ","))
}

nc_waste_rates <- pdfdata %>%
  dplyr::mutate_at(
    .vars = 2:(ncol(.)-1),
    .funs = funs(text_to_numeric)
  ) %>%
  mutate(
    is_orange = county == "Orange"
  )

