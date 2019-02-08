
library(tidycensus)
library(magrittr)
library(dplyr)
library(ggplot2)

# v15 <- load_variables(2010, "sf2", cache = TRUE)
#
# View(v15)

nc <- get_acs(geography = "county",
              variables = c(medincome = "B19013_001"),
              year = 2015,
              state = "NC") %>%
  mutate(
    county = stringr::str_remove_all(NAME, " County, North Carolina")
  ) %>%
  select(county, medincome_15 = estimate)



test <- pdftools::pdf_text("inst/extdata/NC_SWMMAR_FY2017-18_CountyPerCapitaReport.pdf")
pages <- lapply(test, strsplit, "\n")
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

wastedata <- pdfdata %>%
  dplyr::mutate_at(
    .vars = 2:(ncol(.)-1),
    .funs = funs(text_to_numeric)
  ) %>%
  mutate(
    is_orange = county == "Orange"
  ) %>%
  left_join(
    nc, by = "county"
  )

lm(log2(percap_9192) ~ log2(pop_072017) + log10(medincome_15), data = wastedata ) %>% summary()
lm(log2(percap_1718) ~ log2(pop_072017) + log10(medincome_15), data = wastedata ) %>% summary()

glm(log2(percap_9192) ~ log10(medincome_15),
    family = gaussian(), data = wastedata) %>%
  summary()

glm(log(percap_1718) - log(percap_9192)   ~  pop_072017,
    family = gaussian(), data = wastedata) %>%
  summary()
glm(percap_1718 - percap_9192  ~  pop_072017,
    family = gaussian(), data = wastedata) %>%
  summary()



log10(wastedata$tons_managed_9192)
ggplot(
  wastedata,
  aes(x = percap_9192, y = percap_1718, size = medincome_15, color = is_orange)
) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(shape = 1) +

  stat_smooth(method = "loess", se = FALSE) +
  scale_x_continuous(
    limits = c(0, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 2)
  )

wastedata %>%
  select(county, pop_072017, is_orange, medincome_15, tons_managed_9192, msw_cd_1718) %>%
  tidyr::gather(
    key = "key", "value", -county, -is_orange, -pop_072017, -medincome_15
  ) %>%
  mutate(
    key = stringr::str_extract(key, "[0-9]{4}")
  ) -> hold



ggplot(
  hold,
  aes(x = log10(pop_072017), y = log10(value),
      size = log10(medincome_15), color = is_orange)
) +
  geom_point(shape = 1) +
  # scale_y_continuous(limits = c(-5, 5)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_grid(~ key)

ggplot(
  wastedata,
  aes(x = log10(pop_072017), y = log2(percap_1718), size = medincome_15, color = is_orange)
) +
  geom_point(shape = 1) +
  scale_y_continuous(limits = c(-5, 5)) +
  stat_smooth(method = "lm", se = FALSE)

