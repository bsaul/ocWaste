#------------------------------------------------------------------------------#
# TITLE: Collect reports needed to scrape data from
#  DATE: 20190207
#  PROG: B. Saul
#  DESC:
#
#------------------------------------------------------------------------------#

# 2017 Orange County Waste Characterization Report
download.file(
  url = "http://www.orangecountync.gov/DocumentCenter/View/2826/2017-Orange-County-Waste-Characterization-Study-Final-Report-PDF?bidId=",
  destfile = "inst/extdata/2017ocwastestudy.pdf"
)

# 2016-2017 NC Recycling Rates by County
# also here: https://deq.nc.gov/news/press-releases/2018/06/08/county-and-municipality-recycling-numbers-show-5-percent-increase
download.file(
  url = "https://files.nc.gov/ncdeq/Environmental%20Assistance%20and%20Customer%20Service/Local%20Govt/County%20Recycling%20Program%20Performance%20FY%202016-17.pdf",
  destfile = "inst/extdata/2017_nc_recycling_rates.pdf"
)

# 2016-2017 NC per capita MSW rates
download.file(
  url = "https://edocs.deq.nc.gov/WasteManagement/0/edoc/996950/NC_SWMMAR_FY2016-17_CountyPerCapitaReport.pdf?searchid=b1697dcf-7917-4c91-950e-fb192ed909de",
  destfile = "inst/extdata/2017_nc_waste_rates.pdf"
)
