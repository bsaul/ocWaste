#------------------------------------------------------------------------------#
# TITLE: Collect reports needed to scrape data from
#  DATE: 20190207
#  PROG: B. Saul
#  DESC:
#
#------------------------------------------------------------------------------#

download.file(
  url = "http://www.orangecountync.gov/DocumentCenter/View/2826/2017-Orange-County-Waste-Characterization-Study-Final-Report-PDF?bidId=",
  destfile = "inst/extdata/2017ocwastestudy.pdf"
)
