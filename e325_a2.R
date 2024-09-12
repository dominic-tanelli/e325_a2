# install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)

streamH <- read.csv("/cloud/project/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/site_info.csv")

# parse our data
streamH$dateF <- ymd_hm(streamH$datetime,
                        tz = "America/New_York")
year(streamH$dateF)

# join site info to stream gauge height 
floods <- full_join(streamH, siteinfo, by = "siteID")

# filter by siteID (peace river)
peace <- floods %>%
  filter(siteID == 2295637)

example <- floods %>%
  filter(gheight.ft >= 10)
# plot(peace$dataF, peace$gheight.ft, type = "l")

max.ht <- floods %>%
  group_by(names) %>%
  # summarise()
