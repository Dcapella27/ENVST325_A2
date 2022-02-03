# Load packages
library(dplyr)
library(lubridate)

# Read data
siteInfo <- read.csv("site_info.csv")
streamData <- read.csv("stream_gauge.csv")

# Prompt 1: Joining data
floodsData <- full_join(siteInfo, streamData, by = "siteID")

# Prompt 2: Parsing dates
floodsData$datetime <- ymd_hm(floodsData$datetime, tz = "America/New_York")

# Prompt 3: Earliest Date of Flood for Each River
floodsData %>%
  group_by(names) %>% 
  filter(gheight.ft >= major.ft) %>%
  summarise(min(datetime))

floodsData %>%
  filter(gheight.ft >= major.ft) %>% # checking if flood is major
  summarise(min(datetime))
 
