# Load packages
library(dplyr)
library(ggplot2)
library(lubridate)

# Read data
siteInfo <- read.csv("site_info.csv")
stream <- read.csv("stream_gauge.csv")

# Prompt 1: Joining data
floods <- full_join(siteInfo, stream, by = "siteID")

# Prompt 2: Parsing dates
floods$datetime <- ymd_hm(floods$datetime, tz = "America/New_York")

# Prompt 3: Earliest Date of Flood for Each River
floods %>%
  group_by(names) %>% 
  filter(gheight.ft >= flood.ft) %>%
  summarise(firstFloodDate = min(datetime), 
            lastFloodDate = max(datetime),
            maxFloodHeight = max(gheight.ft))

# Homework 2 Part 1

# Evaluating condition for Santa Fe River. If yes, return "Northern Florida". 
# If no, return "Central Florida"
floods$RiverLocaton <- ifelse(floods$names == 
                                    "SANTA FE RIVER NEAR FORT WHITE", 
                                  "Northern Florida", "Central Florida")
View(floods)

# Evaluating same condition, but returning integers
floods$RiverLocatonInt <- ifelse(floods$names == 
                                    "SANTA FE RIVER NEAR FORT WHITE", 1, 0)
View(floods)

# Evaluating same condition, but returning a missing value or a string
floods$RiverLocationNA <- ifelse(floods$names == 
                                       "SANTA FE RIVER NEAR FORT WHITE", NA, 
                                     "Not missing")
View(floods)

# Evaluating the same condition, but returning TRUE or FALSE
floods$RiverLocationBool <- ifelse(floods$names == 
                                       "SANTA FE RIVER NEAR FORT WHITE", TRUE, 
                                     FALSE)
View(floods)

# Homework 2 Part 2
colnames(floods)[2] <- "RiverNames"
floods$floodStatus <- as.factor(ifelse(floods$gheight.ft >= floods$major.ft, "Major Flood", 
                                       ifelse(floods$gheight.ft >= floods$moderate.ft, "Moderate Flood",
                                              ifelse(floods$gheight.ft >= floods$flood.ft, "Minor Flood",
                                                     ifelse(floods$gheight.ft >= floods$action.ft, "Action Stage", 
                                                            "No Action Needed")))))

View(floods)
# Homework 2 Part 2 Question 1
ggplot(data = floods, aes(x = datetime, y = gheight.ft, color = floodStatus)) +
  geom_point() +
  scale_color_manual(values = c("No Action Needed" = "black",
                                "Action Stage" = "red",
                                "Minor Flood" = "#00DAFC",
                                "Moderate Flood" = "#959BF2",
                                "Major Flood" = "#0029FF"
                                )) +
  facet_wrap(vars(RiverNames)) +
  theme_bw()
  
# Homework 2 Part 2 Question 2
earliestFloodDate <- floods %>%
  group_by(RiverNames, floodStatus) %>% 
  filter(gheight.ft >= flood.ft) %>%
  summarise(firstFloodDate = min(datetime), 
            lastFloodDate = max(datetime),
            maxFloodHeight = max(gheight.ft))
View(earliestFloodDate)
