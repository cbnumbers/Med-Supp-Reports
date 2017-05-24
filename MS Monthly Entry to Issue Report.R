## Load Packages
library(tidyverse)
library(RODBC)
library(stringr)
library(lubridate)
library(knitr)

## database name goes here
dsn.name <- "LPAR01"

## username used to log in
user.name <- "CBOUTTE"

## password used with above username
pw.name <- "Lucky096"

## define connection variable here
con1 <- odbcConnect(dsn=dsn.name, uid = user.name, pwd = pw.name)

## Queries below

## Pulls all writing agents for Med Supp using row order to get writing agent
query_Ent_Apps<- sqlQuery(con1, "SELECT T1.MAPPLNO, T1.MPOLNO, T1.MSTATUS, T1.MAPPENTRDT, T2.PHDATE, T2.PHUSRDT
                                  FROM LOADLIB.MKLMAPP01 AS T1 INNER JOIN INSDLIB.POLPRHSP AS T2 ON T1.MAPPLNO = T2.PHAPPLNO
                                  WHERE T1.MBLOCK IN ('142', '163') AND T2.PHPRTCD = 'RL'
                                  AND T2.PHDATE >= 20170101
                          ")

## creating range categories and formatting dates
Ent_Apps <- query_Ent_Apps %>%
  mutate(EntryDate = ymd(MAPPENTRDT), ReleaseDate = ymd(PHDATE)) %>%
  mutate(Month = paste(month(ReleaseDate)), Day_to_Release = ReleaseDate - EntryDate) %>%
  mutate(Range =  ifelse(Day_to_Release == 0, "00", 
                   ifelse(Day_to_Release == 1, "01", 
                    ifelse(Day_to_Release == 2, "02",
                     ifelse(Day_to_Release == 3, "03",
                      ifelse(Day_to_Release == 4, "04",
                       ifelse(Day_to_Release == 5, "05",
                        ifelse(Day_to_Release == 6, "06", 
                         ifelse(Day_to_Release == 7, "07", 
                          ifelse(Day_to_Release == 8, "08",
                           ifelse(Day_to_Release == 9, "09", 
                            ifelse(Day_to_Release == 10, "10",
                             ifelse(Day_to_Release >= 70, "70+", 
                              ifelse(Day_to_Release >= 60, "60-69", 
                               ifelse(Day_to_Release >= 50, "50-59",
                                ifelse(Day_to_Release >= 40, "40-49",
                                 ifelse(Day_to_Release >= 35, "35-39",
                                  ifelse(Day_to_Release >= 30, "30-34",
                                   ifelse(Day_to_Release >= 25, "25-29", 
                                    ifelse(Day_to_Release >= 20, "20-24", 
                                     ifelse(Day_to_Release >= 15, "15-19",
                                      ifelse(Day_to_Release > 10, "11-14", "NA"
                                                                   ))))))))))))))))))))))

## Get total Apps per each range
Range_Totals <- Ent_Apps %>%
  ## filter(Month == 4) %>%
  group_by(Month, Range) %>%
  summarise(Range_Sum = n())

Totals <- Ent_Apps %>%
  filter(Month == 4) %>%
  summarise(Total_Apps = n(), Avg_Days = mean(Day_to_Release))


## WIP below
Monthly_Totals <- Range_Totals %>%
  spread(Range_Totals$Month) %>%
  

ggplot(Range_Totals, aes(x = Range, y = Range_Sum, linetype = Month)) + 
  geom_point() + 
  facet_wrap(~ Month) +
  labs(title = "Days to Release by Month", y = "Apps", x = "Days to Release", fill = "Month") +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))

geom_point() +
  scale_x_discrete((Range_List = c("0","1","2", "3", "4", "5", "6", "7", "8", "9", "10", "11-14",
                                   "15-19", "20-24", "25-29", "30-34", "35-39", "40-49", "50-59", "60-69", "70+")))

