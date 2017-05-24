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

## Pulls all Pending Applications for Med Supp
query_Pend_Apps<- sqlQuery(con1, "SELECT MAPPLNO, MAPPRCVDT, MISSUEDT, MSTATUS, RTRIM(LTRIM(MLNAME)) AS LNAME
                    FROM LOADLIB.MKLMAPP01
                    WHERE MSTATUS IN ('07', '08', '09', '10') AND MBLOCK IN ('142', '163')
                    ")

Pending_Apps <- query_Pend_Apps %>%
  mutate(RECDATE = ymd(MAPPRCVDT), ISSDATE = ymd(MISSUEDT)) %>%
  filter(LNAME != "TEST", MSTATUS != "10" & MISSUEDT != 0) %>%
  mutate(Days_Pending = Sys.Date()-RECDATE) %>%
  mutate(Pending_Range = ifelse(Days_Pending <= 4, "0-4", 
                          ifelse(Days_Pending >= 32, ">=32", 
                           ifelse(Days_Pending > 24, "25-31", 
                            ifelse(Days_Pending > 19, "20-24",
                             ifelse(Days_Pending > 14, "15-19",
                              ifelse(Days_Pending > 9, "10-14",
                               ifelse(Days_Pending > 4, "5-9", "NA"
                                       ))))))))

Summary_Pending <- Pending_Apps %>%
  group_by(Pending_Range) %>%
  summarise(Range_Sum = n())

Summary_Totals <- Pending_Apps %>%
  summarise(Avg_Pending_Age = round(mean(Days_Pending), digits=2), Total_Apps = n())

  

  
 