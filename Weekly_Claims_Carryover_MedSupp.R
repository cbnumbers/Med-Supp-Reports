## claims processed per user script

## read in file

#claimsprocessed <- read_csv("claimsprocessedR.csv")

## script for weekly claims processed for med supp

library(tidyverse)
library(RODBC)
library(stringr)
library(lubridate)
library(knitr)

## Pull in data from database.  

## database name goes here
dsn.name <- "LPAR01"

## username used to log in
user.name <- "CBOUTTE"

## password used with above username
pw.name <-"Lucky096"

con1 <- odbcConnect(dsn=dsn.name, uid = user.name, pwd = pw.name)

wklyclmsrec <- sqlQuery(con1,"SELECT G1.* 
                             FROM (SELECT CLNO06, CLNO03, CLNO02, RTRIM(CLBUSRID) AS USERID, CLOPDT, CLOPTM, LEFT(CLNO06,1) AS UHE, 
                             (CLCC16*1000000+CLYY16*10000+CLMM16*100+CLDD16) AS RECDATE
                             FROM INSDLIB.MLPMCLM AS T1) AS G1
                             WHERE G1.CLNO02 IN ('135', '142', '163', '120', '121', '122') AND
                             G1.RECDATE BETWEEN 20170529 AND 20170602")

wklyclmsproc <- sqlQuery(con1,"SELECT CLNO06, CLNO03, CLCD02, RTRIM(CLBUSRID) AS USERID, CLOPDT, CLOPTM, LEFT(CLNO06,1) AS UHE
                             FROM INSDLIB.MLPTLIN AS T1
                         WHERE T1.CLNO02 IN ('135', '142', '163', '120', '121', '122') AND T1.CLCD02 IN ('9', 'P') AND T1.CLNO06 NOT LIKE ('%-%')
                         AND T1.CLOPDT BETWEEN 20170529 AND 20170602")

#write_csv(wklyclmsrec,"wklyclmsrec.csv")
#write_csv(wklyclmsproc,"wklyclmsproc.csv")

## Received Cleaning and Summaries

claimtype2r <- sub("U", "UB", wklyclmsrec$UHE)
claimtype3r <- sub("H", "HCFA", claimtype2r)
claimtype4r <- sub("E", "Electronic", claimtype3r)

wklyclmsrec_unique <- wklyclmsrec %>% 
  mutate(Date = ymd(RECDATE), ClaimType = claimtype4r, ClaimPol = paste(CLNO06,CLNO03,sep = "-")) %>% 
  group_by(ClaimPol) %>%
  mutate(rank = row_number(CLOPDT)) %>%
  filter(rank == "1")

Total_Received_Date <- wklyclmsrec_unique %>%
  group_by(Date, ClaimType) %>%
  summarise(n()) %>%
  spread(ClaimType, "n()") %>%
  mutate(Total = sum(2, Electronic, HCFA, UB))

Total_Received <- Total_Received_Date %>%
  summarise(TotalU = sum(UB), TotalH = sum(HCFA), TotalE = sum(Electronic), Totals = sum(Total)) 



## Processed Cleaning and Summaries

claimtype2 <- sub("U", "UB", wklyclmsproc$UHE)
claimtype3 <- sub("H", "HCFA", claimtype2)
claimtype4 <- sub("E", "Electronic", claimtype3)

wklyclmsproc_unique <- wklyclmsproc %>% 
  mutate(Date = ymd(CLOPDT), ClaimType = claimtype4, ClaimPol = paste(CLNO06,CLNO03,sep = "-")) %>% 
  group_by(ClaimPol) %>%
  mutate(rank = row_number(CLOPDT)) %>%
  filter(rank == "1", USERID != "LBLAKEY")

#write_csv(wklyclmsproc_unique, "wklyclmsproc_unique.csv")

Total_Processed_Date <- wklyclmsproc_unique %>%
  group_by(Date, ClaimType) %>%
  summarise(n()) %>%
  spread(ClaimType, "n()") %>%
  mutate(Total = sum(2, Electronic, HCFA, UB))

Total_Processed <- Total_Processed_Date %>%
  summarise(TotalU = sum(UB), TotalH = sum(HCFA), TotalE = sum(Electronic), Totals = sum(Total))


## Get total carryover for the week
#works if the tables are equal in size
#Days <- c("Saturday", "Monday", "Tuesday", "Wednsday", "Thursday", "Friday")
Days <- c("Tuesday", "Wednsday", "Thursday", "Friday")
Carryover <- data.frame(Total_Processed[,-1] - Total_Received[,-1], row.names= Days)


## removed automated users here as to not display them in the plots but will still include them in summary tables



## can write to csv to check data if something seems off
#write_csv(dlyclmsproc, "dlyclms.csv")
#write_csv(dlyclmsproc_3, "dlyclms_filt.csv")