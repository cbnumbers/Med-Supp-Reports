

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



## just make multiple summary charts for each type of data needed

## Pulls all 
query_MHTStats<- sqlQuery(con1,"SELECT 
                                FROM LOADLIB.MKLMAPP01 AS T1 INNER JOIN LOADLIB.MKPOLADMP AS T2 ON 
                                INNER JOIN INSDLIB.POLPRHSP AS T3 ON 
