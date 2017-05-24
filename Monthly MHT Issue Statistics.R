

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
query_MHTStats<- sqlQuery(con1,"SELECT T1.PSNO03, T1.PSCD02, T1.PSCD04, T4.PHDATE, (T1.PSCC01*1000000+T1.PSYY01*10000+T1.PSMM01*100+T1.PSDD01) AS ISSUEDATE,
                                T1.PSAD19, T2.PSCD09, T2.PSCDA3, T3.PSCD58, T3.PSCD59, T2.PSNO13,
                                CASE WHEN T1.PSAD19 = 'WI' THEN SUBSTRING(T1.PSCD04, 3,2)
                                ELSE SUBSTRING(T1.PSCD04,7,1) 
                                END AS PLAN
                                FROM LOADLIB.PSLMPOL1 AS T1 LEFT JOIN LOADLIB.PSPMRDR AS T2 ON T1.PSNO03 = T2.PSNO03 LEFT JOIN
                                INSDLIB.POLBILLP AS T3 ON T1.PSNO03 = T3.PSC#01 LEFT JOIN INSDLIB.POLPRHSP AS T4 ON T1.PSNO03 = T4.PHPOLICY
                                WHERE T1.PSNO02 IN ('142', '163') AND T2.PSNO04 = 1 AND T4.PHPRTCD = 'RL'
                                AND T4.PHDATE BETWEEN 20170401 AND 20170430
                                ")

MHTEnt_Apps <- query_MHTStats %>%
  mutate(UserDate = ymd(PHDATE), IssueDate = ymd(ISSUEDATE)) %>%
  mutate(Month = paste(month(UserDate))) %>%
  mutate(Age_Range =  ifelse(PSNO13 < 65, "0-64", 
                         ifelse(PSNO13 >= 84, "84+", 
                                ifelse(PSNO13 >= 81, "81-83",
                                       ifelse(PSNO13 >= 78, "78-80",
                                              ifelse(PSNO13 >= 75, "75-77",
                                                     ifelse(PSNO13 >= 72, "72-74",
                                                            ifelse(PSNO13 >= 69, "69-71",
                                                                   ifelse(PSNO13 >= 66, "66-68", "65"
                                                                   )))))))))

Summary_Iss_Age <- MHTEnt_Apps %>%
  group_by(Age_Range) %>%
  summarise(n())

Summary_Plan <- MHTEnt_Apps %>%
  group_by(PLAN) %>%
  summarise(n())

Summary_Gender <- MHTEnt_Apps %>%
  group_by(PSCD09) %>%
  summarise(n())

Summary_Payment_Type <- MHTEnt_Apps %>%
  group_by(PSCD59) %>%
  summarise(n())

Summary_Payment_Mode <- MHTEnt_Apps %>%
  group_by(PSCD58) %>%
  summarise(n())

Summary_Smoker <- MHTEnt_Apps %>%
  group_by(PSCDA3) %>%
  summarise(n())
                                                                          
                                                     
                        
                        