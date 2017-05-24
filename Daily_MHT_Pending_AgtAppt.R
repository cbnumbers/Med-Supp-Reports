
## Load Packages
library(tidyverse)
library(RODBC)
library(stringr)
library(lubridate)


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
query_WRTAGT<- sqlQuery(con1, "SELECT G.* 
                        FROM (SELECT ROW_NUMBER() OVER(PARTITION BY T3.MAPPLNO ORDER BY T1.AGLEVEL DESC) AS RN, T3.MBLOCK, T3.MAPPLNO, 
                        T1.AGAGENT# AS AGENTNO, T1.AGFNAME, T1.AGLNAME, T1.AGLEVEL 
                        FROM INSDLIB.AGTMSIDJ2 AS T1 LEFT JOIN LOADLIB.MKLMAGT05 AS T2 ON (T1.AGAGENT# = T2.AGENTNO) 
                        LEFT JOIN LOADLIB.MKLMAPP01 AS T3 ON (T2.AAPPLNO = T3.MAPPLNO)
                        WHERE T3.MBLOCK IN ('142', '163') AND T3.MAPPLNO > 3089265 AND T1.AGLEVEL NOT IN ('01','  ')
                        ORDER BY 7 DESC) AS G
                        WHERE RN=1")

## Pulls apps date range provided.  Extracts segments needed from fields
query_Apps <- sqlQuery(con1, "SELECT T1.MCOMPANY, T1.MBLOCK, T1.MAPPLNO, T1.MLNAME, T1.MFNAME, T1. MMINIT, T1.MSTATUS, 
                       T1.MAPPSIGDT, T1.MAPPRCVDT, T1.MAPPENTRDT, T1.MISSUEDT, T1.MISSUEST,
                       T2.RISSUEAGE, T1.MPLANCODE, T2.RSEXCD, T1.MOTHERIND,
                       T2.RSMOKER, T3.MSMODECD, T1.MCOMPCODE, T1.MUSERID, T1.MUSEROPDT,
                       CASE WHEN T1.MISSUEST = 'WI' THEN SUBSTRING(T1.MPLANCODE, 3,2)
                       ELSE SUBSTRING(T1.MPLANCODE,7,1) 
                       END AS PLAN
                       FROM LOADLIB.MKLMAPP01 AS T1 LEFT JOIN LOADLIB.MKLMRDR01 AS T2 ON (T1.MAPPLNO = T2.RAPPLNO) 
                       LEFT JOIN LOADLIB.MKPOLADMP AS T3 ON (T1.MAPPLNO = T3.MMAPPLNO)
                       WHERE T1.MBLOCK IN ('142', '163') AND T1.MSTATUS = 'PA' AND T1.MUSERID <> 'KTRICE' 
                       AND T1.MAPPRCVDT >= 20130930")

## Data Cleaning Start


Sign_Dt <- ymd(query_Apps$MAPPSIGDT)
Rec_Dt <- ymd(query_Apps$MAPPRCVDT)
Ent_Dt <- ymd(query_Apps$MAPPENTRDT)
Iss_Dt <- ymd(query_Apps$MISSUEDT)
User_Dt <- ymd(query_Apps$MUSEROPDT)

####### Need to create status names... Probably using a naming vector and the sub command? #######

Apps_Received <- query_Apps %>%
  mutate(Sign_Date = Sign_Dt, Rec_Date = Rec_Dt, Ent_Date = Ent_Dt, Iss_Date = Iss_Dt, User_Date = User_Dt) %>%
  mutate(Issue_Age = if_else(query_Apps$RISSUEAGE <= 64, "0-64", paste(RISSUEAGE))) %>%
  mutate(App_Status = if_else(query_Apps$MSTATUS == "PA", "PENDING AGT APPT", ""))

## COMBINE NAMES FOR AGENT LISTS
Writing_agts <- query_WRTAGT %>%
  mutate(Writing_Agent = paste(trimws(AGFNAME), trimws(AGLNAME), sep = ", "))


## Join Agents and Apps
Apps_Full_Data <- Apps_Received %>%
  left_join(Writing_agts, by = "MAPPLNO", "MBLOCK") %>%
  mutate(Days_From_Rec = Sys.Date()-Rec_Date) %>%
  rename(App_Number = MAPPLNO, Status = MSTATUS, Issue_State = MISSUEST, 
         Plan = PLAN, Web_App = MOTHERIND, Smoker = RSMOKER, UW_Flag = MSMODECD, User_ID = MUSERID, 
         Writing_Agent_Number = AGENTNO, Writing_Agent_Level = AGLEVEL, State_Appt = MCOMPCODE) %>%
  select(App_Number, App_Status, Sign_Date, Rec_Date, Iss_Date, Issue_State, Writing_Agent, Writing_Agent_Number, Days_From_Rec)

write_csv(Apps_Full_Data, "MHT_Pending_AgtAppt_20170523.csv")  


