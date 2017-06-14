## Pulls Agents and Apps for the comparision on plan A Sellers

## Load Packages
library(tidyverse)
library(RODBC)
library(stringr)
library(lubridate)
library(knitr)
library(XLConnect)

## database name goes here
dsn.name <- "LPAR01"

## username used to log in
user.name <- "CBOUTTE"

## password used with above username
pw.name <-"Lucky097"

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

## Pulls all Upline agents for Med Supp using row order to get writing agent
query_UPLAGT<- sqlQuery(con1, "SELECT G.* 
                     FROM (SELECT ROW_NUMBER() OVER(PARTITION BY T3.MAPPLNO ORDER BY T1.AGLEVEL) AS RN, T3.MBLOCK, T3.MAPPLNO, 
                     T1.AGAGENT# AS AGENTNO, T1.AGFNAME, T1.AGLNAME, T1.AGLEVEL 
                     FROM INSDLIB.AGTMSIDJ2 AS T1 LEFT JOIN LOADLIB.MKLMAGT05 AS T2 ON (T1.AGAGENT# = T2.AGENTNO) 
                     LEFT JOIN LOADLIB.MKLMAPP01 AS T3 ON (T2.AAPPLNO = T3.MAPPLNO)
                     WHERE T3.MBLOCK IN ('142', '163') AND T3.MAPPLNO > 3089265 AND T1.AGLEVEL NOT IN ('01','  ')
                     ORDER BY 7) AS G
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
                      WHERE T1.MBLOCK IN ('142', '163') AND T1.MSTATUS <> '10' AND T1.MUSERID <> 'KTRICE' 
                      AND T1.MAPPRCVDT >= 20170101 AND T1.MAPPRCVDT <= 20170609
                      ")

## Date Variables for the last two summary tables... only change FriLastWeek vaiable to the date of friday of previous week
FriLastWeek <- as.Date("2017-06-09")
Fri2Weeks_Ago <- FriLastWeek - 7


## Data Cleaning Start

## Apps sheet cleaning
Sign_Dt <- ymd(query_Apps$MAPPSIGDT)
Rec_Dt <- ymd(query_Apps$MAPPRCVDT)
Ent_Dt <- ymd(query_Apps$MAPPENTRDT)
Iss_Dt <- ymd(query_Apps$MISSUEDT)
User_Dt <- ymd(query_Apps$MUSEROPDT)

####### Need to create status names... Probably using a naming vector and the sub command? #######

Apps_Received <- query_Apps %>%
  mutate(Applicant_Name = paste(trimws(MLNAME), trimws(MFNAME), sep = ", ")) %>%
  mutate(Sign_Date = Sign_Dt, Rec_Date = Rec_Dt, Ent_Date = Ent_Dt, Iss_Date = Iss_Dt, User_Date = User_Dt) %>%
  mutate(Issue_Age = if_else(query_Apps$RISSUEAGE <= 64, "0-64", paste(RISSUEAGE))) %>%
  mutate(Week_Num = week(Rec_Date))
  

## COMBINE NAMES FOR AGENT LISTS
Writing_agts <- query_WRTAGT %>%
  mutate(Writing_Agent = paste(trimws(AGLNAME), trimws(AGFNAME), sep = ", ")) %>%
  select(-RN, -MBLOCK, -AGFNAME, -AGLNAME)

Upline_agts <- query_UPLAGT %>%
  mutate(Upline_Agent = paste(trimws(AGLNAME), trimws(AGFNAME), sep = ", ")) %>%
  select(-RN, -MBLOCK, -AGFNAME, -AGLNAME)

## Join Agents and Apps
Apps_Full_Data <- Apps_Received %>%
  left_join(Writing_agts, by = "MAPPLNO", "MBLOCK") %>%
  left_join(Upline_agts, by = "MAPPLNO", "MBLOCK") %>%
  select(-MAPPSIGDT, -MAPPRCVDT, -MAPPENTRDT, -MISSUEDT, -MPLANCODE, -MFNAME, -MLNAME, -MMINIT) %>%
  rename(Company = MCOMPANY, Block = MBLOCK, App_Number = MAPPLNO, Status = MSTATUS, Issue_State = MISSUEST, 
         Plan = PLAN, Gender = RSEXCD, Web_App = MOTHERIND, Smoker = RSMOKER, Web_App = MOTHERIND, State_Appt = MCOMPCODE,
         UW_Flag = MSMODECD, User_ID = MUSERID, Writing_Agent_Number = AGENTNO.x, Writing_Agent_Level = AGLEVEL.x) %>%
  select(Company, Block, App_Number, Applicant_Name, Status, Week_Num, Sign_Date, Rec_Date, Ent_Date, Iss_Date, Issue_State, Issue_Age, Plan, Gender, Web_App,
         Smoker, UW_Flag, State_Appt, User_ID, User_Date, Writing_Agent_Number, Writing_Agent, Writing_Agent_Level, Upline_Agent)
  
write_csv(Apps_Full_Data, "PlanA_Apps_Rec.csv")

## Summary Tables need work/verification

summary_YTD <- Apps_Full_Data %>%
  group_by(Writing_Agent_Number, Writing_Agent, Upline_Agent, Plan) %>%
  summarise(n()) %>%
  spread(Plan, "n()") %>%
  filter(A != "NA")

summary_WO_Last_Week <- Apps_Full_Data %>%
  filter(Rec_Date <= Fri2Weeks_Ago) %>%
  group_by(Writing_Agent_Number, Writing_Agent, Upline_Agent, Plan) %>%
  summarise(n()) %>%
  spread(Plan, "n()") %>%
  filter(A != "NA")

summary_Last_Week <- Apps_Full_Data %>%
  filter(Rec_Date >= FriLastWeek - 6 & Rec_Date <= FriLastWeek) %>%
  group_by(Writing_Agent_Number, Writing_Agent, Upline_Agent, Plan) %>%
  summarise(n()) %>%
  spread(Plan, "n()") %>%
  filter(A != "NA")

write_csv(summary_YTD, "Plan A YTD Summary.csv")
write_csv(summary_WO_Last_Week, "Plan A WOLW Summary.csv")
write_csv(summary_Last_Week, "Plan A LW Summary.csv")

######  The code below works but need a decent cpu to run!!!!! Definitely one of the 64 bit machines  #########

#wb <- loadWorkbook("Weekly_MHT_Apps_PlanA.xlsx", create = TRUE)

#createSheet(wb,'YTD Summary')
#createSheet(wb,'WOLW Summary')
#createSheet(wb,'LW Summary')

#writeWorksheet(wb, summary_YTD, "YTD Summary")
#writeWorksheet(wb, summary_WO_Last_Week, "WOLW Summary")
#writeWorksheet(wb, summary_Last_Week, "LW Summary")

#saveWorkbook(wb,"Weekly_MHT_Apps_PlanA.xlsx")


  
