
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
                       WHERE T1.MBLOCK IN ('142', '163') AND T1.MSTATUS <> '10' AND T1.MUSERID <> 'KTRICE' 
                       AND T1.MAPPRCVDT >= 20130930")

query_Sp <- sqlQuery(con1, "SELECT SUBSTRING(T1.PSNO03,1,7) AS MAPPLNO, RIGHT(T2.PSFLEX7,1) AS HHD
                     FROM LOADLIB.PSLMPOL1 AS T1 INNER JOIN INSDLIB.PSPOLADMP AS T2 ON (T1.PSNO01 = T2.PSNO01 AND 
                     T1.PSNO02 = T2.PSNO02 AND T1.PSNO03 = T2.PSNO03)
                     WHERE T1.PSNO02 IN ('142', '163')
                     ")


## Data Cleaning Start

## Apps sheet cleaning

Sign_Dt <- ymd(query_Apps$MAPPSIGDT)
Rec_Dt <- ymd(query_Apps$MAPPRCVDT)
Ent_Dt <- ymd(query_Apps$MAPPENTRDT)
Iss_Dt <- ymd(query_Apps$MISSUEDT)
User_Dt <- ymd(query_Apps$MUSEROPDT)

####### Need to create status names... Probably using a naming vector and the sub command? #######

Apps_Received <- query_Apps %>%
  mutate(Sign_Date = Sign_Dt, Rec_Date = Rec_Dt, Ent_Date = Ent_Dt, Iss_Date = Iss_Dt, User_Date = User_Dt) %>%
  mutate(Issue_Age = if_else(query_Apps$RISSUEAGE <= 64, "0-64", paste(RISSUEAGE))) %>%
  mutate(App_Status = if_else(query_Apps$MSTATUS == "09", "UNDERWRITING", 
                       if_else(query_Apps$MSTATUS == "08", "PENDING INFO", 
                        if_else(query_Apps$MSTATUS == "07", "PENDING PHONE INTERVIEW", 
                         if_else(query_Apps$MSTATUS == "PA", "PENDING AGT APPT",
                          if_else(query_Apps$MSTATUS == 20, "ACTIVE", 
                           if_else(query_Apps$MSTATUS == 16, "WITHDRAWN", 
                            if_else(query_Apps$MSTATUS == 13, "DECLINED","NA"
                                    ))))))))

## COMBINE NAMES FOR AGENT LISTS
Writing_agts <- query_WRTAGT %>%
  mutate(Writing_Agent = if_else(is.na(AGLNAME),"", paste(trimws(AGFNAME), trimws(AGLNAME), sep = ", ")))
  


## Join Agents and Apps
Apps_Full_Data <- Apps_Received %>%
  left_join(Writing_agts, by = "MAPPLNO", "MBLOCK") %>%
  left_join(query_Sp, by = "MAPPLNO")%>%
  select(-MAPPSIGDT, -MAPPRCVDT, -MAPPENTRDT, -MISSUEDT, -MPLANCODE, -RN, -MBLOCK.y, -AGFNAME, -AGLNAME) %>%
  rename(Company = MCOMPANY, Block = MBLOCK.x, App_Number = MAPPLNO, Status = MSTATUS, Issue_State = MISSUEST, 
         Plan = PLAN, Gender = RSEXCD, Web_App = MOTHERIND, Smoker = RSMOKER,
         UW_Flag = MSMODECD, User_ID = MUSERID, Writing_Agent_Number = AGENTNO, Writing_Agent_Level = AGLEVEL, State_Appt = MCOMPCODE) %>%
  select(Company, Block, App_Number, Status, App_Status, Sign_Date, Rec_Date, Ent_Date, Iss_Date, Issue_State, Issue_Age, Plan, Gender, Web_App,
         Smoker, UW_Flag, HHD, State_Appt, User_ID, User_Date, Writing_Agent, Writing_Agent_Number)

## Export Apps Full Data to CSV
write_csv(Apps_Full_Data, "MHT_Apps_Rec_20170523.csv")  

## Some Plots and Summaries

Apps_by_RecDate <- Apps_Full_Data %>%
  filter(Rec_Date >= "2016-01-01") %>%
  group_by(Rec_Date) %>%
  arrange(Rec_Date) %>%
  summarise(Count_Apps = n())

Apps_by_RecDate %>%
  ggplot(aes(x = Rec_Date, y = Count_Apps))+
  geom_line()+
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
  labs(title = "Applications by Received Date", x = "Received Date") +
  scale_y_continuous(breaks = c(0, 50, 100, 200, 400, 600))

Apps_Full_Data %>%
  filter(Rec_Date >= "2016-01-01") %>%
  ggplot(aes(x = Rec_Date, fill = UW_Flag))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
  facet_wrap(~ Iss_Year) +
  labs(title = "Applications by Received Date", x = "Received Date") +
  scale_y_continuous(breaks = c(0, 50, 100, 200, 400, 600))

#Apps_Full_Data2 <- Apps_Full_Data %>%
#  select(Company = Apps_Received$MCOMPANY, Block = Apps_Received$MBLOCK, App_Number = Apps_Received$MAPPLNO, STATUS = Apps_Received$MSTATUS, 
#          Apps_Received$Sign_Date, Apps_Received$Rec_Date, Apps_Received$Ent_Date, Apps_Received$Iss_Date, Issue_State = Apps_Received$MISSUEST, 
#          Issue_Age = Apps_Received$RISSUEAGE, Apps_Received$Underage, Plan = Apps_Received$PLAN, Gender = Apps_Received$RSEXCD, Internet = Apps_Received$MOTHERIND, 
#          HHD = Apps_Received$SPCODE, Smoker = Apps_Received$RSMOKER, UW_Flag = Apps_Received$MSMODECD, User_ID = Apps_Received$MUSERID, 
#          Agent_Number = Apps_Received$AGAGENTNO, Apps_Received$Writing_Agent)

write_csv(Apps_Full_Data, "MHT_Apps_Rec_20170501.csv")  


write.xlsx(Apps_Full_Data, file = "Apps_Full_Data")
#wb <- loadWorkbook("Apps_Rec_20170331.xlsx")
#createSheet(wb, 'Q')
#writeWorksheet(wb,Apps_Full_Data, "Q")
#saveWorkbook(wb)  
