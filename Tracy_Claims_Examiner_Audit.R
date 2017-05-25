library(tidyverse)
library(RODBC)
library(stringr)
library(lubridate)
library(XLConnect)

## database name goes here
dsn.name <- "LPAR01"

## username used to log in
user.name <- "CBOUTTE"

## password used with above username
pw.name <-"Lucky096"

con1 <- odbcConnect(dsn=dsn.name, uid = user.name, pwd = pw.name)

## write SQL query here , HLPMCLM.CLOPTM

Claims_query <- sqlQuery(con1, "SELECT T1.CLNO02 AS BLOCK, T1.CLNO03 AS POLICY, T1.CLNO04 AS RIDER, T1.CLNO06 AS CLAIMNO, 
                                T1.CLNO07 AS CLAIMANT, T1.CLCD02 AS STATUS, T1.CLCC09 , T1.CLYY09, 
                           T1.CLMM09, T1.CLDD09, T1.CLAM07, RTRIM(T1.CLBUSRID) AS USERID, T1.CLOPDT, T1.CLOPTM
                           FROM INSDLIB.MLPTLIN AS T1
                           WHERE T1.CLNO02 IN ('135', '142', '120', '121', '122') AND 
                          T1.CLBUSRID IN ('APARKER', 'DCROMEEN', 'HZUNIGA', 'MDUONG', 'NSPARKS', 
                          'SAUSTIN', 'MLARSON') AND T1.CLOPDT BETWEEN 20170515 AND 20170519
                           ORDER BY CLBUSRID ASC, CLOPDT ASC, CLOPTM ASC, CLNO02 ASC, CLNO03")

## Some Variables
User_Date <- ymd(Claims_query$CLOPDT)
wb <- loadWorkbook("Tracy_Examiner_Audit.xlsx", create = TRUE)

## Create 2.5% list for each user id

## APARKER
Claims_List_APARKER <- Claims_query %>%
  mutate(DATE = User_Date) %>%
  filter(USERID == "APARKER") 
if(nrow(Claims_List_APARKER)>40){
  APARKER_Sample <- Claims_List_APARKER %>%
    sample_frac(.025)
}
APARKER_Sample <- Claims_List_APARKER %>%
  sample_frac(.25)

createSheet(wb,'APARKER')
writeWorksheet(wb, APARKER_Sample, "APARKER")

## DCROMEEN
Claims_List_DCROMEEN <- Claims_query %>%
  mutate(DATE = User_Date) %>%
  filter(USERID == "DCROMEEN") 
if(nrow(Claims_List_APARKER)>40){
  DCROMEEN_Sample <- Claims_List_DCROMEEN %>%
      sample_frac(.025)
  }
DCROMEEN_Sample <- Claims_List_DCROMEEN %>%
  sample_frac(.25)

createSheet(wb,'DCROMEEN')
writeWorksheet(wb, DCROMEEN_Sample, "DCROMEEN")

## HZUNIGA
Claims_List_HZUNIGA <- Claims_query %>%
  mutate(DATE = User_Date) %>%
  filter(USERID == "HZUNIGA") 
if(nrow(Claims_List_HZUNIGA)>40){
  HZUNIGA_Sample <- Claims_List_HZUNIGA %>%
    sample_frac(.025)
}
HZUNIGA_Sample <- Claims_List_HZUNIGA %>%
  sample_frac(.25)

createSheet(wb,'HZUNIGA')
writeWorksheet(wb, HZUNIGA_Sample, "HZUNIGA")

## MDUONG
Claims_List_MDUONG <- Claims_query %>%
  mutate(DATE = User_Date) %>%
  filter(USERID == "MDUONG")
  if(nrow(Claims_List_APARKER)>4){
    MDUONG_Sample <- Claims_List_MDUONG %>%
      sample_frac(.025)
  }
MDUONG_Sample <- Claims_List_MDUONG %>%
  sample_frac(.25)

createSheet(wb,'MDUONG')
writeWorksheet(wb, MDUONG_Sample, "MDUONG")

## NSPARKS
Claims_List_NSPARKS <- Claims_query %>%
  mutate(DATE = User_Date) %>%
  filter(USERID == "NSPARKS") 
if(nrow(Claims_List_NSPARKS)>40){
    NSPARKS_Sample <- Claims_List_NSPARKS %>%
      sample_frac(.025)
  }
NSPARKS_Sample <- Claims_List_NSPARKS %>%
  sample_frac(.25)

createSheet(wb,'NSPARKS')
writeWorksheet(wb, NSPARKS_Sample, "NSPARKS")

## SAUSTIN
Claims_List_SAUSTIN <- Claims_query %>%
  mutate(DATE = User_Date) %>%
  filter(USERID == "SAUSTIN") 
if(nrow(Claims_List_SAUSTIN)>40){
  SAUSTIN_Sample <- Claims_List_SAUSTIN %>%
    sample_frac(.025)
}
SAUSTIN_Sample <- Claims_List_SAUSTIN %>%
  sample_frac(.25)

createSheet(wb,'SAUSTIN')
writeWorksheet(wb, SAUSTIN_Sample, "SAUSTIN")

## MLARSON
Claims_List_MLARSON <- Claims_query %>%
  mutate(DATE = User_Date) %>%
  filter(USERID == "MLARSON") 
if(nrow(Claims_List_APARKER)>40){
  MLARSON_Sample <- Claims_List_MLARSON %>%
    sample_frac(.025)
}
MLARSON_Sample <- Claims_List_MLARSON %>%
  sample_frac(.25)

createSheet(wb,'MLARSON')
writeWorksheet(wb, MLARSON_Sample, "MLARSON")

## FINAL
saveWorkbook(wb,"Tracy_Examiner_Audit.xlsx")

#write.xlsx(Claims_List_DCROMEEN, file = "XLConnext_Test.xlsx")
#wb <- loadWorkbook("XLConnect_Test.xlsx", create = TRUE)
#createSheet(wb, '')
#writeWorksheet(wb,Claims_List_APARKER, "Q")
#saveWorkbook(wb,"XLConnect_Test.xlsx")  



  


