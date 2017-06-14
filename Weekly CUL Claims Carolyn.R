## Load Packages
library(tidyverse)
library(RODBC)
library(stringr)
library(lubridate)
library(scales)
library(data.table)

## database name goes here
dsn.name <- "LPAR01"

## username used to log in
user.name <- "CCO"

## password used with above username
pw.name <-"carolyn2"

## define connection variable here
con1 <- odbcConnect(dsn=dsn.name, uid = user.name, pwd = pw.name)


## pulls policies with bill ctrl 'CUL' and 'P' claim status
query_CUL1 <- sqlQuery(con1,"SELECT *
                        FROM (SELECT T1.PSNO01 AS COMPANY, T1.PSNO02 AS BLOCK, T1.PSNO03 AS POLICY, T1.PSCD02 AS STATUS, 
                        T1.PSCD04 AS PLAN, T2.PSNO05 AS FORM, T1.PSNM06 AS INSUREDLAST, T1.PSNM07 AS INSUREDFIRST, T1.PSC#01 AS BILLCTRL, 
                        T3.CLNO04 AS CLAIMANT, T2.PSNM01 AS PATIENTLAST, T2.PSNM02 AS PATIENTFIRST, T3.CLNO06 AS CLAIMNO, T3.CLAM07 AS AMTPD, 
                        (T3.CLCC09*1000000+CLYY09*10000+CLMM09*100+CLDD09) AS PAIDDATE
                        FROM LOADLIB.PSLMPOL1 AS T1 INNER JOIN LOADLIB.PSPMRDR AS T2 ON (T1.PSNO02 = T2.PSNO02) AND (T1.PSNO03 = T2.PSNO03)
                        INNER JOIN LOADLIB.CLPTLIN AS T3 ON (T2.PSNO02 = T3.CLNO02) AND (T2.PSNO03 = T3.CLNO03) AND (T2.PSNO04 = T3.CLNO04)
                        WHERE T1.PSC#01 = 'CUL' AND T3.CLCD02 = 'P' AND T3.CLNO06 NOT LIKE ('%-%')) AS G1
                        WHERE G1.PAIDDATE >= 20170401
                        ")

query_CUL2 <- sqlQuery(con1,"SELECT *
                        FROM (SELECT T1.PSNO01 AS COMPANY, T1.PSNO02 AS BLOCK, T1.PSNO03 AS POLICY, T1.PSCD02 AS STATUS, 
                        T1.PSCD04 AS PLAN, T2.PSNO05 AS FORM, T1.PSNM06 AS INSUREDLAST, T1.PSNM07 AS INSUREDFIRST, T1.PSC#01 AS BILLCTRL, 
                       T3.CLNO04 AS CLAIMANT, T2.PSNM01 AS PATIENTLAST, T2.PSNM02 AS PATIENTFIRST, T3.CLNO06 AS CLAIMNO, T3.CLAM07 AS AMTPD, 
                        (T3.CLCC09*1000000+CLYY09*10000+CLMM09*100+CLDD09) AS PAIDDATE
                       FROM LOADLIB.PSLMPOL1 AS T1 INNER JOIN LOADLIB.PSPMRDR AS T2 ON (T1.PSNO02 = T2.PSNO02) AND (T1.PSNO03 = T2.PSNO03)
                       INNER JOIN INSDLIB.MLPTLIN AS T3 ON (T2.PSNO02 = T3.CLNO02) AND (T2.PSNO03 = T3.CLNO03) AND (T2.PSNO04 = T3.CLNO04)
                       WHERE T1.PSC#01 = 'CUL' AND T3.CLCD02 = 'P' AND T3.CLNO06 NOT LIKE ('%-%')) AS G1
                       WHERE G1.PAIDDATE >= 20170401
                       ")

CUL_Claims <- query_CUL1 %>%
  bind_rows(query_CUL2) %>%
  mutate(Paid_Date = ymd(PAIDDATE), Claim_Amount = dollar_format()(AMTPD)) %>%
  select(COMPANY, BLOCK, POLICY, STATUS, PLAN, FORM, INSUREDLAST, INSUREDFIRST, BILLCTRL, CLAIMANT, PATIENTLAST, PATIENTFIRST, CLAIMNO, Claim_Amount,
         Paid_Date)
 
 




  

