## Script to automate weekly Spokane calls report

## read in csv file from wd

spokanecalls <- read_csv("spokanecallsR.csv")

## Create date, day of week, and department columns

calldate <- mdy(spokanecalls$EventDate)

calldayofweek <- weekdays(calldate)

calldayofweek2 <- factor(calldayofweek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

dep <- spokanecalls$ExtnName

dep2 <- sub("Jeanne Jarrett|Jennifer Gray|Cheryl VanStone|Julie Singer", "MGMT", dep)
dep3 <- sub("Renee Balcom|Sherrie Cramb|Pam Hodnett|Carol Snyder|April Benson", "CAD", dep2)
dep4 <- sub("Laura Barragan|Melissa Sarner|Laura Gil|Sheana Burden", "MKTG", dep3)
dep5 <- sub("Madlyn Bothmer|Lindsey Dustin|Alyssa McLean|Carnette Ball|Peggy Willis|Stephanie Cheyney", "NBS", dep4)

## add created columns, select needed columns, filter out "Intercom", filter out bad userIDs, filter for dates

spokanecalls2 <- spokanecalls %>% mutate(Date = calldate, Weekday = calldayofweek2, Department = dep5) %>% 
  select(ExtnName, CallType, Weekday, Date, Department) %>% 
  filter(CallType != "Intercom") %>% 
  filter(ExtnName != "open", ExtnName != "Natalie Ledgerwood", ExtnName != "Lindsay Teter", ExtnName != "Christal Strimple",
         ExtnName != "Laura Barragan", ExtnName != "Renee McNally") %>%
  filter(Date >= "2017-5-15" & Date <= "2017-5-19")

## Create Plots
  ggplot(spokanecalls2, aes(x = Weekday, fill = CallType)) + 
    geom_bar() + 
    labs(title = "Calls Per Weekday by Call Type", y = "Calls Taken/Made")
  
  ggplot(spokanecalls2, aes(x = Department, fill = CallType)) + 
    geom_bar() + 
    labs(title = "Calls Per Department by Call Type", y = "Calls Taken/Made")
  
  ggplot(spokanecalls2, aes(x = ExtnName, fill = CallType)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
    labs(title = "Calls Per Employee by Call Type", y = "Calls Taken/Made", x = "Employee")

## Create summary tables

summarytable <- group_by(spokanecalls2, CallType, Weekday) %>%
  summarise(n())

summarytablefinal <- spread(summarytable, CallType, "n()")

summarytabledep <- group_by(spokanecalls2, CallType, Department, ExtnName) %>%
  summarise(n())

summarytabledepfinal <- spread(summarytabledep, CallType, "n()")
