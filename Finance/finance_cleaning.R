library(dplyr)
library(tidyr)
library(lubridate)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Finance-Payments/Finance/Raw")

df <- read.csv('finance_data.csv', stringsAsFactors = FALSE, header = FALSE)

#----Wranglin' time!--------------

#Getting rid of blank or NA rows
df <- df %>% filter(V1!="")

#Dropping columns
df <- df %>% select(-c("V2", "V4", "V6", "V8", "V10", "V12", "V14"))

#Renaming columns
names(df) <- c("student_id","full_name", "class_name", "total_cost", "amt_paid", "date_paid", "balance_due")

#16417 Unique Students
length(unique(df$student_id))

#Dplyr method to do the same thing as above
n_distinct(df$student_id)

#Getting rid of extra row
df <- df %>% slice(-51018)

#Altering data types appropriately
glimpse(df)

df$date_paid <- mdy(df$date_paid)

#Parsing out full date to get year, month, and day of week - can see which days are busiest
df <- df %>% mutate(year = year(df$date_paid), month=month(df$date_paid, label = TRUE), 
                       day = wday(df$date_paid, label=TRUE))

#Breaking up names into separate columns
df <- df %>% separate(full_name, c("last_name", "first_name"), sep=",")


#Continue to clean data

#If course has '.', replace with NA
df <- df  %>% mutate(class_name=na_if(class_name, '.'))


## GETTING RID OF DUPLICATES IN FINANCE DATA
nrow(df)
nrow(distinct(df))

df <- distinct(df)

#Ensuring no extra spaces
df <- df %>% mutate(first_name=str_squish(first_name))

#Ensuring no weird years
df <- df %>% filter(year >= 2000)

View(count(df, vars=class_name))
count(df, vars=`year`)


write.csv(df,"../Output/2000-2020-01-15_financial-data.csv", row.names = FALSE)


