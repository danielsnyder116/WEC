library(dplyr)
library(tidyr)
library(lubridate)

setwd("/Users/Daniel/Documents/wec")

df <- read.csv('finance_data.csv', stringsAsFactors = FALSE, header = FALSE)

#----Wranglin' time!--------------

#Getting rid of blank or NA rows
df <- df %>% filter(., V1!="")

#Dropping columns
df <- df %>% select(., -c("V2", "V4", "V6", "V8", "V10", "V12"))

#Renaming columns
names(df) <- c("student_id","full_name", "course", "total_cost", "amt_paid", "date_paid", "balance_due")

#16417 Unique Students
length(unique(df$student_id))

#Dplyr method to do the same thing as above
n_distinct(df$student_id)

#Getting rid of extra row
df <- df %>% slice(., -51018)

#Altering data types appropriately
glimpse(df)

df$date_paid <- mdy(df$date_paid)

#Parsing out full date to get year, month, and day of week - can see which days are busiest
df <- df %>% mutate(., year=year(df$date_paid), month=month(df$date_paid, label = TRUE), 
                       day=wday(df$date_paid, label=TRUE))

#Breaking up names into separate columns
df <- df %>% separate(., full_name, c("last_name", "first_name"), sep=",")

View(count(df, vars=`course`))
count(df, vars=`year`)


#Continue to clean data

#If course has '.', replace with NA
df <- df  %>% mutate(course=na_if(course, '.'))

#Current state of data sending to Renee
#write.csv(df, 'financial_data_2000_to_20200115.csv', row.names=FALSE)

