library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

setwd("C:/Users/602770/Downloads/volunteer/wec/Term-Dates/Raw")

df <- read.csv("2000-2020 Semester Start and End Dates.csv", header=FALSE, stringsAsFactors = FALSE)

glimpse(df)
names(df)

df <- df %>% select(V7, V9, V11, V13 ) %>% drop_na()

rownames(df) <- 1:nrow(df)

colnames(df) <- c("year", "start_date", "end_date", "semester")

#Getting rid of extra year information by replacing numbers with nothing
df <- df %>% mutate(semester=str_replace_all(semester, pattern="[:digit:]", replacement = ""))


#Formatting dates
df <- df %>% mutate(start_date=mdy(start_date),
                    end_date=mdy(end_date))

#Adding in day of the week
df <- df %>% mutate(start_day=wday(start_date, label=TRUE),
                   end_day=wday(end_date, label=TRUE))
