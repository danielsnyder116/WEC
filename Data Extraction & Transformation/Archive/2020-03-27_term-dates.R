library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

#setwd("C:/Users/602770/Downloads/volunteer/wec/Term-Dates/Raw")
setwd("/Users/Daniel/Documents/volunteer/data")

df <- read.csv("2000-2020 Semester Start and End Dates.csv", header=FALSE, stringsAsFactors = FALSE)

#V7 - year column is messed up, will parse from actual dates further below
#Using na_if allows us to fill in blank spaces with NA so we can then use drop_na and cut 'em out.
df <- df %>% select(V9, V11, V13 ) %>% na_if("") %>% drop_na()

rownames(df) <- 1:nrow(df)

colnames(df) <- c("start_date", "end_date", "semester")

#Filters out data for 2 and 3 semesters (Fall 2017 2, Winter 2018 3)
#Not concerned about enrollment times for this right now.
df <- df %>% filter(!str_detect(semester, pattern = " 3|7 2|8 2|9 2"))

#Getting rid of extra year information by replacing numbers with nothing
df <- df %>% mutate(semester=str_replace_all(semester, pattern="[:digit:]", replacement = ""))


#Formatting dates
df <- df %>% mutate(start_date=mdy(start_date),
                    end_date=mdy(end_date))

#Adding in day of the week
df <- df %>% mutate(start_day=wday(start_date, label=TRUE),
                   end_day=wday(end_date, label=TRUE))


#Sorts values by descending start_date - equivalent to Python: df.sort_values(by="year", ascending=False)
df <- df %>% arrange(desc(start_date))

df <- df %>% mutate(year=year(start_date))

df <- df %>% select(semester, year, start_date, start_day, end_date, end_day)

write.csv(df, "2000-2020_semester-start-end-dates.csv", row.names=FALSE)

