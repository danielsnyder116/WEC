library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

setwd("C:/Users/602770/Downloads/volunteer/wec/Term-Dates/Raw")

df <- read.csv("2000-2020 Semester Start and End Dates.csv", header= FALSE, stringsAsFactors= FALSE)

glimpse(df)
names(df)

df <- df %>% select(V5, V7, V9, V11) %>% drop_na()

rownames(df) <- 1:nrow(df)

colnames(df) <- c("year", "start_date", "end_date", "semester")

#Getting rid of extra year information by replacing numbers with nothing
df <- df %>% mutate(semester=str_replace_all(semester, pattern="[:digit:]", replacement = ""))

#Formatting dates
df <- df %>% mutate(start_date=mdy(start_date),
                    end_date=mdy(end_date))

#Adding in day of the week
df <- df %>% mutate(start_day=wday(start_date, label=TRUE), end_day=wday(end_date, label=TRUE))

df <- df %>% select(c(semester, year, start_date, end_date, start_day, end_day))

#Arranging by most recent year, descending
df <- arrange(df, desc(year))


#write.csv(df, "../Output/2000-2020_term-start-and-end-dates.csv", row.names = FALSE)
