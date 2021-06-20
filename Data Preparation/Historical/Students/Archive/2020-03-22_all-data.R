library(dplyr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Database")


df_demographics <- read.csv("2000-2010_student-demographics.csv", stringsAsFactors = FALSE)
df_finance <- read.csv("2000-2020-01-15_financial-data.csv", stringsAsFactors = FALSE)
df_assessment <- read.csv("2015-2019_student-assessments.csv", stringsAsFactors = FALSE)

glimpse(df_demographics)
glimpse(df_assessment)

#Some minor processing that I missed in previous work - will help with merges

df_demographics <- df_demographics %>% mutate(last_name = str_squish(last_name),
                                              first_name = str_squish(first_name))

df_assessment <- df_assessment %>% mutate(last_name = str_squish(last_name),
                                          first_name = str_squish(first_name))

#Not having much luck with this
df_temp <- full_join(df_demographics, df_assessment, by=c("student_id", "semester", "year",
                                                          "last_name", "first_name"))

View(df_temp %>% filter(year > 2014))


View(count(df_temp %>% filter(year==2019), vars=country))
View(count(df_temp %>% filter(year==2013), vars=country))

