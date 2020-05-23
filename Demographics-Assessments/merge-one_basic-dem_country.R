library(dplyr)
library(stringr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Core-Demographics")


##### Merges in the Country data with the other demographic data 

df_add <- read.csv("Country-of-Origin/Output/additional-demographics_2020-05-23.csv", stringsAsFactors=FALSE)
df_add <- df_add %>% mutate(student_id = as.character(student_id), year=as.character(year))

df_bd <- read.csv("Basics/Output/basic-demographics_2020-05-23.csv", stringsAsFactors = FALSE)
df_bd <- df_bd %>% mutate(student_id = as.character(student_id), year=as.character(year))


glimpse(df_add)
glimpse(df_bd)

df_final <- left_join(df_add, df_bd, by=c("student_id", "semester", "year"))

df_final <- distinct(df_final)

write.csv(df_final, 'improved_demog_01_26.csv', row.names=FALSE)
