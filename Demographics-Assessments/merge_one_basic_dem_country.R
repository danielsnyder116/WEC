library(dplyr)
library(stringr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Core-Demographics/Country-of-Origin/Data/Processed")


##### Merges in the Country data with the other demographic data 

df_base <- read.csv("basic_demographics_2000_2019_0110.csv", stringsAsFactors=FALSE)

df_base <- df_base %>% mutate(., student_id= as.character(student_id), year=as.character(year))

df_base <- df_base %>% select(., 1:12)

dim(df_base)
dim(df_country)

glimpse(df_base)

df_final <- left_join(df_base, df_country, by="student_id")

write.csv(df_final, 'improved_demog_01_26.csv', row.names=FALSE)
