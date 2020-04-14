library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Database")


df_demographics <- read.csv("2000-2019_student-demographics.csv", stringsAsFactors = FALSE)
df_finance <- read.csv("2000-2020-01-15_financial-data.csv", stringsAsFactors = FALSE)
df_assessment <- read.csv("2015-2019_student-assessments.csv", stringsAsFactors = FALSE)


#Due to the nature of the data, it is best to keep the finances separate as the merge
#leads to a less useful conglomeration of data.

## GETTING RID OF DUPLICATES IN FINANCE DATA
nrow(df_finance)
nrow(distinct(df_finance))

df_finance <- distinct(df_finance)

write.csv(df_finance,"2000-2020-01-15_financial-data.csv" )


################ DEMOGRAPHIC/ASSESSMENT DATA ################ 
glimpse(df_demographics)
glimpse(df_assessment)

#Some minor processing that I missed in previous work - will help with merges
df_demographics <- df_demographics %>% mutate(last_name = str_squish(last_name),
                                              first_name = str_squish(first_name))

#Just gonna get rid of the child under 22 column, all "No"
count(df_demographics, vars=child_under_22)
df_demographics <- df_demographics %>% select(-child_under_22)

df_demographics <- df_demographics %>% mutate(employment_status=na_if(employment_status, ""),
                                              ethnicity=na_if(ethnicity, ""))

df_assessment <- df_assessment %>% mutate(last_name = str_squish(last_name),
                                          first_name = str_squish(first_name))

#Doing a full join works
df_student <- full_join(df_demographics, df_assessment, by=c("student_id", "semester", "year",
                                                          "last_name", "first_name"))

nrow(df_student)
nrow(distinct(df_student))

#Getting rid of the couple of duplicate rows
df_student <- distinct(df_student)

#write.csv(df_student, "2000-2019_student-demographics-and-assessment-data.csv", row.names=FALSE)



########## EXTRA CODE ##########
# 
# df_14 <- df_temp %>% filter(year > 2014)
# 
# View(count(df_14, vars=final_exam_score))
# View(count(df_temp %>% filter(year==2019), vars=country))
# View(count(df_temp %>% filter(year==2013), vars=country))

