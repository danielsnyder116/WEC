library(dplyr)
library(stringr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Demographics-Assessments")


df_dem <- read.csv("Core-Demographics/final-demographics_2020-05-23.csv", stringsAsFactors = FALSE)
df_assess <- read.csv("Assessments/Output/2015-2019_student-assessments.csv", stringsAsFactors = FALSE)



################ DEMOGRAPHIC/ASSESSMENT DATA ################ 
glimpse(df_dem)
glimpse(df_assess)


#Doing a full join works
df_student <- full_join(df_dem, df_assess, by=c("student_id", "semester", "year", "class_name"))

#No need for last name or first name from df_assess
df_student <- df_student %>% select(-last_name,-first_name )

nrow(df_student)
nrow(distinct(df_student))

#Getting rid of the couple of duplicate rows
df_student <- distinct(df_student)


write.csv(df_student, "final_demographics-and-assessment-data.csv", row.names=FALSE)


########## EXTRA CODE ##########
# 
# df_14 <- df_temp %>% filter(year > 2014)
# 
# View(count(df_14, vars=final_exam_score))
# View(count(df_temp %>% filter(year==2019), vars=country))
# View(count(df_temp %>% filter(year==2013), vars=country))

