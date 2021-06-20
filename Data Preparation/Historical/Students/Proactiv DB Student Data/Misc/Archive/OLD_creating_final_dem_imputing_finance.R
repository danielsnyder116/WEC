library(dplyr)
library(stringr)
library(tidyr)

#Filling in class name data in student demographics using financial transactions data

setwd("C:/Users/602770/Downloads/volunteer/wec/Database")

df_students <- read.csv("Archive/2000-2019_student-demographics-and-assessment-data.csv",
                        stringsAsFactors = FALSE)

df_finance <- read.csv("Archive/2000-2020_COMPLETE_financial-data_with-payment-method.csv",
                       stringsAsFactors = FALSE)


#Stopping data at 2019 to line up with demographics data
df_finance <- df_finance %>% filter(year <= 2019 & year >= 2000)

#Resaving final correct version ugh
#write.csv(df_finance, "../Database/2000-2019_complete-financial-data_with-payment-method.csv", row.names = FALSE)

#Honing in on relevant columns and getting rid of payments made for materials rather than classes
df_finance <- df_finance %>% select(student_id, last_name, first_name, year, month, course)
df_finance <- df_finance %>% filter(!str_detect(course, pattern="Books"))

#Converting month to semester based on history of term dates
#Could also use mutate(semester=ifelse(), but case_when is easier to read)
df_finance <- df_finance %>% mutate(semester=case_when(str_detect(month, pattern = "Jan|Feb|Mar") ~ "WINTER",
                                                       str_detect(month, pattern = "Apr|May|Jun") ~ "SPRING",
                                                       str_detect(month, pattern = "Jul|Aug")     ~ "SUMMER", 
                                                       str_detect(month, pattern = "Sep|Oct|Nov") ~ "FALL"))
#Getting rid of unnecessary column
df_finance <- df_finance %>% select(-month)
nrow(df_finance)


#Getting rid of multiple entries
df_finance <- distinct(df_finance)
nrow(df_finance)


#Merging these brings in more data as there was some class info. for students
# missing in df_students (I think specifically conversation practice classes and special classes)
df_combined <- full_join(df_students, df_finance, by=c("student_id", "last_name", "first_name", "semester", "year"))
nrow(df_combined)


df_combined <- distinct(df_combined)
nrow(df_combined)

#Filling in missing data with present data
#For every row, if the class name is NA and the course name is present
# we impute course name to class name
for (i in 1:nrow(df_combined)){
  
  if (is.na(df_combined$class_name[i]) & !is.na(df_combined$course[i])) {
    df_combined$class_name[i] <- df_combined$course[i]
    
  }
}

#Getting rid of extra column now that imputation is finished
df_combined <- df_combined %>% select(-course)


#Looking at number of NAs, not bad
View(count(df_combined, vars=class_name))


#Get rid of duplicates based on semester, year, and class name
#Keep all is not like keep=True from python but refers to the other variables of the 
# dataframe that have not been used in determining distinct rows
df_combined <- df_combined %>% distinct(student_id, semester, year, class_name, .keep_all = TRUE)

#Get rid of cases where we don't have information on semester
df_combined <- df_combined %>% filter(!is.na(semester))

#For cases where one row of data has demographic information for same student as another row missing it,
# we write a simple function to fill that data in.

#Quicker way to create blank dataframe, get first row and lag until all columns are NA
df_blank <- data.frame(slice(df_combined, 1)) %>% lag(19)

#For each unique student_id, we filter the dataframe to only that student and use 
# fill() to share any data from other rows (age, gender, zip code, etc.)
#We save this to a temporary dataframe and then bind with the blank dataframe
#This helps us deal with having to subset the data and then reincorporate it to the dataframe
for (i in unique(df_combined$student_id)) {
  
 df_temp <- df_combined %>% filter(student_id == i) %>% fill(last_name:country)
 df_blank <- bind_rows(df_blank, df_temp)

}


df_final <- df_blank %>% slice(-1) %>% distinct()

#write.csv(df_final, "../Database/2000-2019_student-demographics-and-assessment-data_v2.csv", row.names=FALSE)



glimpse(df_final)

##### SUMMARY INFORMATION #####

#Total Number of Students (Not Unique)
#51,057         #nrow(df_final)

#Total Number of Unique Students
#16,466         #length(unique(df_final$student_id))
