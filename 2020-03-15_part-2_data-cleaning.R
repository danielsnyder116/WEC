library(stringr)
library(dplyr)
library(tidyr)
library(readr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Processed")

df <- read.csv("2015-2019_intermediate_educational-performance-data.csv", stringsAsFactors = FALSE)

nrow(df)

###################### Cleaning up Columns ###########################

#Student ID
df <- df %>% mutate(student_id = str_replace_all(student_id, pattern = "ID Number: ", replacement = ""))

#Student Name
df <- df %>% mutate(student_name = str_replace_all(student_name, pattern="Student's name: ", replacement = ""))
df <- df %>% separate(student_name, c("last_name", "first_name"), sep=",")

#Semester
df <- df %>% mutate(semester = str_replace_all(semester, pattern="Semester: ", ""))
df <- df %>% separate(semester, c("semester", "year"), sep=" ", extra="merge")

#Class Name
df <- df %>% mutate(class_name = str_replace_all(class_name, pattern="Class Name: ", replacement = ""))

# Exam Scores, Multiple Columns
df$final_exam_score <- parse_number(df$final_exam_score)
df$attendance <- parse_number(df$attendance)

df$participation <- parse_number(df$participation)
df$language_score <- parse_number(df$language_score)

df$total_eval_score <- parse_number(df$total_eval_score)



#Calculating the number of points given based on the final exam score (out of 100 score to 5 point scale)
df$exam_points <- df$total_eval_score - (df$attendance + df$participation + df$language_score)

#Reordering to make columns easier to read
df <- df %>% select(student_id, last_name, first_name, semester, year, class_name, final_exam_score,
                    exam_points, attendance, participation, language_score, total_eval_score)




View(count(df, vars=total_eval_score))

hist(df$total_eval_score)



















# # Failed attempt to write loop to parse numbers for all four columns
# #Kept getting an error involving is.character(x)
# columns <- list(final_exam_score, attendance, participation, language_score, total_eval_score)
# 
# for (column in columns) {
#   df <- parse_number(df$column)
# }
  
  
  
  
  
  

