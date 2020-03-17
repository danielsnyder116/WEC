library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

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


#Exam scores, Multiple columns NEW WAY
#Columns you want to mutate (must be list of strings) and function(s)
df <- df %>% mutate_at(c("final_exam_score", "participation", "attendance",
                         "language_score", "total_eval_score"), funs(parse_number(.)))


#Filling in all NAs with zero since that is the effective score
#Mutate all applies the function to all columns rather than having to specify
#Funs stands for function and is a way to call a different function on the data

#May want to wait though as there may be lack of reporting, so NA isn't 
# necessarily a zero
#df <- mutate_all(df, funs(replace_na(., 0)))

#Calculating the number of points given based on the final exam score (out of 100 score to 5 point scale)
df$exam_points <- df$total_eval_score - (df$attendance + df$participation + df$language_score)


#Reordering to make columns easier to read
df <- df %>% select(student_id, last_name, first_name, semester, year, class_name, final_exam_score,
                    exam_points, attendance, participation, language_score, total_eval_score)

glimpse(df)

#Adding "2017 2" and "2018 2" data to 2017 and 2018 respectively
#They were just special classes held for different training during same semester
df <- df %>% mutate(year = str_trunc(year, width=4, ellipsis = ""))


#Getting rid of a few rows of 90% NA data and thus unusable
df <- df %>% filter(!is.na(year))

###################### Visualizing Data (EDA) ###########################

#Attendance distribution from 2015-2019 overall
p <- ggplot(df, aes(x=attendance)) + geom_histogram(bins=5, binwidth = .5)
p

#Attendance grouped by Semester
p_color <- ggplot(df, aes(x=attendance, fill=semester)) + 
                  geom_histogram(bins=5, binwidth = .5) +
                  facet_grid(.~ semester) + theme_light()
p_color


#By Year
ggplot(df, aes(x=attendance, fill=year)) + 
  geom_histogram(bins=5, binwidth = .5) +
  facet_grid(.~ semester) + theme_light() + facet_grid(.~year)





count(df, vars=year)












# # Failed attempt to write loop to parse numbers for all four columns
# #Kept getting an error involving is.character(x)
# columns <- list(final_exam_score, attendance, participation, language_score, total_eval_score)
# 
# for (column in columns) {
#   df <- parse_number(df$column)
# }
  
# Exam Scores, Multiple Columns
#Old way, see dplyr way below
# df$final_exam_score <- parse_number(df$final_exam_score)
# df$attendance <- parse_number(df$attendance)
# df$participation <- parse_number(df$participation)
# df$language_score <- parse_number(df$language_score)
# df$total_eval_score <- parse_number(df$total_eval_score)
  
  

