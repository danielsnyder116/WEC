#-------------------------------
# ACADEMIC REPORTING: STUDENTS
#-------------------------------

#-------------------------------
#Establish a baseline for the % of students who score 75% or higher
# on the final exam (out of those who attend 70% or more classes)
#-------------------------------

library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)
library(tibble)
library(glue)
library(ggplot2)

#Identify data we have on final exam data

setwd("/Users/Daniel/Desktop/WEC/Data/Current (2020 - Present)/Students/")

#Bring in Attendance Data and make one file
files <- as_tibble(list.files("Attendance", recursive = TRUE)) %>% rename(file_path = value) %>%
                mutate(base_path = "/Users/Daniel/Desktop/WEC/Data/Current (2020 - Present)/Students/Attendance/",
                       full_path = paste0(base_path, file_path))


for (path in files$full_path){
  
  #GATHERING OTHER DETAILS FOUND IN FILE NAME AND PATH
  #-------------------------------------------------------
  class_time <- str_extract(path, pattern = "Copy of .*\\-") %>% 
                str_remove("Copy of") %>% str_remove(" -") %>% str_squish()
  
  file_class <- str_split(class_time, " ")[[1]][1]
  file_time <- str_split(class_time, " ")[[1]][2]
  
  #Getting Semester and Year Info from File Name
  semester_year <- str_extract(path, pattern = "FALL \\d\\d\\d\\d|WINTER \\d\\d\\d\\d|
                                                |SUMMER \\d\\d\\d\\d|SPRING \\d\\d\\d\\d|
                                                |\\d\\d\\d\\d FALL|\\d\\d\\d\\d WINTER|
                                                |\\d\\d\\d\\d SUMMER|\\d\\d\\d\\d SPRING")
  
  file_semester <- str_extract(semester_year, pattern = "FALL|WINTER|SPRING|SUMMER")
  file_year <- str_extract(semester_year, pattern = "\\d\\d\\d\\d")
  
  
  if(path == files$full_path[1]) {
    
    #path <- files$full_path[10]
    
   
    
    df_final <- read_excel(path) %>% select(1,2,4,5) %>% slice(-c(1:2)) %>% mutate(semester = file_semester,
                                                                year = file_year,
                                                                class = file_class,
                                                                time = file_time)
    
    names(df_final) <- c("last_name", "first_name", "email", "att_percent", "semester", "year", "class", "time")
    
    print(df_final)
  }
  
  else {
    
    df_new <- read_excel(path) %>% select(1,2,4,5) %>% slice(-c(1:2)) %>% mutate(semester = file_semester,
                                                              year = file_year,
                                                              class = file_class,
                                                              time = file_time)
    
    names(df_new) <- c("last_name", "first_name", "email", "att_percent", "semester", "year", "class", "time")
    
    print(df_new)
    
    df_final <- bind_rows(df_final, df_new)
    
    
  }
}

#Cleaning
df <- df_final %>% drop_na(c("last_name")) %>% mutate(att_percent = as.numeric(att_percent)) %>%
  
              #Ensuring all percentages are in the same format by converting percents back to decimals
               mutate(att_percent = case_when(att_percent > 1 ~ (att_percent/100),
                                            TRUE ~ att_percent))
                    



#Filter down to students who attend 70% or more of classes
df_thresh <- df %>% filter(att_percent >= .700)


#A little more than 1/4 of students attend 70% or more
nrow(df_thresh) / nrow(df)


#Bring in student performance data

#Get percentage of students who score 75 or higher on exam



