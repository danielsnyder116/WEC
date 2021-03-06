library(dplyr)
library(stringr)
library(tidyr)

#==================================================
# ONCE AND DONE TO COMBINE ALL VOLUNTEER DATA AND 
# FILL MISSING EMAILS PRESENT ELSEWHERE IN THE DATA
#==================================================

setwd("/Users/Daniel/Desktop/Data/cleaned")

#Combining all the different volunteer datasets 
# Teachers, Tutors, Lang Lab, Librarians, Special Volunteers (Job Coach, Office/Registration Help)
files <- list.files()

for (file in files) {
  if (file == files[1]) {
    
    df_base = read.csv(file, stringsAsFactors = FALSE)
    df_base <- df_base %>% mutate(tutor_type = NA_character_, class = NA_character_)
  } 
  else {
    df = read.csv(file, stringsAsFactors = FALSE)
    df_base = bind_rows(df_base, df)
  }} 

df <- df_base %>% mutate(category = str_to_title(category))

#Code that fills in missing emails where they are present elsewhere
#============================
#Need to replace NAs for loop
df <- df %>% mutate(across(everything(), ~replace_na(., " ")) )

#If the email is missing, get that person's name 
missing_names <- df %>% filter(email == " ")
present_names <- df %>% filter(email != " ") 

#Distinct names with emails
unique_present_names <- df %>% filter(email != " ") %>% distinct()

#For each missing email
for (i in 1:nrow(missing_names)) {
  
  #Check name of missing email with all distinct names
  for (j in 1:nrow(unique_present_names)) {
    
    if (missing_names$name[i] == unique_present_names$name[j]) {
      missing_names$email[i] = unique_present_names$email[j]
    }
    
    else{} }}

#nrow( df %>% filter(email == " "))
#nrow( missing_names %>% filter(email == " "))

#======== RESULTS ========#
#Using the loop filled in 55% of emails, yay! 
#Missingness down from 1325 to 731 (731 / 13582) 5.4%

#Concatenate / bind rows back together
df_filled <- bind_rows(present_names, missing_names)

#Put NAs back in
df_filled <- df_filled %>% mutate(across(everything(), ~na_if(., " "))) %>% arrange(., semester, year)

write.csv(df_filled, '../output/All Volunteers (Teachers, Tutors, Librarians, Special) from 2006 to Fall 2020_1212.csv', row.names=FALSE)

