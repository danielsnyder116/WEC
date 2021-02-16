library(dplyr)
library(tidyr)
library(stringr)

setwd("/Users/Daniel/Desktop/Data/Winter 2021 Volunteer/")

df <- read.csv("volunteers_winter_2021.csv", stringsAsFactors = FALSE)

#Renaming columns
names(df) <- c("category","time","class","day","email","first_name", "last_name", "phone", "new_volunteer", "club_name",
               "tutor_type", "semester", "year")

#Replacing blank strings with NA
df <- df %>% mutate(across(everything(), ~na_if(., "")))

#Getting rid of blank rows
df <- df %>% filter(!is.na(first_name))


#Filling in relevant information
df <- df %>% fill(c(category, time, class, day, semester, year), .direction="down")

#Making Name column uniform
df <- df %>% unite("name", first_name, last_name, sep=" ", remove=TRUE)

#Make sure no extra spaces - using new across method
df <- df %>% mutate(across(everything(), ~str_squish(.)))

df <- df %>% mutate(name = str_replace_all(name, pattern = " NA$", replacement = ""))


#Recoding new volunteer type to be consistent
df <- df %>% mutate(new_volunteer = case_when(str_detect(new_volunteer, "Returning|^Y") ~ "No",
                                              str_detect(new_volunteer, "NEW") ~ "Yes",
                                              TRUE ~ "Unknown"))

#Formatting some columns
df <- df %>% mutate(name = str_to_upper(name), 
                    day = str_to_title(day),
                    email = str_to_lower(email))


#How to view duplicate rows
#View(df %>% add_count(name, year, semester, class, day) %>% filter (n>1))

write.csv(df, "output/All WEC Volunteers Winter 2021.csv", row.names = FALSE)






