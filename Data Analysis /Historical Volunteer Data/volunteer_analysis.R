library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#====================================
#           Analysis Part 
#===================================

setwd("/Users/Daniel/Desktop/Data/Volunteer Historical Data/output")

df_history <- read.csv("All Volunteers (Teachers, Tutors, Librarians, Special) from 2006 to Fall 2020_1212.csv", stringsAsFactors = FALSE)
df_current <- read.csv("All WEC Volunteers Winter 2021.csv", stringsAsFactors = FALSE)

df <- bind_rows(df_history, df_current)


#Count of volunteers helping out the most over all years - SIMPLE
df_all_time <- df %>% filter(year >= 2016) %>%
                  count(name) %>% arrange(., desc(n))

#Adds column (equivalent of using mutate)
#Filter to see who has volunteered most over all years 
#and who is volunteering in the specified YEAR or SEMESTER
#df_filter <- df %>% add_count(name) %>% filter(year == 2020) %>% arrange(., desc(n))

df_filter <- df %>% filter(year >= 2016) %>% 
             add_count(name) %>% arrange(., desc(n)) %>%
             rename(number_classes_tutors_taught=n) %>%
             mutate(normal_name = str_to_title(name))

# df_consec <- distinct(df, name, semester, year) %>% add_count(name) %>% arrange(., desc(n)) %>%
#   rename(number_classes_tutors_taught=n) %>%
#   mutate(normal_name = str_to_title(name))




