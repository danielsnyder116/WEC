library(dplyr)
library(stringr)
library(tidyr)

setwd("/Users/Daniel/Desktop/Data/other")
files <- list.files(pattern = '*.csv')

file <- files[1]

df <- read.csv(file, stringsAsFactors = FALSE)

names(df) <- c('category', 'day', 'name', 'email', 'phone', 'extra', 'semester', 'year')

#Replacing blank strings with NA
df <- df %>% mutate(across(everything(), ~na_if(., "")))

#For multiple filter conditions, just use comma or &
#View(df %>% filter(!is.na(day) & is.na(name_1))) #which(!is.na(df$day) & is.na(df$name_1))

#Get rid of blank rows - all valid rows have volunteer name so
# we just get rid of rows that don't have volunteer name
df <- df %>% filter(!is.na(name))

#Filling in semester and year data
df <- df %>% fill(c(category, day, semester, year), .direction ='down')

#Separate out 2019 & 2020 data to combine two name columns into one
df_19 <- df %>% filter(year >= 2019)
df_rest <- df %>% filter(year < 2019) %>% select(-extra)

#Combining two name columns into one
df_19 <- df_19 %>% unite("name", name:email, sep=" ", remove=TRUE)
df_19 <- df_19 %>% rename(email=phone, phone=extra)

#Bringing it back together
df <- bind_rows(df_rest, df_19)

#Use str_detect, if punctuation at very beginning of col3 (so *), add YES
#To give idea of percentage new teacher vs returner
df <- df %>% mutate(new_volunteer = case_when(str_detect(name, pattern = "\\*") ~ "Yes", TRUE ~ 'No'), 
                    .before=semester, name = str_replace_all(name, pattern = "\\*", replacement = ""))


#Make sure no extra spaces - using new across method
df <- df %>% mutate(across(everything(), ~str_squish(.)))

#Format names
#df <- df %>% mutate(across(c(name, day), ~str_to_upper(.)))

df <- df %>% mutate(name = str_to_upper(name), 
                    day = str_remove(str_to_title(day), pattern='Am|Pm'),
                    email = str_to_lower(email))

#Dropping duplicate rows
nrow(df)

#How to view duplicate rows
View(df %>% add_count(name, year, semester, day) %>% filter (n>1))

#Summer 2009 had weird way of listing people multiple times which is why
#there were a few dozen duplicates - other than that, all unique.
df <- distinct(df)

write.csv(df, "All WEC Language Lab Volunteers 2006 to Fall 2020.csv", row.names=FALSE)
N