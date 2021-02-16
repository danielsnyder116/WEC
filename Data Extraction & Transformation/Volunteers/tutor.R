library(dplyr)
library(stringr)
library(tidyr)

setwd("/Users/Daniel/Desktop/Data/other")
files <- list.files(pattern = '*.csv')

file <- files[4]

df <- read.csv(file, stringsAsFactors = FALSE)

names(df) <- c('category', 'name', 'email',  'phone', 'tutor_type', 'extra','extra_2', 'semester', 'year')

#Making sure season sorts in order since issue with name col in two fields necessitates bind_rows
#and data gets out of order at the end
#Replacing blank strings with NA
df <- df %>% mutate(across(everything(), ~na_if(., "")),
                    semester=factor(semester, levels = c("WINTER", "SPRING", "SUMMER", "SPRING & SUMMER", "FALL")))

#For multiple filter conditions, just use comma or &
#View(df %>% filter(!is.na(day) & is.na(name_1))) #which(!is.na(df$day) & is.na(df$name_1))

#Get rid of blank rows - all valid rows have volunteer name so
# we just get rid of rows that don't have volunteer name
df <- df %>% filter(!is.na(name))

#Filling in semester and year data
df <- df %>% fill(c(category, semester, year, tutor_type), .direction ='down')

#Separate out 2019 & 2020 data to combine two name columns into one
df_19 <- df %>% filter(year >= 2019 & semester != 'SPRING & SUMMER') %>% select (-extra_2)
df_rest <- df %>% filter(year < 2019 | semester == 'SPRING & SUMMER') %>% select(-c(extra, extra_2))

#Combining two name columns into one
df_19 <- df_19 %>% unite("name", name:email, sep=" ", remove=TRUE)
df_19 <- df_19 %>% rename(email=phone, phone=tutor_type, tutor_type=extra)
df_19 <- df_19 %>% mutate(tutor_type='both')


#Resolving new data for 2020
df_rest <- df_rest %>% mutate(tutor_type = replace_na(tutor_type, 'both'), .before=semester)

#Bringing it back together
df <- bind_rows(df_rest, df_19) %>% arrange(., year, semester)

#Use str_detect, if punctuation at very beginning of col3 (so *), add YES
#To give idea of percentage new teacher vs returner
df <- df %>% mutate(new_volunteer = case_when(str_detect(name, pattern = "\\*") ~ "Yes", 
                                              str_detect(phone, pattern = "Yes") ~ "No",
                                              str_detect(phone, pattern = "NEW") ~ "Yes",
                                              TRUE ~ 'No'),  .before=semester, 
                    name = str_replace_all(name, pattern = "\\*", replacement = ""),
                    phone = str_replace_all(phone, regex(pattern = "\\(|\\)|Yes|NEW", ignore_case = TRUE), replacement = NA_character_))


#Make sure no extra spaces - using new across method
df <- df %>% mutate(across(everything(), ~str_squish(.)))

#Format names
#df <- df %>% mutate(across(c(name, day), ~str_to_upper(.)))

df <- df %>% mutate(name = str_to_upper(name), 
                    #day = str_remove(str_to_title(day), pattern='Am|Pm'),
                    email = str_to_lower(email))

#Dropping duplicate rows
nrow(df)

#How to view duplicate rows
View(df %>% add_count(name, year, semester) %>% filter (n>1))

#Summer 2009 had weird way of listing people multiple times which is why
#there were a few dozen duplicates - other than that, all unique.
#df <- distinct(df)

write.csv(df, "../All WEC Tutor Volunteers 2006 to Fall 2020.csv", row.names=FALSE)
