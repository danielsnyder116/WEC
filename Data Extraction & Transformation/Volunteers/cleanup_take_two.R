library(dplyr)
library(stringr)
library(tidyr)

setwd("/Users/Daniel/Desktop/volunteer-files/")

#LESSON LEARNED: Sometimes, rather than trying to clean a bunch of messy data sets from start to finish using code, 
# it can be much more efficient to do the basic binning of relevant information manually with human knowledge
# as to how things should be, and then take care of the rest automatically. There was just way too much 
# variation and unclear recording practices from a machine-readable standpoint to try to do it all with code.

df <- read.csv()
  

#Get rid of blank rows


#Dropping duplicate rows
df <- distinct(df)

#Replacing blank strings with NA
df <- df %>% mutate(col1 = na_if(col1, ""))

#Filling in class names to make data tidy
df <- df %>% fill(col1, .direction = "down")


#Filling in class day to make data tidy
df <- df %>% fill(col2, .direction = "down")

#Use str_detect, if punctuation at very beginning of col3 (so *), add YES
#To give idea of percentage new teacher vs returner
df <- df %>% mutate(new_volunteer = case_when(str_detect(teacher_name, pattern = "\\*") ~ "Yes",TRUE ~ 'No'),
                    teacher_name = str_replace_all(teacher_name, pattern = "\\*", replacement = ""))

#Need to get read of (TL)

#Make sure no extra spaces - using new across method
df <- df %>% mutate(across(everything(), ~str_trim(.)))
df <- df %>% mutate(across(everything(), ~str_squish(.)))


