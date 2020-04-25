library(dplyr)
library(stringr)
library(fuzzyjoin)

setwd("C:/Users/602770/Downloads/volunteer/wec/Database")

df_demog <- read.csv("2000-2019_student-demographics-and-assessment-data_v2.csv", stringsAsFactors = FALSE)
df_finance <- read.csv("2000-2019_complete-financial-data_with-payment-method.csv", stringsAsFactors = FALSE)


df_class <- read.csv("2000-2020_final-class-details_includes-dropped-transfers-totals.csv", stringsAsFactors = FALSE)

#Pairing down class
df_class <- df_class %>% select(class_id, class_name, semester, year, DAYS, TIME, simple_standard_class, complete_standard_class) %>%
                    mutate(semester=str_to_upper(semester))

#Capitalizing, getting rid of extra spaces, and replacing in-between spaces with dashes 
df_demog <- df_demog %>% mutate(class_name = str_replace_all(class_name, pattern=" - ", replacement=" "),
                                last_name = str_replace_all(last_name, pattern = " - |- |-  ", replacement = "-"))

#Dealing with weird spaces causing multiple dashes in a row
df_demog <- df_demog %>% mutate(class_name = str_squish(str_replace_all(str_to_upper(class_name), " ", "-")))
df_demog <- df_demog %>% mutate(class_name = str_replace_all(class_name, pattern="-\\/-|-\\/", replacement = "\\/"))


#Lol so to improve the merge we have to go back and fix mistakes in student class info and have that match the 
#non-standard names from df_classes ahhhhhhhhh.

#Changing classes marked with S for summer but have spring as semester
df_demog <- df_demog %>% mutate(semester = 
                                      case_when(semester == 'SPRING' & str_detect(class_name, pattern = "^SC") ~ "SUMMER",
                                                TRUE ~ semester))


#Improving demog details
#Since the demographics data doesn't have a unique class identifier, just the name, this leads to a lot of duplicate creation
#since a class like 1A might be offered 3-5 times during one semester, with no indicator in the old name.
df_better_dem <- left_join(df_demog, df_class, by=c("class_name", "semester", "year"))

#To deal with this, after merging, we drop "duplicate" rows by looking at 
# student_id, class_name, semester, and year - there's no way a student was taking multiple of the same class
# during one semester.


#Only lost two compared to previous 51,057 - nice! 
df_better_dem <- distinct(df_better_dem, student_id, class_name, semester, year, .keep_all = TRUE)


########## TASK 1: ADD CLASSES THAT HAPPENED THAT WERE NOT RECORDED ########## 
########## IN PROACTIVE BUT WERE FOUND IN STUDENT DEMOGRAPHIC DATA ########## 

#Focusing in on rows where there was no match and where df_demog had a class name to match
df_dem_check <- df_better_dem %>% filter(is.na(class_id) & !is.na(class_name)) %>% select(class_name, semester, year) %>%
                                  distinct(class_name, semester, year, .keep_all = TRUE)

#1076 rows long, so 1076 classes that didn't merge correctly - ew.

#After doing this for one semester of one year, I realized that this would be better to automate crikey
#Then I tried to automate it using fuzzy-matching - issue is, most of the differences are only 1 or 2 characters
# which leads to huge amounts of matches which actually complicates things more *sigh*


#### SUMMER 2000 ####
#It looks like there are some classes in the early years (2000-2010) that were not captured with class ids
# df_class, so we will create some new rows for df_class
df_class <- add_row(df_class, class_id = 200010, class_name = "ESL-1B", semester = 'SUMMER', year = 2000, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200020, class_name = "ESL-3", semester = 'SUMMER', year = 2000, simple_standard_class = "3" )

df_class <- add_row(df_class, class_id = 200030, class_name = "ESL-BASIC-I", semester = 'SUMMER', year = 2000, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200040, class_name = "ESL-2", semester = 'SUMMER', year = 2000, simple_standard_class = "2" )

df_class <- add_row(df_class, class_id = 200050, class_name = "ESL-1A", semester = 'SUMMER', year = 2000, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200060, class_name = "ESL-4", semester = 'SUMMER', year = 2000, simple_standard_class = "4")
df_class <- add_row(df_class, class_id = 200070, class_name = "ESL-5", semester = 'SUMMER',year = 2000, simple_standard_class = "5")


#### SUMMER 2000 ####
#### SUMMER 2000 ####
#### SUMMER 2000 ####
#### SUMMER 2000 ####
#### SUMMER 2000 ####
#### SUMMER 2000 ####
#### SUMMER 2000 ####
#### SUMMER 2000 ####
#### SUMMER 2000 ####















########## TASK 2: ALTER CLASS NAMES IN DEMOGRAPHICS DATA TO MATCH CLASS NAMES ########## 
########## IN DF_CLASS SO THERE ARE MORE MATCHES AND MORE CLARITY              ########## 


#This for loop iterates over the data that has merging issues from df_demog
#and performs renaming tasks for class_name for each year (and by semester when needed)
#This looks like there could be a better way to code it, but unfortunately since each year has
#different issues, it has be done one year at at time yippeeeeeee

years <- seq(2000, 2019)
  
for (year in years) {
  
  if (year == 2000 & semest) {
    
    #Oh boy lots of issues, weeeeeee 
    df_demog <- df_demog %>% mutate(class_name = 
                                      case_when(str_detect(class_name, pattern="") ~ ,
                                                str_detect(class_name, pattern="") ~ ,
                                                str_detect(class_name, pattern="") ~ ,
                                                str_detect(class_name, pattern="") ~ ,
                                                str_detect(class_name, pattern="") ~ ,))
  }
  else if (year == 2001) {}
  else if (year == 2003) {}
  else if (year == 2004) {}
  else if (year == 2005) {}
  else if (year == 2006) {}
  else if (year == 2007) {}
  else if (year == 2008) {}
  else if (year == 2009) {}
  else if (year == 2010) {}
  else if (year == 2011) {}
  else if (year == 2012) {}
  else if (year == 2013) {}
  else if (year == 2014) {}
  else if (year == 2015) {}
  else if (year == 2016) {}
  else if (year == 2016) {}
  else if (year == 2016) {}
  else if (year == 2016) {}
  
  else {
    
  }
} 
  

































### GRAVEYARD ###
# # years <- seq(2000, 2019)
# # semesters <- c("WINTER", "SPRING", "SUMMER", "FALL")
# 
# years <- c(2000, 2001, 2002)
# semesters <- c("SPRING", "SUMMER")
# 
# #For each year, for each semester,
# for (year_ in years) {
#   
#   for (semester_ in semesters) {
#     
#     if (year_ == 2000 & semester_=="SPRING") {
#       
#       #Only look at specific year and semester, only analyzing cases where demographics row has a class name
#       df_check_filtered <- df_check %>% filter(year == year_ & semester == semester_ & !is.na(class_name)) %>% distinct(class_name)
#       
#       #Creating an encompassing common key to allow for string distance measurement
#       df_check_filtered <- df_check %>% mutate(test = paste0(class_name, "-" , semester, "-", as.character(year)))
#       
#       
#       #Filtering df_class down to the specific year and semester
#       df_class_filtered <- df_class %>% filter(year == year_ & semester == semester_) %>% select(class_name, semester, year, simple_standard_class)
#       
#       #Creating an encompassing common key to allow for string distance measurement
#       df_class_filtered <- df_class %>% mutate(test = paste0(class_name, "-" , semester, "-", as.character(year)))
#       
#       
#       #Fuzzymatching, comparing each row by class name, semester, and year (now one column called "test")
#       df_result <- stringdist(df_class_filtered$test, df_check_filtered$test,  method="lv")
#       
#     }
#     
#     else {
#       
#       #Allows us to bind new cases together into one large dataframe (df_result) - same as above
#       new_check_filtered <- df_check %>% filter(year == year_ & semester == semester_ & !is.na(class_name)) %>% distinct(class_name)
#       new_check_filtered <- df_check %>% mutate(test = paste0(class_name, "-", semester, "-", as.character(year)))
#       
#       df_check_filtered <- bind_rows(df_check_filtered, new_check_filtered)
#       
#       new_class_filtered <- df_class %>% filter(year == year_ & semester == semester_) %>% select(class_name, semester, year, simple_standard_class)
#       new_class_filtered <- df_class %>% mutate(test = paste0(class_name, "-" , semester, "-", as.character(year)))
#       
#       df_class_filtered <- bind_rows(df_class_filtered, new_class_filtered)
#       
#       df_result <- bind_rows(df_result, stringdist(df_class_filtered$test, df_check_filtered$test,
#                                                    method="lv"))
#     }
#     
#   }
#   
#   
# }
# 




                                