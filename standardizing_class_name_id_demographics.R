library(dplyr)
library(stringr)

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

#Adding in str_pad to help differentiate
df_demog <- df_demog %>% mutate(class_name = str_pad(class_name, width=30, side="right", pad=" "))
#More fixes for CLASS NAME
df_demog <- df_demog %>% mutate(class_name = case_when(str_detect(class_name, pattern="LITERACY-INTERMEDIA ") ~ "LITERACY-INTERMEDIATE",
                                                       str_detect(class_name, pattern="TAPES|LATE-FEE|CALCULATOR") ~ NA_character_,
                                                       str_detect(class_name, pattern="MICROSOFT-POWER-POI ") ~ "MICROSOFT-POWER-POINT-2000",
                                                       str_detect(class_name, pattern= "ESL-CONV-&-PRON-BEG ") ~ "ESL-CONV-&-PRON-BEGINNERS",
                                                       str_detect(class_name, pattern= "COMPUTERS-101-ENGLI ") ~ "COMPUTERS-101",
                                                       str_detect(class_name, pattern="ESLCONV-&-PRON-ADV") ~ "ESL-CONV-&-PRON-ADV",
                                                       str_detect(class_name, pattern="COM ") ~ "COMPUTERS",
                                                       str_detect(class_name, pattern="ESL-CONV-&-PRON-INT ") ~ "ESL-CONV-&-PRON-INTERMEDIATE",
                                                       str_detect(class_name, pattern="ESL-CONV-&-PRON-ADV ") ~ "ESL-CONV-&-PRON-ADVANCED",
                                                       str_detect(class_name, pattern="SPANISH-LITERACY-IN ") ~ "SPANISH-LITERACY-INTERMEDIATE",
                                                       str_detect(class_name, pattern="SPANISH-LITERACY-BE ") ~ "SPANISH-LITERACY-BEGINNERS",
                                                       str_detect(class_name, pattern="SPANISH-LITERACY-AD ") ~ "SPANISH-LITERACY-ADVANCED",
                                                       str_detect(class_name, pattern="EL/CIVICSBASIC-A |EL/CIVICSBASICA ") ~ "EL/CIVICS-BASIC-A",
                                                       
                                                       #str_detect(class_name, pattern="") ~ "",
                                                       #str_detect(class_name, pattern="") ~ "",
                                                       
                                                       TRUE ~ class_name))
#Getting rid of extra spaces
df_demog <- df_demog %>% mutate(class_name = str_squish(class_name))

#Replacing empty class names with NA
df_demog <- df_demog %>% mutate(class_name = na_if(class_name, "| |  |   |    |     |      "))

#Fixes for SEMESTER
#Changing classes marked for summer but have spring as semester
# df_demog <- df_demog %>% mutate(semester = 
#                                       case_when(semester == 'SPRING' & year==2001 & str_detect(class_name, pattern = "^SC") ~ "SUMMER",
#                                                 semester == 'SPRING' & year==2001 & str_detect(class_name, pattern = "CONV|COM") ~ "SUMMER",
#                                                 TRUE ~ semester))
# 

########## TASK 2: ALTER CLASS NAMES IN DEMOGRAPHICS DATA TO MATCH CLASS NAMES ########## 
########## IN DF_CLASS SO THERE ARE MORE MATCHES AND MORE CLARITY              ########## 


#Improving demog details
#Since the demographics data doesn't have a unique class identifier, just the name, this leads to a lot of duplicate creation
#since a class like 1A might be offered 3-5 times during one semester, with no indicator in the old name.
df_better_dem <- left_join(df_demog, df_class, by=c("class_name", "semester", "year"))

#To deal with this, after merging, we drop "duplicate" rows by looking at 
# student_id, class_name, semester, and year - there's no way a student was taking multiple of the same class
# during one semester.


#Only lost two compared to previous 51,057 - nice! 
df_better_dem <- distinct(df_better_dem, student_id, class_name, semester, year, .keep_all = TRUE)


#Checking out last 2 years of data
df_1819 <- df_better_dem %>% filter(year >= 2018)

nrow(df_1819 %>% filter(is.na(class_id)))

1542 / 5127

nrow(df_1819 %>% filter(is.na(country)))
329 / 5217

nrow(df_1819 %>% filter(is.na(class_name)))
272 / 5127

nrow(df_1819 %>% filter(is.na(ethnicity)))
311 / 5127

########## TASK 1: ADD CLASSES THAT HAPPENED THAT WERE NOT RECORDED ########## 
########## IN PROACTIVE BUT WERE FOUND IN STUDENT DEMOGRAPHIC DATA ########## 

#Focusing in on rows where there was no match and where df_demog had a class name to match
df_dem_check <- df_better_dem %>% filter(is.na(class_id) & !is.na(class_name)) %>% select(class_name, semester, year) %>%
                                  distinct(class_name, semester, year, .keep_all = TRUE)

#1076 rows long, so 1076 classes that didn't merge correctly - ew.

#After doing this for one semester of one year, I realized that this would be better to automate crikey
#Then I tried to automate it using fuzzy-matching - issue is, most of the differences are only 1 or 2 characters
# which leads to huge amounts of matches which actually complicates things more *sigh*



#It looks like there are some classes in the early years (2000-2010) that were not captured with class ids
# df_class, so we will create some new rows for df_class

df_class <- add_row(df_class, class_id = 200001, class_name = "ESL-2", semester = 'WINTER', year = 2000 , simple_standard_class = "2" )

df_class <- add_row(df_class, class_id = 200021, class_name = "ESL-1B", semester = 'SUMMER', year = 2000, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200022, class_name = "ESL-3", semester = 'SUMMER', year = 2000, simple_standard_class = "3" )
df_class <- add_row(df_class, class_id = 200023, class_name = "ESL-BASIC-I", semester = 'SUMMER', year = 2000, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200024, class_name = "ESL-2", semester = 'SUMMER', year = 2000, simple_standard_class = "2" )
df_class <- add_row(df_class, class_id = 200025, class_name = "ESL-1A", semester = 'SUMMER', year = 2000, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200026, class_name = "ESL-4", semester = 'SUMMER', year = 2000, simple_standard_class = "4")
df_class <- add_row(df_class, class_id = 200027, class_name = "ESL-5", semester = 'SUMMER',year = 2000, simple_standard_class = "5")

df_class <- add_row(df_class, class_id = 200031, class_name = "ESL-2B", semester = 'FALL', year = 2000, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200032, class_name = "ESL-2A", semester = 'FALL', year = 2000, simple_standard_class = "2A" )

df_class <- add_row(df_class, class_id = 200101, class_name = "ESL-1", semester = 'WINTER', year = 2001, simple_standard_class = "1" )

df_class <- add_row(df_class, class_id = 200121, class_name = "ESL-1B", semester = 'SUMMER', year = 2001 , simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200122, class_name = "ESL-3", semester = 'SUMMER', year = 2001 , simple_standard_class = "3")
df_class <- add_row(df_class, class_id = 200123, class_name = "ESL-1A", semester = 'SUMMER',year = 2001 , simple_standard_class = "1A")
df_class <- add_row(df_class, class_id = 200124, class_name = "ESL-BASIC-I", semester = 'SUMMER', year = 2001, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200125, class_name = "ESL-2A", semester = 'SUMMER', year = 2001 , simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200126, class_name = "ESL-ADVANCED", semester = 'SUMMER', year = 2001, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200127, class_name = "ESL-2B", semester = 'SUMMER', year = 2001, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200128, class_name = "ESL-2", semester = 'SUMMER', year = 2001, simple_standard_class = "2" )
df_class <- add_row(df_class, class_id = 200129, class_name = "ESL-1A", semester = 'SUMMER', year = 2001, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200130, class_name = "ESL-4", semester = 'SUMMER', year = 2001, simple_standard_class = "4")
df_class <- add_row(df_class, class_id = 200131, class_name = "ESL-5", semester = 'SUMMER',year = 2001, simple_standard_class = "5")
df_class <- add_row(df_class, class_id = 200132, class_name = "ESL-1", semester = 'SUMMER', year = 2001, simple_standard_class = "1" )
df_class <- add_row(df_class, class_id = 200133, class_name = "GED", semester = 'SUMMER', year = 2001, simple_standard_class = "GED" )
df_class <- add_row(df_class, class_id = 200135, class_name = "LITERACY-BEGINNERS", semester = 'SUMMER', year = 2001 , simple_standard_class = "LIT-BEG" )
df_class <- add_row(df_class, class_id = 200136, class_name = "ESL-1A-I", semester = 'SUMMER', year = 2001, simple_standard_class = "1A" )

df_class <- add_row(df_class, class_id = 200137, class_name = "ESL-1", semester = 'FALL', year = 2001, simple_standard_class = "1")
df_class <- add_row(df_class, class_id = 200138, class_name = "ESL-2", semester = 'FALL', year = 2001, simple_standard_class = "2")
df_class <- add_row(df_class, class_id = 200139, class_name = "ESL-3A-II-TTH", semester = 'FALL', year = 2001, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200140, class_name = "ESL-ADVANCED", semester = 'FALL', year = 2001, simple_standard_class = "5" )

df_class <- add_row(df_class, class_id = 200201, class_name = "ESL-1A-I", semester = 'WINTER', year = 2002, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200202, class_name = "3A-II-TTH", semester = 'WINTER', year = 2002, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200203, class_name = "ESL-1", semester = 'WINTER', year = 2002, simple_standard_class = "1" )
df_class <- add_row(df_class, class_id = 200204, class_name = "ESL-4B-I", semester = 'WINTER', year = 2002, simple_standard_class = "4B")
df_class <- add_row(df_class, class_id = 200205, class_name = "ESL-2A-II", semester = 'WINTER',year = 2002, simple_standard_class = "2A")

df_class <- add_row(df_class, class_id = 200211, class_name = "GED", semester = 'SPRING', year = 2002, simple_standard_class = "GED" )
df_class <- add_row(df_class, class_id = 200212, class_name = "ESL-1A-I", semester = 'SPRING', year = 2002, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200213, class_name = "ESL-2B-I", semester = 'SPRING', year = 2002, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200214, class_name = "ESL-2", semester = 'SPRING', year = 2002, simple_standard_class = "2" )

df_class <- add_row(df_class, class_id = 200221, class_name = "LITERACY-BEGINNERS", semester = 'SUMMER', year = 2002, simple_standard_class = "LIT-BEG" )
df_class <- add_row(df_class, class_id = 200222, class_name = "ESL-BASIC-I", semester = 'SUMMER', year = 2002, simple_standard_class = "INTRO")
df_class <- add_row(df_class, class_id = 200223, class_name = "WINDOWS-2000", semester = 'SUMMER',year = 2002, simple_standard_class = "WINDOWS-2000")
df_class <- add_row(df_class, class_id = 200224, class_name = "ESL-2A-I", semester = 'SUMMER', year = 2002, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200225, class_name = "ESL-1B", semester = 'SUMMER', year = 2002, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200226, class_name = "ESL-BASIC-II", semester = 'SUMMER', year = 2002, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200227, class_name = "ESL-2B-II", semester = 'SUMMER', year = 2002, simple_standard_class = "2B")
df_class <- add_row(df_class, class_id = 200228, class_name = "ESL-2B", semester = 'SUMMER',year = 2002, simple_standard_class = "2B")
df_class <- add_row(df_class, class_id = 200229, class_name = "ESL-ADVANCED", semester = 'SUMMER', year = 2002, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200230, class_name = "ESL-1A-I", semester = 'SUMMER', year = 2002, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200231, class_name = "ESL-4", semester = 'SUMMER',year = 2002, simple_standard_class = "4")
df_class <- add_row(df_class, class_id = 200232, class_name = "ESL-1B-I", semester = 'SUMMER', year = 2002, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200233, class_name = "ESL-3A-I", semester = 'SUMMER', year = 2002, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200234, class_name = "ESL-2A-II", semester = 'SUMMER',year = 2002, simple_standard_class = "2A")
df_class <- add_row(df_class, class_id = 200235, class_name = "ESL-1A-II", semester = 'SUMMER', year = 2002, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200236, class_name = "ESL-2B-I", semester = 'SUMMER', year = 2002, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200237, class_name = "ESL-5", semester = 'SUMMER', year = 2002, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200248, class_name = "GED", semester = 'SUMMER', year = 2002, simple_standard_class = "GED" )
df_class <- add_row(df_class, class_id = 200249, class_name = "ESL-2A", semester = 'SUMMER', year = 2002, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200250, class_name = "ESL-BASIC-III", semester = 'SUMMER', year = 2002, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200251, class_name = "LITERACY INTERMEDIATE", semester = 'SUMMER', year = 2002, simple_standard_class = "LIT-INTER" )
df_class <- add_row(df_class, class_id = 200252, class_name = "ESL-1B-II", semester = 'SUMMER', year = 2002, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200253, class_name = "CITIZENSHIP", semester = 'SUMMER', year = 2002, simple_standard_class = "CITIZEN" )
df_class <- add_row(df_class, class_id = 200254, class_name = "ESL-1", semester = 'SUMMER', year = 2002, simple_standard_class = "1" )
df_class <- add_row(df_class, class_id = 200255, class_name = "ESL-2", semester = 'SUMMER', year = 2002, simple_standard_class = "2" )
df_class <- add_row(df_class, class_id = 200256, class_name = "ESL-3", semester = 'SUMMER', year = 2002, simple_standard_class = "3" )

df_class <- add_row(df_class, class_id = 200257, class_name = "INTERNET", semester = 'FALL', year = , simple_standard_class = "INTERNET" )

df_class <- add_row(df_class, class_id = 200301, class_name = "ESL-3A-II-TTH", semester = 'WINTER', year = 2003 , simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200302, class_name = "ESL-5A-I", semester = 'WINTER', year = 2003 , simple_standard_class = "5A" )
df_class <- add_row(df_class, class_id = 200303, class_name = "ESL-2B-II", semester = 'WINTER', year = 2003 , simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 200310, class_name = "MICROSOFT-POWER-POINT-2000", semester = 'SPRING', year = 2003 , simple_standard_class = "MS-PP-2000" )

df_class <- add_row(df_class, class_id = 200320, class_name = "ESL-2A-II", semester = 'SUMMER', year = 2003, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200321, class_name = "ESL-1B-I", semester = 'SUMMER', year = 2003, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200322, class_name = "ESL-1A-I", semester = 'SUMMER', year = 2003, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200323, class_name = "ESL-2A-I", semester = 'SUMMER', year = 2003, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200324, class_name = "ESL-BASIC-I", semester = 'SUMMER', year = 2003, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200325, class_name = "ESL-3A-I", semester = 'SUMMER', year = 2003, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200326, class_name = "ESL-BASIC-II", semester = 'SUMMER', year = 2003, simple_standard_class = "INTER" )
df_class <- add_row(df_class, class_id = 200327, class_name = "ESL-2B-I", semester = 'SUMMER', year = 2003, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200328, class_name = "ESL-1A-II", semester = 'SUMMER', year = 2003, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200329, class_name = "ESL-4B-I", semester = 'SUMMER', year = 2003, simple_standard_class = "4B" )
df_class <- add_row(df_class, class_id = 200330, class_name = "ESL-5A-I", semester = 'SUMMER', year = 2003, simple_standard_class = "5A" )
df_class <- add_row(df_class, class_id = 200331, class_name = "ESL-4A-I", semester = 'SUMMER', year = 2003, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 200332, class_name = "ESL-3B-I", semester = 'SUMMER', year = 2003, simple_standard_class = "3B" )
df_class <- add_row(df_class, class_id = 200333, class_name = "WINDOWS-2000", semester = 'SUMMER', year = 2003, simple_standard_class = "WINDOWS-2000" )
df_class <- add_row(df_class, class_id = 200334, class_name = "MICROSOFT-POWER-POINT-2000", semester = 'SUMMER', year = 2003, simple_standard_class = "MS-PP-2000" )
df_class <- add_row(df_class, class_id = 200335, class_name = "LITERACY-BEGINNERS", semester = 'SUMMER', year = 2003, simple_standard_class = "LIT-BEG" )
df_class <- add_row(df_class, class_id = 200336, class_name = "INTERNET", semester = 'SUMMER', year = 2003, simple_standard_class = "INTERNET" )
df_class <- add_row(df_class, class_id = 200337, class_name = "LITERACY-INTERMEDIATE", semester = 'SUMMER', year = 2003, simple_standard_class = "LIT-INTER" )
df_class <- add_row(df_class, class_id = 200338, class_name = "COMPUTERS-101", semester = 'SUMMER', year = 2003, simple_standard_class = "COMP-INTRO" )
df_class <- add_row(df_class, class_id = 200339, class_name = "MICROSOFT-WORD-2000", semester = 'SUMMER', year = 2003, simple_standard_class = "MS-WORD-2000" )

df_class <- add_row(df_class, class_id = 200340, class_name = "LITERACY-ADVANCED", semester = 'FALL', year = 2003, simple_standard_class = "LIT-ADV" )
df_class <- add_row(df_class, class_id = 200341, class_name = "ESL-CONV-&-PRON-INTERMEDIATE", semester = 'FALL', year = 2003, simple_standard_class = "CONV-&-PRON-INTER" )
df_class <- add_row(df_class, class_id = 200342, class_name = "MICROSOFT-POWER-POINT-2000", semester = 'FALL', year = 2003, simple_standard_class = "MS-PP-2000" )

df_class <- add_row(df_class, class_id = 200401, class_name = "ESL-2A-II", semester = 'WINTER', year = 2004, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200402, class_name = "ESL-3A-II-TTH", semester = 'WINTER', year = 2004, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200403, class_name = "ESL-2B", semester = 'WINTER', year = 2004, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200404, class_name = "COMPUTERS-101", semester = 'WINTER', year = 2004, simple_standard_class = "COMP-INTRO" )

df_class <- add_row(df_class, class_id = 200420, class_name = "ESL-3B-I", semester = 'SUMMER', year = 2004, simple_standard_class = "3B" )
df_class <- add_row(df_class, class_id = 200421, class_name = "ESL-3A-I", semester = 'SUMMER', year = 2004, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200422, class_name = "ESL-1A-I", semester = 'SUMMER', year = 2004, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200423, class_name = "ESL-2A-I", semester = 'SUMMER', year = 2004, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200424, class_name = "ESL-1B-I", semester = 'SUMMER', year = 2004, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200425, class_name = "EL/CIVICS-BASIC-II", semester = 'SUMMER', year = 2004, simple_standard_class = "CIVICS-INTRO" )
df_class <- add_row(df_class, class_id = 200426, class_name = "COMPUTERS-101", semester = 'SUMMER', year = 2004, simple_standard_class = "COMP-INTRO" )
df_class <- add_row(df_class, class_id = 200427, class_name = "ESL-4A-I", semester = 'SUMMER', year = 2004, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 200428, class_name = "ESL-4B-I", semester = 'SUMMER', year = 2004, simple_standard_class = "4B" )
df_class <- add_row(df_class, class_id = 200429, class_name = "ESL-BASIC-I", semester = 'SUMMER', year = 2004, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200430, class_name = "ESL-5A-I", semester = 'SUMMER', year = 2004, simple_standard_class = "5A" )
df_class <- add_row(df_class, class_id = 200431, class_name = "ESL-2B-I", semester = 'SUMMER', year = 2004, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200432, class_name = "ESL-BASIC-II", semester = 'SUMMER', year = 2004, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200433, class_name = "ESL-5B-I", semester = 'SUMMER', year = 2004, simple_standard_class = "5B" )
df_class <- add_row(df_class, class_id = 200434, class_name = "EL/CIVICS-BASIC-I", semester = 'SUMMER', year = 2004, simple_standard_class = "CIVICS-INTRO" )
df_class <- add_row(df_class, class_id = 200435, class_name = "ESL-1A-II", semester = 'SUMMER', year = 2004, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200436, class_name = "GED", semester = 'SUMMER', year = 2004, simple_standard_class = "GED" )
df_class <- add_row(df_class, class_id = 200437, class_name = "LITERACY-ADVANCED", semester = 'SUMMER', year = 2004, simple_standard_class = "LIT-ADV" )
df_class <- add_row(df_class, class_id = 200438, class_name = "LITERACY-BEGINNERS", semester = 'SUMMER', year = 2004, simple_standard_class = "LIT-BEG" )
df_class <- add_row(df_class, class_id = 200439, class_name = "LITERACY-INTERMEDIATE", semester = 'SUMMER', year = 2004, simple_standard_class = "LIT-INTER" )
df_class <- add_row(df_class, class_id = 200440, class_name = "INTERNET", semester = 'SUMMER', year = 2004, simple_standard_class = "INTERNET" )
df_class <- add_row(df_class, class_id = 200441, class_name = "MICROSOFT-POWER-POINT", semester = 'SUMMER', year = 2004, simple_standard_class = "MS-PP-2000" )
df_class <- add_row(df_class, class_id = 200442, class_name = "ESL-5", semester = 'SUMMER', year = 2004, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200443, class_name = "ESL-CONV-&-PRON-ADVANCED", semester = 'SUMMER', year = 2004, simple_standard_class = "CONV-&-PRON-ADV" )

df_class <- add_row(df_class, class_id = 200444, class_name = "ESL-2A-II", semester = 'FALL', year = 2004, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200445, class_name = "ESL-2B-II", semester = 'FALL', year = 2004, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200446, class_name = "ESL-1B-II", semester = 'FALL', year = 2004, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200447, class_name = "WINDOWS-2000", semester = 'FALL', year = 2004, simple_standard_class = "WINDOWS-2000" )
df_class <- add_row(df_class, class_id = 200448, class_name = "INTERNET", semester = 'FALL', year = 2004, simple_standard_class = "INTERNET" )

df_class <- add_row(df_class, class_id = 200501, class_name = "ESL-ADVANCED", semester = "WINTER", year = 2005, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200502, class_name = "LITERACY-ADVANCED", semester = "WINTER", year = 2005, simple_standard_class = "LIT-ADV" )
df_class <- add_row(df_class, class_id = 200503, class_name = "LITERACY-INTERMEDIATE", semester = "WINTER", year = 2005, simple_standard_class = "LIT-INTER" )
df_class <- add_row(df_class, class_id = 200504, class_name = "ESL-4A", semester = "WINTER", year = 2005, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 200505, class_name = "ESL-CONV-&-PRON-INTERMEDIATE", semester = "WINTER", year = 2005, simple_standard_class = "CONV-&-PRON-INTER" )
df_class <- add_row(df_class, class_id = 200506, class_name = "ESL-2B-II", semester = "WINTER", year = 2005, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200507, class_name = "ESL-1B", semester = "WINTER", year = 2005, simple_standard_class = "1B" )

#THESE TWO ARE FILLING IN NAs from df_class
df_class[3019, c("class_name")] <- "COMPUTERS-102"
df_class[3020, c("class_name")] <- "ESL-3A"


df_class <- add_row(df_class, class_id = 200520, class_name = "ESL-2A-I", semester = "SUMMER", year = 2005, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200521, class_name = "ESL-3A-I", semester = "SUMMER", year = 2005, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200522, class_name = "ESL-BASIC-I", semester = "SUMMER", year = 2005, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200523, class_name = "EL/CIVICS-BASIC-I", semester = "SUMMER", year = 2005, simple_standard_class = "CIVICS-INTRO" )
df_class <- add_row(df_class, class_id = 200524, class_name = "ESL-1B-I", semester = "SUMMER", year = 2005, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200525, class_name = "ESL-1A-I", semester = "SUMMER", year = 2005, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200526, class_name = "ESL-BASIC-II", semester = "SUMMER", year = 2005, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200527, class_name = "ESL-2B-I", semester = "SUMMER", year = 2005, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200528, class_name = "ESL-3B-I", semester = "SUMMER", year = 2005, simple_standard_class = "3B" )
df_class <- add_row(df_class, class_id = 200529, class_name = "ESL-3B", semester = "SUMMER", year = 2005, simple_standard_class = "3B" )
df_class <- add_row(df_class, class_id = 200530, class_name = "LITERACY-ADVANCED", semester = "SUMMER", year = 2005, simple_standard_class = "LIT-ADV" )
df_class <- add_row(df_class, class_id = 200531, class_name = "ESL-2B", semester = "SUMMER", year = 2005, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200532, class_name = "ESL-2A", semester = "SUMMER", year = 2005, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200533, class_name = "ESL-4A-I", semester = "SUMMER", year = 2005, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 200534, class_name = "ESL-4B-I", semester = "SUMMER", year = 2005, simple_standard_class = "4B" )
df_class <- add_row(df_class, class_id = 200535, class_name = "EL/CIVICS-BASIC-II", semester = "SUMMER", year = 2005, simple_standard_class = "CIVICS-INTRO" )
df_class <- add_row(df_class, class_id = 200536, class_name = "COMPUTERS-101", semester = "SUMMER", year = 2005, simple_standard_class = "COMP-INTRO" )
df_class <- add_row(df_class, class_id = 200537, class_name = "ESL-ADVANCED", semester = "SUMMER", year = 2005, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200538, class_name = "ESL-4A", semester = "SUMMER", year = 2005, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 200539, class_name = "LITERACY-BEGINNERS", semester = "SUMMER", year = 2005, simple_standard_class = "LIT-BEG" )
df_class <- add_row(df_class, class_id = 200540, class_name = "LITERACY-INTERMEDIATE", semester = "SUMMER", year = 2005, simple_standard_class = "LIT-INTER" )
df_class <- add_row(df_class, class_id = 200541, class_name = "ADVANCED", semester = "SUMMER", year = 2005, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200542, class_name = "ESL-1A", semester = "SUMMER", year = 2005, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200543, class_name = "1A-II", semester = "SUMMER", year = 2005, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200544, class_name = "ESL-3A", semester = "SUMMER", year = 2005, simple_standard_class = "3A" )

df_class <- add_row(df_class, class_id = 200545, class_name = "ESL-3B", semester = "FALL", year = 2005, simple_standard_class = "3B" )
df_class <- add_row(df_class, class_id = 200546, class_name = "ESL-3A", semester = "FALL", year = 2005, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200547, class_name = "ESL-2B", semester = "FALL", year = 2005, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200549, class_name = "ESL-1B", semester = "FALL", year = 2005, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200550, class_name = "ADVANCED", semester = "FALL", year = 2005, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200551, class_name = "ESL-4A", semester = "FALL", year = 2005, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 200552, class_name = "ESL-4B", semester = "FALL", year = 2005, simple_standard_class = "4B" )
df_class <- add_row(df_class, class_id = 200553, class_name = "ESL-5B", semester = "FALL", year = 2005, simple_standard_class = "5B" )
df_class <- add_row(df_class, class_id = 200554, class_name = "1A-II", semester = "FALL", year = 2005, simple_standard_class = "1A" )

df_class <- add_row(df_class, class_id = 200601, class_name = "LITERACY-BEGINNERS", semester = "WINTER", year = 2006, simple_standard_class = "LIT-BEG" )
df_class <- add_row(df_class, class_id = 200602, class_name = "LITERACY-ADVANCED", semester = "WINTER", year = 2006, simple_standard_class = "LIT-ADV" )
df_class <- add_row(df_class, class_id = 200603, class_name = "LITERACY-INTERMEDIATE", semester = "WINTER", year = 2006, simple_standard_class = "LIT-INTER" )
df_class <- add_row(df_class, class_id = 200604, class_name = "COMPUTERS-102", semester = "WINTER", year = 2006, simple_standard_class = "COMP-INTER" )

df_class <- add_row(df_class, class_id = 200611, class_name = "ESL-5B", semester = "SPRING", year = 2006, simple_standard_class = "5B" )

df_class <- add_row(df_class, class_id = 200620, class_name = "ESL-3A", semester = "SUMMER", year = 2006, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200621, class_name = "ESL-2A", semester = "SUMMER", year = 2006, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200622, class_name = "ESL-ADVANCED", semester = "SUMMER", year = 2006, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200623, class_name = "EL/CIVICS-BASIC-I", semester = "SUMMER", year = 2006, simple_standard_class = "CIVICS-INTRO" )
df_class <- add_row(df_class, class_id = 200624, class_name = "ESL-4B", semester = "SUMMER", year = 2006, simple_standard_class = "4B" )
df_class <- add_row(df_class, class_id = 200625, class_name = "EL/CIVICS-BASIC-II", semester = "SUMMER", year = 2006, simple_standard_class = "CIVICS-INTRO" )
df_class <- add_row(df_class, class_id = 200626, class_name = "ESL-1A-I", semester = "SUMMER", year = 2006, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200627, class_name = "ESL-1B", semester = "SUMMER", year = 2006, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200628, class_name = "ESL-5B", semester = "SUMMER", year = 2006, simple_standard_class = "5B" )
df_class <- add_row(df_class, class_id = 200629, class_name = "LITERACY-BEGINNERS", semester = "SUMMER", year = 2006, simple_standard_class = "LIT-BEG" )
df_class <- add_row(df_class, class_id = 200630, class_name = "ESL-4A", semester = "SUMMER", year = 2006, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 200631, class_name = "ESL-1A-II", semester = "SUMMER", year = 2006, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200632, class_name = "ESL-5A", semester = "SUMMER", year = 2006, simple_standard_class = "5A" )
df_class <- add_row(df_class, class_id = 200633, class_name = "ESL-3B", semester = "SUMMER", year = 2006, simple_standard_class = "3B" )
df_class <- add_row(df_class, class_id = 200634, class_name = "LITERACY-INTERMEDIATE", semester = "SUMMER", year = 2006, simple_standard_class = "LIT-INTER" )
df_class <- add_row(df_class, class_id = 200635, class_name = "ESL-2B", semester = "SUMMER", year = 2006, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200636, class_name = "LITERACY-ADVANCED", semester = "SUMMER", year = 2006, simple_standard_class = "LIT-ADV" )

df_class <- add_row(df_class, class_id = 200638, class_name = "ESL-1B-I", semester = "FALL", year = 2006, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200639, class_name = "ESL-1A-III", semester = "FALL", year = 2006, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200640, class_name = "GED-SPANISH", semester = "FALL", year = 2006, simple_standard_class = "GED-SPAN" )
df_class <- add_row(df_class, class_id = 200641, class_name = "ESL-1B-II", semester = "FALL", year = 2006, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200642, class_name = "ESL-3A-I", semester = "FALL", year = 2006, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200643, class_name = "ESL-5A-I", semester = "FALL", year = 2006, simple_standard_class = "5A" )

df_class <- add_row(df_class, class_id = 200711, class_name = "ESL-4A", semester = "SPRING", year = 2007, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 200712, class_name = "CITIZENSHIP", semester = "SPRING", year = 2007, simple_standard_class = "CITIZEN" )

df_class <- add_row(df_class, class_id = 200720, class_name = "ESL-2A", semester = "SUMMER", year = 2007, simple_standard_class = "2A" )
df_class <- add_row(df_class, class_id = 200721, class_name = "ESL-4A", semester = "SUMMER", year = 2007, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 200722, class_name = "EL/CIVICS-BASIC-I", semester = "SUMMER", year = 2007, simple_standard_class = "CIVICS-INTRO" )
df_class <- add_row(df_class, class_id = 200723, class_name = "ESL-2B", semester = "SUMMER", year = 2007, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200724, class_name = "ESL-BASIC-II", semester = "SUMMER", year = 2007, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200725, class_name = "EL/CIVICS-BASIC-II", semester = "SUMMER", year = 2007, simple_standard_class = "CIVICS-INTRO" )
df_class <- add_row(df_class, class_id = 200726, class_name = "ESL-3B", semester = "SUMMER", year = 2007, simple_standard_class = "3B" )
df_class <- add_row(df_class, class_id = 200727, class_name = "ESL-ADVANCED", semester = "SUMMER", year = 2007, simple_standard_class = "5" )
df_class <- add_row(df_class, class_id = 200728, class_name = "ESL-1B-I", semester = "SUMMER", year = 2007, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200729, class_name = "ESL-3A", semester = "SUMMER", year = 2007, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200731, class_name = "LITERACY-BEGINNERS", semester = "SUMMER", year = 2007, simple_standard_class = "LIT-BEG" )
df_class <- add_row(df_class, class_id = 200732, class_name = "ESL-4B", semester = "SUMMER", year = 2007, simple_standard_class = "4B" )
df_class <- add_row(df_class, class_id = 200733, class_name = "ESL-1A-II", semester = "SUMMER", year = 2007, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200734, class_name = "LITERACY-ADVANCED", semester = "SUMMER", year = 2007, simple_standard_class = "LIT-ADV" )

#WEIRD ESL-EVE GENERIC CLASSES
# df_class <- add_row(df_class, class_id = 200548, class_name = "ESL-EVE-7-9PM", semester = "FALL", year = 2005, simple_standard_class = "?" )
# df_class <- add_row(df_class, class_id = 200605, class_name = "ESL-EVE-7-9PM", semester = "WINTER", year = 2006, simple_standard_class = "?" )
# df_class <- add_row(df_class, class_id = 200610, class_name = "ESL-EVE-7-9PM", semester = "SPRING", year = 2006, simple_standard_class = "?" )
# df_class <- add_row(df_class, class_id = 200644, class_name = "ESL-EVE-7-9PM", semester = "FALL", year = 2006, simple_standard_class = "?" )
# df_class <- add_row(df_class, class_id = 200701, class_name = "ESL-EVE-7-9PM", semester = "WINTER", year = 2007, simple_standard_class = "?" )
# df_class <- add_row(df_class, class_id = 200710, class_name = "ESL-EVE-7-9PM", semester = "SPRING", year = 2007, simple_standard_class = "?" )
# df_class <- add_row(df_class, class_id = 200637, class_name = "ESL-EVE-7-9PM", semester = "SUMMER", year = 2006, simple_standard_class = "?" )
# df_class <- add_row(df_class, class_id = 200730, class_name = "ESL-EVE-7-9PM", semester = "SUMMER", year = 2007, simple_standard_class = "?" )
# df_class <- add_row(df_class, class_id = 200735, class_name = "ESL-EVE-7-9PM", semester = "FALL", year = 2007, simple_standard_class = "?" )
# df_class <- add_row(df_class, class_id = 200802, class_name = "ESL-EVE-7-9PM", semester = "WINTER", year = 2008, simple_standard_class = "?" )
# FALL 2008, WINTER 2009, SPRING 2009,



df_class <- add_row(df_class, class_id = 200801, class_name = "ESL-BASIC-A-I", semester = "WINTER", year = 2008, simple_standard_class = "INTRO-A" )
df_class <- add_row(df_class, class_id = 200803, class_name = "ESL-1A", semester = "WINTER", year = 2008, simple_standard_class = "1A" )


# SCONV-A	SPRING	2008
# 2	SBASIC-B-II	SPRING	2008
# 3	SBASIC-A-I	SPRING	2008
# 4	SBASIC-A-II	SPRING	2008
# 5	SBASIC-A	SPRING	2008
# 6	SCONV-B	SPRING	2008
# 7	SBASIC-B	SPRING	2008
# 8	ESL-EVE-7-9PM	SPRING	2008
# 9	SBASIC-B-I	SPRING	2008
# 10	SCONV-A-I	SPRING	2008
# 11	ESL-1B	SPRING	2008
# 12	ESL-2A-II	SPRING	2008

#df_class <- add_row(df_class, class_id = 200735, class_name = "", semester = "SPRING", year = 2008, simple_standard_class = "?" )

# ESL-1B	SUMMER	2008
# 2	EL/CIVICS-BASIC-A	SUMMER	2008
# 3	ESL-2A	SUMMER	2008
# 4	ESL-BASIC-B	SUMMER	2008
# 5	ESL-2A-II	SUMMER	2008
# 6	ESL-BASIC-A	SUMMER	2008
# 7	ESL-3A	SUMMER	2008
# 8	ESL-1A	SUMMER	2008
# 9	ESL-3B	SUMMER	2008
# 10	ESL-BASIC-A-I	SUMMER	2008
# 11	SCONV-B-I	SUMMER	2008
# 12	ESL-BASIC-II	SUMMER	2008
# 13	SBASIC-A	SUMMER	2008
# 14	ESL-1B-I	SUMMER	2008
# 15	ESL-2B	SUMMER	2008
# 16	SBASIC-B	SUMMER	2008
# 17	ESL-ADVANCED	SUMMER	2008
# 18	ESL-2A-I	SUMMER	2008
# 19	ESL-4A	SUMMER	2008
# 20	EL/CIVICSBASIC-A	SUMMER	2008
# 21	ESL-2B-I	SUMMER	2008
# 22	COMPUTERS-101	SUMMER	2008
# 23	ESL-4B	SUMMER	2008
# 24	ESL-BASIC-B-I
#df_class <- add_row(df_class, class_id = 200820, class_name = "", semester = "SUMMER", year = 2008, simple_standard_class = "" )


df_class <- add_row(df_class, class_id = 200830, class_name = "EL/CIVICS-BASIC-A", semester = "FALL", year = 2008, simple_standard_class = "CIVICS-INTRO-A" )
df_class <- add_row(df_class, class_id = 200831, class_name = "EL/CIVICS-BASIC-B", semester = "FALL", year = 2008, simple_standard_class = "CIVICS-INTRO-B" )
df_class <- add_row(df_class, class_id = 200832, class_name = "ESL-1A-I", semester = "FALL", year = 2008, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200832, class_name = "3A-II", semester = "FALL", year = 2008, simple_standard_class = "3A" )

df_class <- add_row(df_class, class_id = 200901, class_name = "EL/CIVICS-BASIC-A", semester = "WINTER", year = 2009, simple_standard_class = "CIVICS-INTRO-A" )
df_class <- add_row(df_class, class_id = 200902, class_name = "EL/CIVICS-BASIC-B", semester = "WINTER", year = 2009, simple_standard_class = "CIVICS-INTRO-B" )
df_class <- add_row(df_class, class_id = 200903, class_name = "3A-II", semester = "WINTER", year = 2009, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 200904, class_name = "ESL-2B-I", semester = "WINTER", year = 2009, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200905, class_name = "3A-I", semester = "WINTER", year = 2009, simple_standard_class = "3A" )


# EL/CIVICS-BASIC-A	SPRING	2009
# 2	SCONV-B-II	SPRING	2009
# 3	SBASIC-A-II	SPRING	2009
# 4	SBASIC-A-I	SPRING	2009
# 5	SCONV-A-I	SPRING	2009
# 6	ESL-EVE-7-9PM	SPRING	2009
# 7	ESL-BASIC-A	SPRING	2009
# 8	SCONV-A	SPRING	2009
# 9	SCONV-B	SPRING	2009
# 10	SCONV-B-I	SPRING	2009
# 11	ESL-BASIC-B-II	SPRING	2009
# 12	CITIZENSHIP-PREP	SPRING	2009
# 13	SCONV-A-II	SPRING	2009
# 14	SPANISH-LITERACY-BEGINNERS	SPRING	2009
# 15	SCONV-A-III	SPRING	2009
# 16	EL/CIVICS-BASIC-I
#df_class <- add_row(df_class, class_id = 200910, class_name = "EL/CIVICS-BASIC-A", semester = "SPRING", year = 2009, simple_standard_class = "CIVICS-INTRO-A" )
#df_class <- add_row(df_class, class_id = 200911, class_name = "SPANISH-LITERACY-BEGINNERS", semester = "SPRING", year = 2009, simple_standard_class = "SPAN-LIT-BEG" )



# ESL-ADVANCED	SUMMER	2009
# 2	ESL-2A	SUMMER	2009
# 3	ESL-1A-I	SUMMER	2009
# 4	ESL-2B	SUMMER	2009
# 5	ESL-BASIC-B	SUMMER	2009
# 6	EL/CIVICS-BASIC-A	SUMMER	2009
# 7	SPANISH-LITERACY-BEGINNERS	SUMMER	2009
# 8	ESL-4A	SUMMER	2009
# 9	ESL-3A	SUMMER	2009
# 10	ESL-1B	SUMMER	2009
# 11	ESL-BASIC-A-I	SUMMER	2009
# 12	ESL-BASIC-B-I	SUMMER	2009
# 13	SCONVA-III	SUMMER	2009
# 14	ESL-BASIC-A	SUMMER	2009
# 15	ESL-ADVANCED-II	SUMMER	2009
# 16	ESL-4B	SUMMER	2009
# 17	ESL-3B	SUMMER	2009


df_class <- add_row(df_class, class_id = 200920, class_name = "SPANISH-LITERACY-BEGINNERS", semester = "SUMMER", year = 2009, simple_standard_class = "SPAN-LIT-BEG" )
df_class <- add_row(df_class, class_id = 200921, class_name = "EL/CIVICS-BASIC-A", semester = "SUMMER", year = 2009, simple_standard_class = "CIVICS-INTRO-A" )
df_class <- add_row(df_class, class_id = 200922, class_name = "SPANISH-LITERACY-ADVANCED", semester = "SUMMER", year = 2009, simple_standard_class = "SPAN-LIT-ADV" )

df_class <- add_row(df_class, class_id = 200930, class_name = "EL/CIVICS-BASIC-A", semester = "FALL", year = 2009, simple_standard_class = "CIVICS-INTRO-A" )
df_class <- add_row(df_class, class_id = 200931, class_name = "ESL-1B-I", semester = "FALL", year = 2009, simple_standard_class = "1B" )
df_class <- add_row(df_class, class_id = 200932, class_name = "ESL-BASIC", semester = "FALL", year = 2009, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200933, class_name = "ESL-1B-II", semester = "FALL", year = 2009, simple_standard_class = "1B" )


######################
View(df_dem_check %>% filter(year==2010, semester=="FALL") %>% unique())


df_class <- add_row(df_class, class_id = 201010, class_name = "EL/CIVICS-BASIC-A", semester = "WINTER", year = 2010, simple_standard_class = "CIVICS-INTRO-A" )
df_class <- add_row(df_class, class_id = 201011, class_name = "ESL-1A-II", semester = "WINTER", year = 2010, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 201012, class_name = "ESL-1A-I", semester = "WINTER", year = 2010, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 201013, class_name = "ESL-3A-II-TTH", semester = "WINTER", year = 2010, simple_standard_class = "3A" )
df_class <- add_row(df_class, class_id = 201014, class_name = "ESL-4A-II", semester = "WINTER", year = 2010, simple_standard_class = "4A" )
df_class <- add_row(df_class, class_id = 201015, class_name = "ESL-BASIC-C", semester = "WINTER", year = 2010, simple_standard_class = "INTRO-C" )

# 
# SCONVINT	SPRING	2010
# 2	ESL-MW10-1PM	SPRING	2010
# 3	ESL-3A-II-TTH	SPRING	2010
# 4	INTROCONV	SPRING	2010
# 5	ESL-3C-S	SPRING	2010
# 6	SBASIC-II	SPRING	2010
# 7	EL/CIVICS-BASIC-A	SPRING	2010
# 8	SBASIC	SPRING	2010
# 9	SCONVADV	SPRING	2010
# 10	SBASIC-I	SPRING	2010
# 11	EL/CIVICSBASICA	SPRING	2010
# 12	SCONV-A	SPRING	2010
# 13	SBASIC-A-I	SPRING	2010
# 14	ESL-EVE-7-9PM	SPRING	2010
# 15	ESL-2A-I	SPRING	2010
# 16	COMPUTER-INTERNET	SPRING	2010
# 17	ESL-BASIC-B-I

df_class <- add_row(df_class, class_id = 201020, class_name = "", semester = "SPRING", year = 2010, simple_standard_class = "" )



# ESL-2A	SUMMER	2010
# 2	ESL-BASIC-A	SUMMER	2010
# 3	ESL-3A	SUMMER	2010
# 4	ESL-1B	SUMMER	2010
# 5	ESL-3B	SUMMER	2010
# 6	ESL-MW10-1PM	SUMMER	2010
# 7	COMPUTERS-101	SUMMER	2010
# 8	ESL-4A	SUMMER	2010
# 9	ESL-1A	SUMMER	2010
# 10	ESL-ADVANCED	SUMMER	2010
# 11	EL/CIVICS-BASIC-B	SUMMER	2010
# 12	ESL-2B	SUMMER	2010
# 13	ESL-4B	SUMMER	2010
# 14	ESL-M-TH-10-1AM	SUMMER	2010
# 15	ESL-2A-I	SUMMER	2010
# 16	ESL-BASIC-B	SUMMER	2010
# 17	ESL-BASIC-A-II	SUMMER	2010
# 18	ESL-EVE-7-9PM	SUMMER	2010
# 19	ESL-1C-S	SUMMER	2010
# 20	SPANISH-LITERACY-INTERMEDIATE	SUMMER	2010
# 21	EL/CIVICS-BASIC-A	SUMMER

df_class <- add_row(df_class, class_id = 201020, class_name = "SPANISH-LITERACY-INTERMEDIATE", semester = "SUMMER", year = 2010, simple_standard_class = "SPAN-LIT-INTER" )















df_class <- add_row(df_class, class_id = 20092, class_name = "", semester = "FALL", year = 2009, simple_standard_class = "" )






#After adding in missing class data and fixing class names:

#We need to check by semester and year and see if the new class rows fill in any of the NAs in df_class.


#Since the demographics data doesn't have a unique class identifier, just the name, this leads to a lot of duplicate creation
#since a class like 1A might be offered 3-5 times during one semester, with no indicator in the old name.
df_new_better_dem <- left_join(df_demog, df_class, by=c("class_name", "semester", "year"))

#To deal with this, after merging, we drop "duplicate" rows by looking at 
# student_id, class_name, semester, and year - there's no way a student was taking multiple of the same class
# during one semester.


#Only lost two compared to previous 51,057 - nice! 
df_new_better_dem <- distinct(df_better_dem, student_id, class_name, semester, year, .keep_all = TRUE)




#Focusing in on rows where there was no match and where df_demog had a class name to match
df_new_dem_check <- df_new_better_dem %>% filter(is.na(class_id) & !is.na(class_name)) %>% select(class_name, semester, year) %>%
  distinct(class_name, semester, year, .keep_all = TRUE)



















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
# }
# 
#This for loop iterates over the data that has merging issues from df_demog
# #and performs renaming tasks for class_name for each year (and by semester when needed)
# #This looks like there could be a better way to code it, but unfortunately since each year has
# #different issues, it has be done one year at at time yippeeeeeee
# 
# years <- seq(2000, 2019)
# 
# for (year in years) {
#   
#   
#   if (year == 2001 & semester== "SUMMER") {
#     df_demog <- df_demog %>% mutate(class_name = 
#                                       case_when(,
#                                                 TRUE ~ class_name))
#   }
#   
#   else if (year == 2003 & semester == "SUMMER") {
#     df_demog <- df_demog %>% mutate(class_name = 
#                                       case_when(
#                                         
#                                         TRUE ~ class_name))
#     
#   }
#   
#   
#   else if (year == 2003 & semester == "FALL") {
#     df_demog <- df_demog %>% mutate(class_name = 
#                                       case_when(str_detect(class_name, pattern="ESL-CONV-&-PRON-INT ") ~ "ESL-CONV-&-PRON-INTERMEDIATE",
#                                                 str_detect(class_name, pattern="ESL-CONV-&-PRON-ADV ") ~ "ESL-CONV-&-PRON-ADVANCED",
#                                                 
#                                                 
#                                                 TRUE ~ class_name))
#     
#     
#   }
#   
#   else {
#     
#   }
# }
#   else if (year == 2004) {}
#   else if (year == 2005) {}
#   else if (year == 2006) {}
#   else if (year == 2007) {}
#   else if (year == 2008) {}
#   else if (year == 2009) {}
#   else if (year == 2010) {}
#   else if (year == 2011) {}
#   else if (year == 2012) {}
#   else if (year == 2013) {}
#   else if (year == 2014) {}
#   else if (year == 2015) {}
#   else if (year == 2016) {}
#   else if (year == 2016) {}
#   else if (year == 2016) {}
#   else if (year == 2016) {}
#   
#   else {
#     
#   }
# } 
#   






                                