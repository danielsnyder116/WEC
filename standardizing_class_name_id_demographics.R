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
df_demog <- df_demog %>% mutate(class_name = case_when(str_detect(class_name, pattern="LITERACY-INTERMEDIA ") ~ "LITERACY INTERMEDIATE",
                                                       str_detect(class_name, pattern="TAPES") ~ NA_character_,
                                                       str_detect(class_name, pattern="MICROSOFT-POWER-POI ") ~ "MICROSOFT-POWER-POINT-2000",
                                                       str_detect(class_name, pattern = "ESL-CONV-&-PRON-BEG ") ~ "ESL-CONV-&-PRON-BEGINNERS",
                                                       str_detect(class_name, pattern = "COMPUTERS-101-ENGLI ") ~ "COMPUTERS-101",
                                                       
                                                       
                                                       TRUE ~ class_name))
#Getting rid of extra spaces
df_demog <- df_demog %>% mutate(class_name = str_squish(class_name))

#Fixes for SEMESTER
#Changing classes marked for summer but have spring as semester
df_demog <- df_demog %>% mutate(semester = 
                                      case_when(semester == 'SPRING' & year==2001 & str_detect(class_name, pattern = "^SC") ~ "SUMMER",
                                                semester == 'SPRING' & year==2001 & str_detect(class_name, pattern = "CONV|COM") ~ "SUMMER",
                                                TRUE ~ semester))


########## TASK 2: ALTER CLASS NAMES IN DEMOGRAPHICS DATA TO MATCH CLASS NAMES ########## 
########## IN DF_CLASS SO THERE ARE MORE MATCHES AND MORE CLARITY              ########## 


#This for loop iterates over the data that has merging issues from df_demog
#and performs renaming tasks for class_name for each year (and by semester when needed)
#This looks like there could be a better way to code it, but unfortunately since each year has
#different issues, it has be done one year at at time yippeeeeeee

years <- seq(2000, 2019)

for (year in years) {
  
  
  if (year == 2001 & semester== "SUMMER") {
    df_demog <- df_demog %>% mutate(class_name = 
                                      case_when(str_detect(class_name, pattern="ESLCONV-&-PRON-ADV") ~ "ESL-CONV-&-PRON-ADV",
                                                str_detect(class_name, pattern="COM") ~ "COMPUTERS",
                                                str_detect(class_name, pattern="LITERACY-INTERMEDIA") ~ "LITERACY INTERMEDIATE",
                                                TRUE ~ class_name))
  }
  
  else if (year == 2003 & semester == "SUMMER") {
    df_demog <- df_demog %>% mutate(class_name = 
                                      case_when(str_detect(class_name, pattern="ESL-CONV-&-PRON-INT ") ~ "ESL-CONV-&-PRON-INTERMEDIATE",
                                                str_detect(class_name, pattern="ESL-CONV-&-PRON-ADV ") ~ "ESL-CONV-&-PRON-ADVANCED",
                                                
                                                
                                                TRUE ~ class_name))
    
    
  }
  
  
  else if (year == 2003 & semester == "FALL") {
    df_demog <- df_demog %>% mutate(class_name = 
                                      case_when(str_detect(class_name, pattern="ESL-CONV-&-PRON-INT ") ~ "ESL-CONV-&-PRON-INTERMEDIATE",
                                                str_detect(class_name, pattern="ESL-CONV-&-PRON-ADV ") ~ "ESL-CONV-&-PRON-ADVANCED",
                                                
                                                
                                                TRUE ~ class_name))
    
    
  }
  
  else {
    
  }
}
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

View(df_dem_check %>% filter(year==2004, semester=="SPRING") %>% unique())

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



# 25	SUMMER	2004
# 26	ESL-CONV-&-PRON-ADV	

df_class <- add_row(df_class, class_id = 200420, class_name = "ESL-2B-I", semester = 'SUMMER', year = 2004, simple_standard_class = "2B" )
df_class <- add_row(df_class, class_id = 200421, class_name = "ESL-BASIC-II", semester = 'SUMMER', year = 2004, simple_standard_class = "INTRO" )
df_class <- add_row(df_class, class_id = 200422, class_name = "ESL-5B-I", semester = 'SUMMER', year = 2004, simple_standard_class = "5B" )
df_class <- add_row(df_class, class_id = 200423, class_name = "EL/CIVICS-BASIC-I", semester = 'SUMMER', year = 2004, simple_standard_class = "CIVICS-INTRO" )
df_class <- add_row(df_class, class_id = 200424, class_name = "ESL-1A-II", semester = 'SUMMER', year = 2004, simple_standard_class = "1A" )
df_class <- add_row(df_class, class_id = 200425, class_name = "GED", semester = 'SUMMER', year = 2004, simple_standard_class = "GED" )
df_class <- add_row(df_class, class_id = 200426, class_name = "LITERACY-ADVANCED", semester = 'SUMMER', year = 2004, simple_standard_class = "LIT-ADV" )
df_class <- add_row(df_class, class_id = 200427, class_name = "LITERACY-BEGINNERS", semester = 'SUMMER', year = 2004, simple_standard_class = "LIT-BEG" )
df_class <- add_row(df_class, class_id = 200428, class_name = "LITERACY-INTERMEDIATE", semester = 'SUMMER', year = 2004, simple_standard_class = "LIT-INTER" )
df_class <- add_row(df_class, class_id = 200429, class_name = "INTERNET", semester = 'SUMMER', year = 2004, simple_standard_class = "INTERNET" )
df_class <- add_row(df_class, class_id = 200430, class_name = "MICROSOFT-POWER-POINT", semester = 'SUMMER', year = 2004, simple_standard_class = "MS-PP-2000" )
df_class <- add_row(df_class, class_id = 200431, class_name = "ESL-5", semester = 'SUMMER', year = 2004, simple_standard_class = "5" )

df_class <- add_row(df_class, class_id = 200432, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )

df_class <- add_row(df_class, class_id = 20042, class_name = "", semester = 'SUMMER', year = 2004, simple_standard_class = "" )







#After adding in missing class data and fixing class names:


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
#   
# }
# 




                                