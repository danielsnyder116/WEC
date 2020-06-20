library(dplyr)
library(stringr)
library(glue)
library(readxl)
library(purrr)
library(tidyr)

setwd("/Users/Daniel/Desktop/volunteer-files/")

##### CREATING DATAFRAME FOR ALL YEARS #####

#Reading in files and standardizing capitalization
files <- str_to_upper(list.files(pattern = "*.xls*", recursive=TRUE))

## This code focuses on CORE, SCHEDULED TEACHER ROSTERS
## Other code will look at subs, tutors, etc.

#We are filtering out the workbooks that are not of use to us
roster_files <- str_subset(files, pattern = "AUTOSAVED|BOOK|CANDIDATES|COPY|DO NOT USE|EMAIL|LIST|LOG|OLD|
                                             |POTENTIAL|RETURNING|SHOW|SIGN|SUB|TECH|RETURNING|
                                             |TRAINING|TUTOR|\\~|\\(1\\)", negate = TRUE)

#str_subset is a wrapper around files[str_detect(files, pattern = "Log")]

#This also works#Keep takes in list and a function to apply as a filter
#roster_files <- files %>% keep(., str_detect(files, pattern = "List|Log|
                                    #|Potential|Show|Sign|sign|Training|Tutor", negate = TRUE))

for (roster in roster_files) {

  #Better way is to simply combine sheets to avoid messy workbook issues
  #Need to use "function (x)" to be able to set parameters of read_excel
  #Doing this makes it unnecessary to put "path = roster" for the second argument of map()
  #We also want to filter out unnecessary sheets 
  
  print(roster)
  
  #Can also just use str_subset again but converting to dataframe is another valid method
  raw <- roster %>% excel_sheets() %>% as.data.frame(.) %>% 
                    filter(., !str_detect(., pattern = "New|NEW|new|Retention|Gala|Email|applicants|All Vol|
                                                |Outreach|To Contact|VOAs|Waiting|Tutors|TUTORS|Job|DO NOT||
                                                |External|Subs|Office|Using|Deloitte|Cars|Owed|Return|St Mary|
                                                |Teacher|Salsa|Potential|Help|Lesson|Candidates|Teams" )) %>%
                    map(function (x) read_excel(path = roster, skip = 1, col_types = "text")) %>% 
                    bind_rows() %>% compact()
  
  
  #Skips empty workbook cases
  if (nrow(raw) == 0) {
    
  } else {
    
  #Due to poor file names need to add in some logic to ensure files are uniform in column
  #names to ensure easy binding later
 
  #Gets the number of columns and creates a sequence (1,2,3,4,...)
  #Then pastes each number to col to create unique, generic column names to rename the columns
  #This will allow for easier and cleaner binding
  colnames(raw) <- paste0("col", seq(ncol(raw)))

  #Adding in semester and year data - need other patterns due to variation in naming
  semester_year <- str_extract(roster, pattern = "FALL \\d\\d\\d\\d|WINTER \\d\\d\\d\\d|
                                                |SUMMER \\d\\d\\d\\d|SPRING \\d\\d\\d\\d|
                                                |\\d\\d\\d\\d FALL|\\d\\d\\d\\d WINTER|
                                                |\\d\\d\\d\\d SUMMER|\\d\\d\\d\\d SPRING")
  
  file_semester <- str_extract(semester_year, pattern = "FALL|WINTER|SPRING|SUMMER")
  file_year <- str_extract(semester_year, pattern = "\\d\\d\\d\\d")
    
  raw <- raw %>% mutate(semester = file_semester, year = file_year)

  }
   
   #Putting everything together
   if (str_detect(roster, pattern = "FALL 06 TEACHERS EVENINGS")) {
     df <- raw
     
   } else {
     df <- bind_rows(df, raw)
   }
   
}

##### CLEANING CLASS NAME COL1 #####

#Sorting dataframe by semester and year
#First need to make semester a factored variable
#This is MAGICAL
df <- df %>% mutate(semester = factor(semester, levels = c("WINTER", "SPRING", "SUMMER", "FALL")))
df <- arrange(df, year, semester)

df <- df %>% select(-col6)

#Getting rid of rows of all NA or "solo" rows - doing together causes issues
df <- df %>% filter(!is.na(col3))
df <- df %>% filter(!str_detect(col3, pattern = "Solo|solo"))

#Dropping duplicate rows
df <- distinct(df)

#Fixing cases where was one class and changed to avoid confusion
df <- df %>% mutate(col1 = str_remove_all(col1, pattern = "now |1A\\-II \\(NOW |was 2A\\-II|[:punct:]"))

#Replacing blank strings with NA
df <- df %>% mutate(col1 = na_if(col1, ""))

#Some manual adding ins
df[3686:3687, "col1"] <- c(rep("Conversation Group at Shaw Library",2))

#Get rid of reserve contacts
df <- df %>% slice(-3688:-3753)

nrow(df)

#Temporarily replace NAs with string to be able to use str_detect to get rid of junk text
#Better way down further down in code
# df <- df %>% mutate(col1 = replace_na(col1, "NA"),col2 = replace_na(col2, "NA"))

#Using new Across method rather than mutate_at
df <- df %>% mutate(across(c("col1", "col2","col3", "col4", "col5"), ~replace_na(., "NA")))

length_df <- nrow(df) - 1

#### DEALING WITH CLASS NAME SPREAD OVER THREE PLUS ROWS ####
#Case of class name being spread over 3 or 4 rows
for (i in 1:length_df) {
  
  #Cases where name is spread over 3 lines and needs to be consolidated to one
  if ( str_detect(df$col1[i], pattern = "Beginning|Intermediate|Conversation|Advanced") &
       str_detect(df$col1[i+1], pattern = "Conversation|English in the|Intermediate|Low Advanced") & 
       str_detect(df$col1[i+2], pattern =  "Advanced II|Advanced I|High|Plus|^II|^I")
  ) {
    #We take this data and add it to the previous row
    df$col1[i] <- paste(df$col1[i], df$col1[i+1], df$col1[i+2])
    
    #We then replace the data to make it easy to delete later. Deleting
    # in the middle of the loop causes issues
    df$col1[i+1] <- "SKIP"
    df$col1[i+2] <- "SKIP"
  }
}

##### TWO ####
#Case of name spread over two rows
for (i in 1:length_df) {
  
  #Cases where name is spread over two lines and needs to be consolidated to one
  if ( str_detect(df$col1[i], pattern = "Intro|Beginning|Intermediate|Advanced|High|English in the") &
       str_detect(df$col1[i+1], pattern = "Beginner II|Beginner I|Conversation|Conversation II|Conversation I|
                                           |Summit|Workplace|^II|^I|Plus") ) {
    
    #We take this data and add it to the previous row
    df$col1[i] <- paste(df$col1[i], df$col1[i+1])
    
    #We then replace the data to make it easy to delete later. Deleting
    # in the middle of the loop causes issues
    df$col1[i+1] <- "SKIP"
  }
}

#Get rid of SKIP columns
df <- df %>% filter(!str_detect(col1, pattern = "SKIP"))

#Adds conversation to #Intermediate/Advanced in col1 and 202/204 in col2
for (i in 1:nrow(df)) {
  
  if (str_detect(df$col2[i], pattern = "202|204") & str_detect(df$col1[i], pattern = "Citizenship|Conversation", negate = TRUE)) {

    df$col1[i] <- paste(df$col1[i], "Conversation")
    
  }
}

#Couldn't figure out a tidy way of doing this so using for loop for now...
for (i in 1:nrow(df)) {

  if (str_detect(df$col1[i], pattern = "Disc|disc|Their|Emailed|Emld|Will be|Sent|sent|\\d\\d\\d+|must|Room|Syllabus|
                                      |of|winter|Brain|workbook|Workbook|shares|April|12|No class|https|WHITE|Summer|
                                      |Monday|Georgetown|Name|11AM|9|4PM|25|returns|Wrote|Thank|UPDATE|update|Tue\\/|
                                      |Saturday 11|Bob|Fall|Sat 10|amsolomo|Angelina|Anne|Nichelle|Ellen Cam|
                                      |Shares with|New|Fillers|for 4B|Could be|^izzy|^Summit|Weekday 67|TueThu 67|
                                      |1A2A|2B3B|4AAdv|1A1B|2A3A|3BAdv|2B3A|3B4B")) {
    df$col1[i] <- NA_character_
  }
}

#write.csv(df, "test.csv", row.names = FALSE)

#Manual deletion - necessary due to manual untidyness of data
df <- df %>% slice(-2846:-2860,-4353:-4366,-4655:-4664,-4710:-4719,-4895:-4900)

#Reverting back to all NA_character_
df <- df %>% mutate(col1 = str_replace_all(col1, pattern = "NA", replacement = NA_character_),
                    col2 = str_replace_all(col2, pattern = "NA", replacement = NA_character_))

#Filling in class names to make data tidy
df <- df %>% fill(col1, .direction = "down")

#### Now it's time to get rid of rows that aren't relevant ####

#Dropping rows that are column headers or tutor/sub info. (treated separately)
df <- df %>% filter(!str_detect(col1, pattern = "LevelSection|Section|Jaw|Mich|Elis|Andr|Lori|Charles|Sula|Erin|Jess|
                                |Camer|Elsa|Bob|Aileen|Ina|Moc|Preston|Name|Sat|Sun|COPY|Reserve|RESERVE|
                                |Conference|Did Orientation|Need to Email|Could be |Chuck|Tutor|tutor|
                                |Wilson|Katie|Rachelle|TH|M or|Drop|TBD|Permanent|withdrew|term began|Sub|
                                |College Park|Returning|Rebecca Stewart|Lauren Mai|Meewa|Tonisha|
                                |Marcela|Donna|Alex|Hallie|Waiting|Additional|Waitlist|Other|SundayAM|Writing|
                                |Administrative"))
nrow(df)

#Filling in class day to make data tidy
df <- df %>% fill(col2, .direction = "down")

df <- df %>% filter(!str_detect(col2, pattern = "Day|Extra|Tutor|tutor|any |Any ") 
                    & !str_detect(col3, pattern = "SUB REQUEST|SOLO"))

## Class Name CLEANUP
df <- df %>% mutate(col1 = str_remove_all(col1, pattern = "I$|II$|\\+|Level | Level| 1$| 2$| 3$"))

#Add in extra space to differentiate between Intermediate and Intermediate Conv
df <- df %>% mutate(col1 = str_pad(col1, width = 30, side = "right"),
                    col3 = str_pad(col3, width = 30, side="right"))

#Cases where the entire row is the value that needs to be replaced - 
#remaining that are parts of a string are fixed below.
df <- df %>% mutate(col1 = str_squish(case_when(str_detect(col1, pattern = "1 A") ~ "1A",
                                                str_detect(col1, pattern = "1 B|1B1|1B2") ~ "1B",
                                                str_detect(col1, pattern = "2 A") ~ "2A",
                                                str_detect(col1, pattern = "2 B") ~ "2B",
                                                str_detect(col1, pattern = "3 A") ~ "3A",
                                                str_detect(col1, pattern = "4 A") ~ "4A",
                                                TRUE ~ col1)))

#Need to replace only part of string vs whole thing as above
df <- df %>% mutate(col1 = str_replace_all(col1, c("^Intermediate  " = "Intermediate Conversation",
                                                   "Grp" = "Group","AI"= "A","BII" = "B","Advanced" = "Adv")))
                                           
df <- df %>% mutate(col1 = str_to_upper(str_replace_all(col1, pattern = " ", replacement = "-")))

col1_contents <- unique(df$class_name)

#Bringing in standardization from other script
df <- df %>% mutate(col1 = str_squish(str_replace_all(col1, c("CONVERSATION|CC|CONVO" = "CONV", "GRAMMAR" = "GRAM",
                                             "BEGINNERS|BEGINNER|BEGINNING" = "BEG","INTERMEDIATE" = "INTER", 
                                             "SPANISH" = "SPAN", "-ENGLISH" = "-ENG", "BEG-CONV|BEG-CON" = "CONV-BEG",
                                             "INT-CONV|INT-CON" = "CONV-INTRO",  "LITERACY" = "LIT", 
                                             "LANGUAGE" = "LANG","COMPUTERS|COMPUTER" = "COMP", "VPLUS" = "V-PLUS", 
                                             "VLOWADV" = "V-LOW-ADV", "INTER-INTER" = "INTER",
                                             "ADV-FINAL" = "6", "CHOIR" = "CHORUS", "VV" = "V", "ADV-ADV" = "ADV",
                                             
                                             "BASIC" = "INTRO", "CONV-CLUB-" = "CONV-", "INTER-CONV" = "CONV-INTER",
                                             "ADV-N" = "5A", "ADV-A" = "5A", "ADV-R" = "5B", "ADV-B" = "5B",
                                             "ADV-PLUS-N" = "6A", "ADV-PLUS-A" = "6A",
                                             "ADV-PLUS-R" = "6B", "ADV-PLUS-B" = "6B",
                                             
                                             "ADV-PLUS-CONV" = "CONV-ADV-PLUS", "ADV-PLUSCONV" = "CONV-ADV-PLUS",
                                             "ADVCONV|ADV-CON|ADV-CONV" = "CONV-ADV", "ADV-CONV" = "CONV-ADV",
                                             "^ADV-PLUS" = "6", "^ADV-PLUS-I" = "6-I", "^ADV-PLUS-II" = "6-II",
                                             
                                          
                                             "CITIZENSHIP" = "CITIZEN", "CAREER-EXPLORATION" = "CAREER-EXPLOR",
                                             "MICROSOFT" = "MS", "OFFICE-ASSISTANT" = "OFFICE-ASSIST",
                                             "POWER-POINT" = "PP", "INTRODUCTION" = "INTRO",
                                             
                                             "CIVICS-INTRO-A-II" = "CIVICS-INTRO-A", "6N" = "6A",
                                             "INTCONV" = "CONV-INT", "^ADV " = "5",
                                             
                                             #Although we standardized it in the above replacements, getting
                                             #rid of the day/time for a simple version with simply content level
                                             #Will create a more complete class name with all information below.
                                             
                                             "-AM|-PM|-WKND|-SAT|-SUN|-TTH|-MTH|-MW|-E |-P |-WK |\\.|'S" = "",
                                             "-E-" = "-", "6BR" = "6B","-PREP|-CLASS" = "", "PREINTRO|PREBASIC" = "PRE-INTRO",
                                             "-PREP-" = "-", "INTRO-TO-COMP" = "COMP-INTRO", "INT-COMP" = "COMP-INTER",
                                             "-INT-" = "-INTER-", "-INT " = "-INTER", "COMP-101" = "COMP-INTRO",
                                             "COMP-102" = "COMP-INTER", "^ADV  " = "5", "-TRANING" = "",
                                             "MS-WORD-OFFICE-ASSISTS" = "MS-WORD-OFFICE-ASSIST", " " = ""
                                             
                                           ))))

nrow(df)

df <- df %>% slice(-5874:-5877)

#FALL 2011 has some issues but only on part of the file
for (i in 1:nrow(df)) {
  
  if (str_detect(df$year[i], pattern = "2011") & str_detect(df$semester[i], pattern = "FALL") &
      nchar(df$col3[i]) <= 2) {
    
    #Adding last name to first name column
    df$col3[i] <- paste(df$col4[i])
    
    #Adding email to email column
    df$col4[i] <- df$col5[i]
    df$col5[i] <- NA_character_
  }
}

#Because of changes in file layout starting Winter 2019, need to shift data over some
for (i in 1:nrow(df)) {
  
  if (str_detect(df$year[i], pattern = "2019|2020")) {
    
    #Adding last name to first name column
    df$col3[i] <- paste(df$col3[i], df$col4[i])
    
    #Adding email to email column
    df$col4[i] <- df$col5[i]
    df$col5[i] <- "NA"
  }
}


#Replacing rows where the data is not an email to NA
for (i in 1:nrow(df)) {
  
  if (!str_detect(df$col4[i], pattern = "@")) {
    
    df$col4[i] <- NA_character_
  }
  
}

#Bringing in some more specific column names
df <- df %>% rename(class_name = col1, class_day = col2, teacher_name = col3, 
                    teacher_email = col4, teacher_phone = col5)

#Dropping cell number as not a lot of data from last few years
df <- df %>% select(-teacher_phone)

nrow(df %>% filter(is.na(teacher_email))) / nrow(df)

nrow(df)


#Use str_detect, if punctuation at very beginning of col3 (so *), add YES
#To give idea of percentage new teacher vs returner
df <- df %>% mutate(new_volunteer = case_when(str_detect(teacher_name, pattern = "\\*") ~ "Yes",TRUE ~ 'No'))

df <- df %>% mutate(teacher_name = str_replace_all(teacher_name, pattern = "\\*", replacement = ""))

#Rearranging columns
df <- df %>% select(c("class_name":"teacher_name", "new_volunteer", everything()))

#Getting rid of rows with no teacher info or unclear info
df <- df %>% filter(!str_detect(teacher_name, pattern = "TBD|\\?|\\-\\-+|day|Yes|No |NO |no "))

#Reverting remaining columns (col3, col4, col5)
df <- df %>% mutate(across(everything(), ~na_if(., "NA")))

#Make sure no extra spaces - using new across method
df <- df %>% mutate(across(everything(), ~str_trim(.)))
df <- df %>% mutate(across(everything(), ~str_squish(.)))



#Getting rid of data where it is just a list of volunteer emails or other irrelevant text
col1_contents <- unique(df$class_name)


#Now we need to consolidate the class names - maybe in another script







#### CODE GRAVEYARD ####

#Alrighty, now it's cleanin' time
#Using new dplyr verb across to filter specific columns that are not na
#Getting rid of rows that are all NAs
#df <- df %>% filter(across(col1:col3, ~!is.na(.)))

# #Dang it couldn't get it 
# df %>% mutate(across(col1, starts_with("Em")))
# View(df %>% mutate(across(col1, ~na_if(., "must offer for|Their"))))
# df %>% filter(across(col1, starts_with("Level"), ~na_if(.)))
# 
# df %>% mutate(col1 = across(col1, str_detect(., pattern = "^[:punct:]|Level|Their|Emailed|Emld|
#                                     |Will be|Sent|\\d\\d\\d+|must||Room|Syllabus|Jaw|Mich|Elis|Andr|Lori|
#                                     |of|Moc|winter|Charles|Sula||Erin|Jess|Camer|Elsa|Brain|workbook|
#                                     |shares|Satur|April|12|No class|https|WHITE|Summer|Preston|Monday|
#                                     |Georgetown|Bob|Name|Aileen|Ina|11AM|9|4PM"), ~na_if(.)))
# 
# df <- df %>% mutate(col1 = na_if(col1, "^[:punct:]|"))
# 
# #We replace irrelevant values with NA to make it easier to fill in info.
# df <- df %>% mutate(col1 = na_if(col1, str_detect(col1, pattern = "^[:punct:]|Level|Their|Emailed|Emld|
#                                     |Will be|Sent|\\d\\d\\d+|must||Room|Syllabus|Jaw|Mich|Elis|Andr|Lori|
#                                     |of|Moc|winter|Charles|Sula||Erin|Jess|Camer|Elsa|Brain|workbook|
#                                     |shares|Satur|April|12|No class|https|WHITE|Summer|Preston|Monday|
#                                     |Georgetown|Bob|Name|Aileen|Ina|11AM|9|4PM")))

# df <- df %>% filter(str_replace_all(col1, pattern = ""),
#                     replacement = NA_character_)

#Anonymous/lambda function version
#colnames(raw) <- map(y, function(x) paste0("col", y))

#'Formula' version
#colnames(raw) <- map(y, ~paste0("col", .x))

# Although the intuition of converting a list to a dataframe to then use stringr works,
# purrr provides the solutions for filtering without having to take extra steps and with
# cleaner code. Hadley you da man!

# #Gets all the excel files and deals with folders - 318
# files <- as.data.frame(list.files(pattern = "*.xls*", recursive=TRUE), stringsAsFactors = FALSE)
# colnames(files) <- 'paths'
# 
# roster_files <- files %>% filter(!str_detect(paths, pattern = "List|Log|Potential|Show|Sign|sign|Training|Tutor"))
# other_files <- files %>% filter(str_detect(paths, pattern = "List|Log|Potential|Show|Sign|sign|Training|Tutor"))
# #roster <- roster_files$paths[1]
    
    # # #Lists all sheets in excel file - thanks Hadley!
    # # print(glue(roster, ":", length(excel_sheets(roster))))
    # # 
    # if (length(excel_sheets(roster)) > 1 & excel_sheets(roster)[1] != "")  {
    #   raw <- read_excel(roster, skip = 1, sheet = 2,  col_types = "text")
    # }
    # 
    # else {
    #   
    #   #Read in the file and get the first sheet name 
    #   raw <- read_excel(roster, skip = 1, sheet = excel_sheets(roster)[1], col_types = "text")
    #   
    # }