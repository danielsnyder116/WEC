library(dplyr)
library(stringr)
library(glue)
library(readxl)
library(purrr)
library(tidyr)

setwd("/Users/Daniel/Desktop/volunteer-files/")

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
  #print(roster %>% excel_sheets())
  
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

#Getting rid of rows of all NA or "solo" rows - doing together causes issues
df <- df %>% filter(!is.na(col3))
df <- df %>% filter(!str_detect(col3, pattern = "Solo|solo"))

nrow(df)

#Dropping duplicate rows
df <- distinct(df)

#Getting rid of data where it is just a list of volunteer emails or other irrelevant text
col1_contents <- unique(df$col1)

nrow(df)

#Temporarily replace NAs with string to be able to use str_detect to get rid of junk text
df <- df %>% mutate(col1 = replace_na(col1, "NA"))

#Clearing first column to make data tidyr in terms of class data
#Couldn't figure out a tidy way of doing this so using for loop for now..
for (i in 1:nrow(df)) {

  if (str_detect(df$col1[i], pattern = "Disc|disc|Their|Emailed|Emld|Will be|Sent|sent|\\d\\d\\d+|must|Room|Syllabus|
                                      |of|winter|Brain|workbook|Workbook|shares|April|12|No class|https|WHITE|Summer|
                                      |Monday|Georgetown|Name|11AM|9|4PM|25|returns|Wrote|Thank|UPDATE|update|Tue\\/|
                                      |Saturday 11|Bob|Fall|Sat 10|amsolomo|Angelina|Anne|Nichelle|Ellen Cam|
                                      |Shares with|New|Fillers|for 4B")) {
    df$col1[i] <- NA_character_
  }
}

#Reverting back to all NA_character_
df <- df %>% mutate(col1 = str_replace_all(col1, pattern = "NA", replacement = NA_character_))

#Filling in class names  and day of week to make data tidy
df <- df %>% fill(c(col1, col2), .direction = "down")

#### Now it's time to get rid of rows that aren't relevant ####

#Dropping rows that are column headers without information or extra info.
df <- df %>% filter(!str_detect(col1, pattern = "Level\\/|Jaw|Mich|Elis|Andr|Lori|Charles|Sula|Erin|Jess|Camer|Elsa|
                                |Bob|Aileen|Ina|Moc|Preston|Name|Sat|Sun|COPY|Reserve|RESERVE|
                                |Conference|Did Orientation|Need to Email|Could be |Chuck|Tutor|tutor|
                                |Wilson|Katie|Rachelle|TH|M or|Drop|TBD|Permanent|withdrew|term began|Sub|
                                |College Park|izzy|Returning|Rebecca Stewart|Lauren Mai|Meewa"))

#Potentially shift over some of the rows with people names - possibly just column issue and not mistake

df <- df %>% mutate(col1 = str_remove(col1, pattern = "now |1A\\-II \\(NOW |was 2A\\-II"))

#Need to finish going through col1_contents to either replace with NA to improve filling in or 
# to delete row after filling in process. Almost there!!!



## CODE GRAVEYARD ##

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
# 
# 
# 
# #Need to figure this out
# #For certain
# 
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
# 
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
    
   