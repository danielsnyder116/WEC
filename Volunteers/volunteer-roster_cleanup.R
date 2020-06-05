library(dplyr)
library(stringr)
library(glue)
library(readxl)
library(purrr)

setwd("/Users/Daniel/Desktop/volunteer-files/")

#Reading in files and standardizing capitalization
files <- str_to_upper(list.files(pattern = "*.xls*", recursive=TRUE))

#We are filtering out the workbooks that are not of use to us
roster_files <- str_subset(files, pattern = "AUTOSAVED|BOOK|CANDIDATES|COPY|DO NOT USE|EMAIL|LIST|LOG|OLD|
                                             |POTENTIAL|RETURNING|SHOW|SIGN|SUB|TECH|
                                             |TRAINING|TUTOR|\\~|\\(1\\)", negate = TRUE)

#str_subset is a wrapper around files[str_detect(files, pattern = "Log")]

#This also works#Keep takes in list and a function to apply as a filter
#roster_files <- files %>% keep(., str_detect(files, pattern = "List|Log|
                                    #|Potential|Show|Sign|sign|Training|Tutor", negate = TRUE))

for (roster in roster_files) {

  #Better way is to simply combine sheets to avoid messy workbook issues
  #Need to use "function (x)" to be able to set parameters of read_excel
  #Doing this makes it unnecessary to put "path = roster" for the second argument of map()
  print(roster)
  raw <- roster %>% excel_sheets() %>% set_names() %>%
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
  
  #print(paste0(roster, "\n:", file_semester, " ", file_year))
  
  #print(paste0(roster, "\n", head(raw)))
  
  }
   
   #Putting everything together
   if (str_detect(roster, pattern = "FALL 06 TEACHERS EVENINGS")) {
     df <- raw
     
   } else {
     df <- bind_rows(df, raw)
   }
   
    
}

#Alrighty, now it's cleanin' time
#Using new dplyr verb across to filter specific columns that are not na
#Getting rid of rows that are all NAs

nrow(df)
df <- df %>% filter(across(col1:col3, ~!is.na(.)))

nrow(df)

#Getting rid of data where it is just a list of volunteer emails or other irrelevant text
col1_contents <- unique(df$col1)

nrow(df)
#Getting rid of spaces
#df <- df %>% mutate(col1 = str_to_upper(str_remove_all(col1, " ")))

#Getting rid of punctuation
df <- df %>% mutate(col1 = str_squish(str_remove_all(col1, pattern = "^[:punct:]")))

#Replacing empty col1 with NA separate from rest of editing
df <- df %>% mutate(col1 = na_if(col1, ""))

#We replace irrelevant values with NA to make it easier to fill in info.

#Need to figure this out
#For certain

df <- df %>% filter(str_replace_all(col1, pattern = "Level|Their|Emailed|Emld|Will be|Sent|\\d\\d\\d+|must|
                                    |Room|Syllabus|Jaw|Mich|Elis|Andr|Lori|of|Moc|winter|Charles|Sula|
                                    |Erin|Jess|Camer|Elsa|Brain|workbook|shares|Satur|April|12|No class|
                                    |https|WHITE|Summer|Preston|Monday|Georgetown|Bob|\\||Name|Aileen|
                                    |Ina|11AM|9|2\\-4PM"),
                                     replacement = NA_character_)
starts

nrow(df)

# glimpse(df)
# 
# unique(df$col1)






## CODE GRAVEYARD ##

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
    
   