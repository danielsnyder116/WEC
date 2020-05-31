library(dplyr)
library(stringr)
library(glue)
library(readxl)
library(purrr)

setwd("/Users/Daniel/Desktop/volunteer-files/")

files <- list.files(pattern = "*.xls*", recursive=TRUE)

#We are filtering out the workbooks that are not of use to us
roster_files <- str_subset(files, pattern = "List|Log|Potential|Show|Sign|sign|Training|Tutor", negate = TRUE)
#str_subset is a wrapper around files[str_detect(files, pattern = "Log")]

#This also works#Keep takes in list and a function to apply as a filter
#roster_files <- files %>% keep(., str_detect(files, pattern = "List|Log|Potential|Show|Sign|sign|Training|Tutor", negate = TRUE))
# roster <- roster_files[1]

for (roster in roster_files) {

  #Better way is to simply combine sheets to avoid messy workbook issues
  raw <- roster %>% excel_sheets() %>% set_names() %>%
                   #function applied, #object
               map(read_excel, path = roster) %>% bind_rows()
  

  #Due to poor file names need to add in some logic to ensure files are uniform in column
  #names to ensure easy binding later

  if (ncol(raw) == 4 ) {
    colnames(raw) <- c("col1","col2", 'col3', 'col4')
  }

  else if (ncol(raw) == 3) {
    colnames(raw) <- c("col1", "col2", "col3")
  }

  else if (ncol(raw) == 2) {
    colnames(raw) <- c("col1", "col2")
  }

  else if (ncol(raw) == 1) {
    colnames(raw) <- c("col1")
  }

  else {
    #Only will need the first 5 columns for the info.
    raw <- raw %>% select(1:5)
    colnames(raw) <- c("col1","col2", "col3", "col4", "col5")

  }
  
  
    
    #Adding in semester and year data
    semester_year <- unlist(str_split(roster, pattern = "\\/"))[2]
    file_semester <- str_extract(semester_year, pattern = "Fall|Winter|Spring|Summer")
    file_year <- str_extract(semester_year, pattern = "\\d\\d\\d\\d")
    
    raw <- raw %>% mutate(semester = str_to_upper(file_semester),
                          year = str_to_upper(file_year))
   
   #Putting everything together
   if (str_detect(roster, pattern = "Fall 2006")) {
     
     df <- raw
     
   } else {
     
     df <- bind_rows(df, raw)
   }
   
    
}


## CODE GRAVEYARD ##
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
    
   