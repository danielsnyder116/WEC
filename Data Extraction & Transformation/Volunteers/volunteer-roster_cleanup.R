library(dplyr)
library(stringr)
library(readxl)

setwd("/Users/Daniel/Desktop/Data")

#Gets all the excel files and deals with folders - 318
files <- as.data.frame(list.files(pattern = "*.xls*", recursive=TRUE), stringsAsFactors = FALSE)

colnames(files) <- 'paths'

roster_files <- files %>% filter(!str_detect(paths, pattern = "List|Log|Potential|Show|Sign|sign|Training|Tutor"))
other_files <- files %>% filter(str_detect(paths, pattern = "List|Log|Potential|Show|Sign|sign|Training|Tutor"))
#roster <- roster_files$paths[1]

for (roster in roster_files$paths) {

 #Lists all sheets in excel file - thanks Hadley!
  print(paste0(roster, ":", length(excel_sheets(roster))))
   
  if ()
  
  
    #Read in the file and get the first sheet name 
    raw <- read_excel(roster, skip = 1, sheet = excel_sheets(roster)[1])
    
    #Due to poor file names need to add in some logic to ensure files are uniform in column
    #names to ensure easy binding later
    
    if (ncol(raw) == 4 ) {
      colnames(raw) <- c("col1","col2", 'col3', 'col4')
    }
    
    else if (ncol(raw) == 3) {
    colnames(raw) <- c("col1", "col2")
    }
  
    else if (ncol(raw) == 2) {
      colnames(raw) <- c("col1", "col2")
    }
    
    else if (ncol(raw) == 1) {
      colnames(raw) <- c("col1")
    }
    
    else {
    #Only will need the first 4 columns for the info.
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



