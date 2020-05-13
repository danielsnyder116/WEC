library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(tidyr)

# IMPROVED VERSION OF STUDENT DEMOGRAPHIC DATA, now that I know how extract data better # 

setwd("C:/Users/602770/downloads/volunteer/wec/Students/Core-Demographics/Basics/Raw/student-data_raw")

files <- list.files(pattern="*.pdf")[1]

#for (file in files) {
  
  pdf_text <- extract_text(file, encoding='UTF-8')
  
  #Keeping Latino from being separated from 'Hispanic or' and causing issues
  pdf_text <- str_replace_all(pdf_text, pattern = "\r\nLatino\r\n", replacement="Latino ")
  
  #Rewording Hispanic or Latino to make cleaning more efficient
  pdf_text <- str_replace_all(pdf_text, pattern="Hispanic or Latino", replacement = "Hispanic/Latino")
  
  #Separating years of education with employment status
  pdf_text <- str_replace_all(pdf_text, pattern="Not ", replacement = " Not-")
  
  
  #Splitting giant string into rows and creating dataframe
  pdf_text <- as.data.frame(str_split(pdf_text, pattern="\r\n"))
  
  colnames(pdf_text) <- "data"
  
  #Isolating rows with data
  pdf_text <- pdf_text %>% filter(str_detect(data, pattern="\\d\\d\\d\\d\\d"))
  
  #Getting the semester and year info from the file name
  pdf_text <- pdf_text %>% mutate(semester=str_to_upper(unlist(str_split(file, "_"))[1]))
  pdf_text <- pdf_text %>% mutate(year=unlist(str_split(unlist(str_split(file, "\\."))[1], "_"))[2])
  
  
  
  df <- pdf_text %>% separate(data, c("student_id", "last_name", "first_name", "messy", "rest"), sep=" ", extra="merge")
  
  
  pdf_text[1,]
  
#}
  


           
