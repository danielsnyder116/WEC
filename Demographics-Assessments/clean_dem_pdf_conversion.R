library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)

# IMPROVED VERSION OF STUDENT DEMOGRAPHIC DATA, now that I know how extract data better # 

setwd("C:/Users/602770/downloads/volunteer/wec/Students/Core-Demographics/Basics/Raw/student-data_raw")

files <- list.files(pattern="*.pdf")[1]

#for (file in files) {
  
  pdf_text <- extract_text(file, encoding='latin-1')
  
  #Keeping Latino from being separated from 'Hispanic or' and causing issues
  pdf_text <- str_replace_all(pdf_text, pattern = "\r\nLatino\r\n", replacement="Latino")
  
  pdf_text <- as.data.frame(str_split(pdf_text, pattern="\r\n"))
  
  colnames(pdf_text) <- "Data"
  
  
#}
