library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/602770/downloads/volunteer/wec/Students/Core-Demographics/Country-of-Origin/Raw")

file <- list.files(pattern = "*.pdf")[19]

#for (file in files) {

# GET TEXT #

 pdf_text <- extract_text(file, encoding="UTF-8")
 
 pdf_text <- as.data.frame(str_split(pdf_text, pattern = "\r\n"), stringsAsFactors = FALSE)
  
 colnames(pdf_text) <- "data"
  
 pdf_text <- pdf_text %>% filter(!str_detect(data, pattern = "Washington|Student|School|Class Roster|Building|Home|Page"))
 
 pdf_text <- pdf_text %>% filter(data != "")
 
 #Getting the semester and year info from the file name
 pdf_text <- pdf_text %>% mutate(semester=str_to_upper(unlist(str_split(file, "_"))[1]))
 pdf_text <- pdf_text %>% mutate(year=unlist(str_split(unlist(str_split(file, "\\."))[1], "_"))[2])
 
 pdf_text <- pdf_text %>% mutate(rest = NA)
 
 for (i in 1:nrow(pdf_text)) {
   
   #If a row with class info. is detected
   if (str_detect(pdf_text$data[i], pattern = "Class Name")) {
     
   #We add that text to the row 
     pdf_text$rest[i] <- pdf_text$data[i]
     
   }
 }
 
 
 #Now we can use fill() to fill in the values 
 
 pdf_text <- pdf_text %>% fill(rest, .direction = "down")
 
 #Get rid of teacher rows now that we have what we need
 pdf_text <- pdf_text %>% filter(!str_detect(data, pattern = "Teacher"))
 
 
 #Taking care of names in own row
 
 length_pdf_text <- nrow(pdf_text) -1
 
 length_pdf_text
 

 #For each row, assuming the first row is fine
 for (i in 1:length_pdf_text) {
   
   #if the row doesn't start with a student id but previous one does
   if ( !str_detect(pdf_text$data[i+1], pattern = "\\d\\d\\d\\d\\d\\d\\d+|SKIP") & 
         str_detect(pdf_text$data[i], pattern = "\\d\\d\\d\\d\\d\\d\\d+")) {
     
     #We take this data and add it to the previous row
     pdf_text$data[i] <- paste0(pdf_text$data[i], pdf_text$data[i+1])
     
     #We then replace the data to make it easy to delete later. Deleting
     # in the middle of the loop causes issues
     pdf_text$data[i+1] <- "SKIP"
     
   }
   
 }
 
 for (i in 2:length_pdf_text) { 
   
    if (str_detect(pdf_text$data[i], pattern = "SKIP") & 
        str_detect(pdf_text$data[i+1], pattern = "^\\(\\d|^[:alpha:]")) {
 
      #We do the same thing but skip the skip row
      pdf_text$data[i-1] <- paste(pdf_text$data[i-1], pdf_text$data[i+1])
      
      #If there is additional info, we replace this with SKIP for removal
      pdf_text$data[i+1] <- "SKIP"

 
    }
 }
 
 df <- pdf_text %>% filter(!str_detect(data, pattern = "SKIP"))
 
 df <-pdf_text  %>% separate(data, c("student_id", "more_rest"), extra = "merge", sep = " ")
 
 
 
 
#}
