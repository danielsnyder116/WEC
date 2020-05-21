library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/602770/downloads/volunteer/wec/Students/Core-Demographics/Country-of-Origin/Raw")

files <- list.files(pattern = "*.pdf")


#This loop takes around 10 minutes to run
for (file in files) {

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
 #Has to be one less than nrow to make sure we don't do an extra row due to +1 
 #Extra rows are automatically NAs which will throw an error.
    if ( nrow(pdf_text) == 0) {
       
     } else if (nrow(pdf_text) > 0 & nrow(pdf_text) <= 3) {
       
       length_pdf_text <- 3
       
       pdf_text <- add_row(pdf_text, data = " ")
       pdf_text <- add_row(pdf_text, data = " ")
       
    } else {
       length_pdf_text <- nrow(pdf_text) - 1
       }

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
 
 
 #Round 2
 for (i in 2:length_pdf_text) { 
   
    #If we detect a SKIP and the row after is still not a student id (extra data)
    if (str_detect(pdf_text$data[i], pattern = "SKIP") & 
        str_detect(pdf_text$data[i+1], pattern = "^\\(\\d|^[:alpha:]")) {
 
      #We do the same thing but add the data two rows up
      pdf_text$data[i-1] <- paste(pdf_text$data[i-1], pdf_text$data[i+1])
      
      #If there is additional info, we replace this with SKIP for removal
      pdf_text$data[i+1] <- "SKIP"

 
    }
 }
 
 #Cleaning up items (getting rid of SKIP rows and dividing up long string of data)
 df <- pdf_text %>% filter(!str_detect(data, pattern = "SKIP"))
 
 df <- df  %>% separate(data, c("student_id", "more_rest"), extra = "merge", sep = " ")
 
 df <- df %>% separate(rest, c("trash", "teacher","class_name", "more_trash"), extra = "merge", sep = ":")
 
 #Creates teacher column and gets rid of session junk
 df <- df %>% mutate(teacher = str_remove(teacher, pattern = " Class Name"),
                     class_name = str_remove(class_name, pattern = " AMSession| PMSession| WeekendSession"))
 
 
 
 #Gets rid of unnecessary punctuation and phone numbers and leaves anchor to separate out for country
 df <- df %>% mutate(more_rest = str_replace_all(more_rest, pattern = "\\d|\\(|\\)|\\.|\\/|\\+|\\=|\\--| \\-", ""))
 
 
 #Create country column
 df <- df %>% separate(more_rest, c("name", "country"), sep = "  - ", extra = "merge")



 #Bring in official country names to aid in data cleaning
 countries <- read.csv("../countries-crosswalk.csv", stringsAsFactors = FALSE)
 countries <- countries %>% select(Country) %>% unlist(.) %>% as.vector(.) %>% str_trim(., side="both")
 
 #If a correct country name is within other gobbledygook in a row,
 # this double for loop replaces that row with just the country name
 # deals with the issue of misparsed names included as part of the country
 
 for (i in 1:length(countries)) {
   for (k in 1:nrow(df)) {
     if (str_detect(df$name[k], regex(countries[i], ignore_case = TRUE))) {
       df$country[k] <- countries[i]
       df$name[k] <- str_replace(df$name[k], pattern = countries[i], replacement = "")
     }
     
   }
 }
 
  #Clean Country Column
  df <- df %>% mutate(country = str_squish(str_remove_all(country, pattern = "^ |^  |\\-")))
  
  #Clean Name Column
  df <- df %>% mutate(name = str_remove_all(name, pattern = " \\-\\-"))
  
  df <- df %>% select(student_id, name, country, semester, year, class_name, teacher)

  
#Putting it all together
 
  if (file == "fall_2000_cc.pdf"){
      
      df_final <- df
      
      #Weird case - I guess if you do just an if else statement, the else has to be on the same line as the 
      # end of the if curly brackets 
      } else { 
      
      df_final <- bind_rows(df_final, df)
      
      }
 
 
}





