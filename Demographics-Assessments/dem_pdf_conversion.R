library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(tidyr)

# IMPROVED VERSION OF STUDENT DEMOGRAPHIC DATA, now that I know how extract data better # 

setwd("C:/Users/602770/downloads/volunteer/wec/Students/Core-Demographics/Basics/Raw")

files <- list.files(pattern="*.pdf")


for (file in files) {
  
### GET TEXT ### 
  pdf_text <- extract_text(file, encoding='UTF-8')
  
  #Keeping Latino from being separated from 'Hispanic or' and causing issues
  pdf_text <- str_replace_all(pdf_text, pattern = "\r\nLatino\r\n", replacement="Latino ")
  
  #Rewording Hispanic or Latino to make cleaning more efficient
  pdf_text <- str_replace_all(pdf_text, pattern="Hispanic or Latino", replacement = "Hispanic/Latino")
  
  #Making multi-word employment status one unit and adding in space to separate from years of education
  pdf_text <- str_replace_all(pdf_text, pattern="Not ", replacement = " Not-")
  pdf_text <- str_replace_all(pdf_text, pattern="Full-Time", replacement = " Full-Time")
  pdf_text <- str_replace_all(pdf_text, pattern="Part-Time", replacement = " Part-Time")
  pdf_text <- str_replace_all(pdf_text, pattern="Looking", replacement = " Looking")
  
  #Splitting giant string into rows and creating dataframe
  #We want to split on \r\n and a digit but can't because str_split gets rid of the digit and we need it 
  #Will have to take of it as a dataframe
  
  pdf_text <- as.data.frame(str_split(pdf_text, pattern="\r\n"), stringsAsFactors = FALSE)
  
  colnames(pdf_text) <- "data"
  
  #Messy data necessitates messy code at times
  #We need to filter out any rows without data by name since we have rows with data that don't start with
  #student id due to the pdf formatting
  
  pdf_text <- pdf_text %>% filter(!str_detect(data, pattern = "Washington|Student|Last|School|Fisrt|First|w/|under|Low|Income|Employment|Status|Years|Education|Page"))
  pdf_text <- pdf_text %>% filter(data != "" & !is.na(data))
  
 
   #Taking care of names in own row
   
   length_pdf_text <- nrow(pdf_text) -1
 
   
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
 
  pdf_text <- pdf_text %>% filter(!str_detect(data, pattern = "SKIP"))
 
  #Getting the semester and year info from the file name
  pdf_text <- pdf_text %>% mutate(semester=str_to_upper(unlist(str_split(file, "_"))[1]))
  pdf_text <- pdf_text %>% mutate(year=unlist(str_split(unlist(str_split(file, "\\."))[1], "_"))[2])
  

### EXTRACT DATA FROM STRINGS  
  df <- pdf_text %>% separate(data, c("student_id", "rest"), sep=" ", extra="merge")
  
  df <- df %>% separate(rest, c("half_one", "half_two"), sep = "Yes |No ", extra = "merge")
  
  #One more separate to deal with Page 1 of 5 info. 
  df <- df %>% separate(half_two, c("half_two", "trash"), sep = "Page", extra = "drop")
  
  ## HALF ONE
  
  #Age
  df <- df %>% mutate(age = str_extract(half_one, pattern = "\\d\\d"))
  
  #Gender
  df <- df %>% mutate(gender = str_extract(half_one, pattern = "Male|Female"))
  
  #Ethnicity
  df <- df %>% mutate(ethnicity = str_extract(half_one, pattern = "African|Asian|Asian/Pacific|Black|Hispanic/Latino|Multi-Racial|Non\\-Hispanic/Latino|Spanish|White"))
  
  #Name
  df <- df %>% separate(half_one, c("name"), sep = "\\d\\d|Female|Male|Unknown|\\d", extra = "drop")
  
  
  ## HALF TWO
  
  #Zip Code
  df <- df %>% mutate(zip_code = str_extract(half_two, pattern = "\\d\\d\\d\\d\\d"))
  
  #Years of Education
  df <- df %>% mutate(half_two = str_pad(half_two, 3, side='left', pad = " "))
  df <- df %>% mutate(education_years = str_extract(half_two, pattern = " \\d\\d| \\d"))
  df <- df %>% mutate(education_years = str_squish(education_years))
  
  #Employment
  df <- df %>% mutate(employment_status = str_extract(half_two, pattern = "Full-Time|Part-Time|Looking|Not Applicable|Not-Employed"))
  
  #Getting rid of unneeded columns and rearranging
  df <- df %>% select(student_id, name, age, gender, ethnicity, employment_status, 
                      zip_code, education_years, semester, year)
  
  
  #Putting it all together
  if (file == "fall_2000.pdf") {
    df_final <- df
    
  #Weird case - I guess if you do just an if else statement, the else has to be on the same line as the 
  # end of the if curly brackets 
  } else { 
    
    df_final <- bind_rows(df_final, df)
      
  }

}
  

df_final <- df_final %>% mutate(age = as.numeric(age), education_years = as.numeric(education_years))

glimpse(df_final)




  
### CODE GRAVEYARD ### 

#Isolating rows with data
# pdf_text <- pdf_text %>% filter(str_detect(data, pattern="\\d\\d\\d\\d\\d"))

  #Unfortunately in the raw text there is no indication of where last name(s) and first name(s) start. 
  #We have the student id and will do what we can but will remember that this is the best solution to a not ideal
  # situation. Itsa okay! Mario!
  
  # #Homemade for loop: ensures age is either actual age or NA and that "rest" column has all same columns
  # for (i in 1:nrow(df)) {
  #   
  #   #Cases where messy is male or female (indicates NA for Age)
  #   #Add gender to "rest" column and replace age with NA
  #   if (str_detect(df$messy[i], pattern="Female")) {
  #     
  #     df$messy[i] <- NA_character_
  #     df$rest[i] <- paste("Female", df$rest[i])
  #     
  #   }
  #   
  #   #Add gender to "rest" column and replace age with NA
  #   else if (str_detect(df$messy[i], pattern="Male")) {
  #     
  #     df$messy[i] <- NA_character_
  #     df$rest[i] <- paste("Male", df$rest[i])
  #     
  #   }
  #   
  #   #Cases where individual has more than one first and last name
  #   else if (str_detect(df$messy[i], pattern="[:alpha:]")) {
  #     
  #     #Adds name to first name column
  #     df$first_name[i] <- paste(df$first_name[i], df$messy[i])
  #     
  #     df$messy[i] <- str_extract(df$rest[i], pattern="\\d\\d ")
  #     
  #     #Turns out str_extract is more ctrl+C vs. ctrl+X
  #     df$rest[i] <- str_remove(df$rest[i], pattern="\\d\\d ")
  #     
  #   }
  #   
  #   else {
  #   }
  # }
  # 
  # 
  # df <- df %>% rename(age = messy) 
  # 
  # 
  # df <- df %>% separate(rest, c("gender", "rest"), sep=" ", extra="merge")
  # 
  # 
  # #Since we won't use info on whether student has child under 22 (mostly NAs),
  # # we use separate and then replace Yes/No with NA
  # df <- df %>% separate(rest, c("ethnicity", "rest"), sep=" ", extra="merge")
  # 
  # #Padding values to allow for more precision in str_replace
  # df <- df %>% mutate(ethnicity = str_pad(ethnicity, 4, side="right", pad=" "))
  # df <- df %>% mutate(ethnicity = str_replace(ethnicity, pattern="Yes |No ", replacement = NA_character_))
  # 
  # #Getting rid of extra spaces
  # df <- df %>% mutate(ethnicity = str_squish(ethnicity)) #df[217,"ethnicity"]
  # 
  # 
  # #Realized rather than using separate it will be easier to simply extract values - this way we don't have to 
  # # manually account for NAs for each variable
  # 
  # #Zip Code
  # df <- df %>% mutate(zip_code = str_extract(rest, pattern="\\d\\d\\d\\d\\d"))
  # 
  # #Years of Education
  # df <- df %>% mutate(rest = str_pad(rest, 40, side="right", pad = " "))
  # df <- df %>% mutate(education_years = str_extract(rest, pattern = " \\d | \\d\\d "))
  # 
  # #Employment Status
  # df <- df %>% mutate(employment_status = str_extract(rest, pattern="Full-Time|Part-Time|
  #                                                     Looking|Not Applicable|Not-Employed"))
  #          
