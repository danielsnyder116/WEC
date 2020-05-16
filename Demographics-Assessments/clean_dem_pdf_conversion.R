library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(tidyr)

# IMPROVED VERSION OF STUDENT DEMOGRAPHIC DATA, now that I know how extract data better # 

setwd("C:/Users/602770/downloads/volunteer/wec/Students/Core-Demographics/Basics/Raw")

file <- list.files(pattern="*.pdf")[5]

#for (file in files) {
  

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
  #We want to split on \r\n and a digit of some sort - this deals with cases where name is on multiple lines in pdf
  pdf_text <- as.data.frame(str_split(pdf_text, pattern="\r\n\\d"))
  
  colnames(pdf_text) <- "data"
  
  #Isolating rows with data
  pdf_text <- pdf_text %>% filter(str_detect(data, pattern="\\d\\d\\d\\d\\d"))
  
  #Getting the semester and year info from the file name
  pdf_text <- pdf_text %>% mutate(semester=str_to_upper(unlist(str_split(file, "_"))[1]))
  pdf_text <- pdf_text %>% mutate(year=unlist(str_split(unlist(str_split(file, "\\."))[1], "_"))[2])
  

### EXTRACT DATA FROM STRINGS  
  df <- pdf_text %>% separate(data, c("student_id", "rest"), sep=" ", extra="merge")
  
  df <- df %>% separate(rest, c("half_one", "half_two"), sep = "Yes |No ", extra = "merge")
  
  ## HALF ONE
  
  #Age
  df <- df %>% mutate(age = str_extract(half_one, pattern = "\\d\\d"))
  
  #Gender
  df <- df %>% mutate(gender = str_extract(half_one, pattern = "Male|Female"))
  
  #Ethnicity
  df <- df %>% mutate(ethnicity = str_extract(half_one, pattern = "African|Asian|Asian/Pacific|Black|Hispanic/Latino|Multi-Racial|Non\\-Hispanic/Latino|Spanish|White"))
  
  #Name
  df <- df %>% separate(half_one, c("name"), sep = "\\d\\d", extra = "drop" )
  
  
  ## HALF TWO
  
  #Zip Code
  df <- df %>% mutate(zip_code = str_extract(half_two, pattern = "\\d\\d\\d\\d\\d"))
  
  #Years of Education
  df <- df %>% mutate(education_years = str_extract(half_two, pattern = " \\d\\d| \\d"))
  
  #Name
  #df <- df %>% mutate(name = str_extract(rest, pattern = "\\w \\d\\d"))
  

#Unfortunately in the raw text there is no indication of where last name(s) and first name(s) start. 
#We have the student id and will do what we can but will remember that this is the best solution to a not ideal
# situation. Itsa okay! Mario!
  
#Homemade for loop: ensures age is either actual age or NA and that "rest" column has all same columns
  for (i in 1:nrow(df)) {
    
    #Cases where messy is male or female (indicates NA for Age)
    #Add gender to "rest" column and replace age with NA
    if (str_detect(df$messy[i], pattern="Female")) {
      
      df$messy[i] <- NA_character_
      df$rest[i] <- paste("Female", df$rest[i])
      
    }
    
    #Add gender to "rest" column and replace age with NA
    else if (str_detect(df$messy[i], pattern="Male")) {
      
      df$messy[i] <- NA_character_
      df$rest[i] <- paste("Male", df$rest[i])
      
    }
    
    #Cases where individual has more than one first and last name
    else if (str_detect(df$messy[i], pattern="[:alpha:]")) {
      
      #Adds name to first name column
      df$first_name[i] <- paste(df$first_name[i], df$messy[i])
      
      df$messy[i] <- str_extract(df$rest[i], pattern="\\d\\d ")
      
      #Turns out str_extract is more ctrl+C vs. ctrl+X
      df$rest[i] <- str_remove(df$rest[i], pattern="\\d\\d ")
      
    }
    
    else {
    }
  }
    
  
  df <- df %>% rename(age = messy) 
    
  
  df <- df %>% separate(rest, c("gender", "rest"), sep=" ", extra="merge")
  
  
  #Since we won't use info on whether student has child under 22 (mostly NAs),
  # we use separate and then replace Yes/No with NA
  df <- df %>% separate(rest, c("ethnicity", "rest"), sep=" ", extra="merge")
  
  #Padding values to allow for more precision in str_replace
  df <- df %>% mutate(ethnicity = str_pad(ethnicity, 4, side="right", pad=" "))
  df <- df %>% mutate(ethnicity = str_replace(ethnicity, pattern="Yes |No ", replacement = NA_character_))
  
  #Getting rid of extra spaces
  df <- df %>% mutate(ethnicity = str_squish(ethnicity)) #df[217,"ethnicity"]
  
  
  #Realized rather than using separate it will be easier to simply extract values - this way we don't have to 
  # manually account for NAs for each variable
  
  #Zip Code
  df <- df %>% mutate(zip_code = str_extract(rest, pattern="\\d\\d\\d\\d\\d"))
  
  #Years of Education
  df <- df %>% mutate(rest = str_pad(rest, 40, side="right", pad = " "))
  df <- df %>% mutate(education_years = str_extract(rest, pattern = " \\d | \\d\\d "))
  
  #Employment Status
  df <- df %>% mutate(employment_status = str_extract(rest, pattern="Full-Time|Part-Time|
                                                      Looking|Not Applicable|Not-Employed"))
  
  
  #Getting rid of unneeded columns and rearranging
  df <- df %>% select(student_id, last_name, first_name, age, gender, ethnicity, employment_status, 
                      zip_code, education_years, semester, year, rest)
  
  
  
  if (file == "fall_2000.pdf") {
    df_final <- df
    
  #Weird case - I guess if you do just an if else statement, the else has to be on the same line as the 
  # end of the if curly brackets ¯\_(ツ)_/¯.
  } else { 
    
    df_final <- bind_rows(df_final, df)
      
      }

#}
  


           
