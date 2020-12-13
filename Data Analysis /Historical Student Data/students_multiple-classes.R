library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Multiple-Classes")

files <- list.files()[33]

pdf_text <- extract_text(files, encoding="UTF-8")

pdf_text <- as.data.frame(str_split(pdf_text, pattern="\r\n"))

colnames(pdf_text) <- "data"

#Creating Semester and Year Column
df <- pdf_text %>% mutate(semester=str_extract(data, pattern="Summer|Fall|Winter|Spring")[4],
                          year=str_extract(data, pattern="\\d\\d\\d\\d")[4])

#Filtering for student lines
df <- df %>% filter(str_detect(data, pattern="\\d\\d\\d\\d\\d"))


#Creating columns - need to CLEAN THIS UP
df <- df %>% separate(data, c("student_id", "last_name","first_name", "age", 
                              "gender","ethnicity", "rest"), sep=" ", extra = "merge")
