library(rJava)
library(tabulizer)
library(tidyverse)
library(glue)
library(data.table)
#library(janitor)


setwd("C:/Users/602770/downloads/volunteer/wec/student_data")

files <- list.files(pattern="*.pdf")


for (i in 1:length(files)) {
  
  i <- 41
  
  df <- extract_tables(files[i], output = "data.frame", method='stream',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  
  #converts all columns to character type without having to then convert list to data frame - thanks Hadley!
  df[] <- lapply(df, as.character)
  
  
  #Getting rid of NA columns - uses Janitor package
  #But doesn't work well in this case as there is sparse data that we 
  #don't want to delete
  # df <- remove_empty(df, which="cols")
  
  #Separate Messy data for cleaning later
  extra <- df[, c(12:length(df))]
  
  #Hone in on clean data
  df <- df[, c(1:7,9:11)]
  
  
  colnames(df) <- c("student_id", "last_name", "first_name","age",
                    "gender", "ethnicity", "child_under_22","employment_status", 
                    "zip_code","education_years")
  
  #There is no equivalent of dropna(thresh=10) for R so will have to write a small loop
  #If the student ID is NA, we know it's an extra row so delete the entire row
  #Ask Martin if there is a better way
  
  bad_data_1 <- vector()
  
  #Gets rid of rows with blank strings that aren't NAs
  for (j in 1:nrow(df)) {
    
    if (is.na(df$student_id[j])) {
      bad_data_1[j] <- j
    }
  }
  
  bad_rows <- which(!is.na(bad_data_1))
  
  #Avoids error when there is no bad data to delete
  if (length(bad_data_1) > 0) {
    df <- df[-c(bad_rows), ]
  }
  
  
  df$ethnicity <- gsub("Hispanic or", "Hispanic/Latino",df$ethnicity)
  
  #Getting rid of extra spaces and blank values
  clean_up <- function(frame, column) {
    
    frame[, c(column)] <- str_trim(frame[, c(column)])
    
    #Filling in blanks with NAs
    for (i in 1:length(frame)){
      
      if ((frame[i, c(column)] == "") | (is.na(frame[i, c(column)]))) {
        
        frame[i, c(column)] <- NA
      }
     
    }
    
    return(frame[, c(column)])
  }
  
  # #For loop to run all of the columns through the clean_up function
  # column_names <- colnames(df)
  # 
  # for (k in 1:length(column_names)) {
  #   df[, c(column_names[k])] <- clean_up(df, column_names[k])
  #   
  # }
  
  #Splits the file name to get rid of pdf and then splits by "_" to get semester and year
  df$semester <- unlist(strsplit(unlist(strsplit(files[i], ".", fixed=TRUE))[1], "_"))[1]
  df$year <- unlist(strsplit(unlist(strsplit(files[i], ".", fixed=TRUE))[1], "_"))[2]
  
  
  #Student IDs vary between 7 and 10 digits - ugh
  # nchar(df$student_id)
  
  df$age <- as.numeric(df$age)
  df$education_years <- as.numeric(df$education_years)
  
  #Creates the file name with .csv extension
  file_name <- paste0(unlist(strsplit(files[i], ".", fixed=TRUE))[1], ".csv")
  
  write.csv(df, paste0("../processed_data/", file_name))
  
  print(glue("Successfully created {file_name}. Yippee!"))
  
  #-----------------------------------------------
  #Saves messy data if it contains real data
  if (class(extra) == "data.frame") {
    
    extra$semester <- unlist(strsplit(unlist(strsplit(files[i], ".", fixed=TRUE))[1], "_"))[1]
    extra$year <- unlist(strsplit(unlist(strsplit(files[i], ".", fixed=TRUE))[1], "_"))[2]
    
    messy_name <- paste0((unlist(strsplit(files[i], ".", fixed=TRUE)))[1],"_messy", ".csv")
    
    write.csv(extra, paste0("../messy_data/", messy_name))
    
    print(glue("Successfully created {messy_name}. Yippee!"))
  }

}
