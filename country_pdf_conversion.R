library(rJava)
library(tabulizer)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(glue)
library(data.table)
library(miniUI)
#library(janitor)


setwd("C:/Users/602770/downloads/volunteer/wec/country_class_data")

files <- list.files(pattern="*.pdf")


for (i in 1:length(files)) {
  
  i <- 1
  
  df <- extract_areas(files[i], output = "data.frame", method='decide',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  
  df <- extract_tables(files[i], output = "data.frame", method='decide',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  
  #converts all columns to character type without having to then convert list to data frame - thanks Hadley!
  df[] <- lapply(df, as.character)
  

  colnames(df) <- c("col1", "col2", "col3","col4",
                    "col5", "col6", "col7","col8", 
                    "col9", "col10", "col11", "col12",
                    "col13", "col14")
  

  
  #There is no equivalent of dropna(thresh=10) for R so will have to write a small loop
  #If the student ID is NA, we know it's an extra row so delete the entire row
  #Ask Martin if there is a better way
  
  left_data <- vector()
  center_data <- vector()
  right_data <- vector()

  
  #Gets rid of rows with blank strings that aren't NAs
  for (i in 1:nrow(df)) {
    
    #Getting left data
    if (!is.na(df$col1[i]) & !is.na(df$col4[i])) {
      left_data[i] <- i
    }
    
    #Getting center data
    if (is.na(df$col1[i]) & !is.na(df$col3[i])) {
      center_data[i] <- i
    }
    
    #Getting right side data
    if (is.na(df$col1[i]) & is.na(df$col3[i]) & !is.na(df$col9[i])) {
      right_data[i] <- i
    }
  }
  
  left_rows <- which(!is.na(left_data))
  center_rows <- which(!is.na(center_data))
  right_rows <- which(!is.na(right_data))
  
  
  if (length(left_rows) > 0) {
    df_left <- df[c(left_rows), ]
  }

  if (length(center_rows) > 0) {
    df_center <- df[c(center_rows), ]
  }
  
  if (length(right_side_rows) > 0) {
    df_right <- df[c(right_rows), ]
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
