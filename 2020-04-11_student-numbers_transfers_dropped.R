library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Totals_Transfers_Dropped")


files <- list.files()


for (file in files) {

  pdf_text <- extract_text(file, encoding="UTF-8")
  pdf_text <- as.data.frame(str_split(pdf_text, pattern="\r\n"))
  
  colnames(pdf_text) <- "data"
  
  #Filter out rows without student coutns and garbage 
  pdf_text <- pdf_text %>% filter(str_detect(data, pattern="\\d") & !str_detect(data, pattern="Page"))
  
  
  #Creating Semester and Year Column - get first result from first row ([1]) and set that for all rows
  df <- pdf_text %>% mutate(semester=str_extract(data, pattern="Spring|Summer|Fall|Winter")[1],
                                  year=str_extract(data, pattern="20\\d\\d")[1])
  
  #Get rid of first row and last row now that we have the information we need
  df <- df %>% slice(-1, -nrow(df))
  
  
  #Sort data into columns
  df <- df %>% separate(data, c("A", "B"), sep=" ", fill="right", extra="merge")
  
  
  #This is a darn ugly function, but it works. Essentially splitting the messy data in
  # column B and grabbing each element from the end and adding the remaining elements 
  # to the class name to make everything right.
  
  for (i in 1:nrow(df)) {
    
    
    if (length(unlist(str_split(df$B[i], pattern=" "))) >= 6) {
      
      number_elements <- length(unlist(str_split(df$B[i], pattern=" ")))
      
      df$total_total[i] <- unlist(str_split(df$B[i], pattern=" "))[number_elements]
      df$dropped[i] <- unlist(str_split(df$B[i], pattern=" "))[number_elements - 1]
      df$transferred[i] <- unlist(str_split(df$B[i], pattern=" "))[number_elements - 2]
      df$total_active[i] <- unlist(str_split(df$B[i], pattern=" "))[number_elements - 3]
      
      df$A[i] <- paste0(df$A[i], "-",  unlist(str_split(df$B[i], pattern=" "))[number_elements - 5], "-",
                                       unlist(str_split(df$B[i], pattern=" "))[number_elements - 4])
    }
      
    else if (length(unlist(str_split(df$B[i], pattern=" "))) == 5){
      
      df$total_total[i] <- unlist(str_split(df$B[i], pattern=" "))[5]
      df$dropped[i] <- unlist(str_split(df$B[i], pattern=" "))[4]
      df$transferred[i] <- unlist(str_split(df$B[i], pattern=" "))[3]
      df$total_active[i] <- unlist(str_split(df$B[i], pattern=" "))[2]
      
      df$A[i] <- paste0(df$A[i], "-",  unlist(str_split(df$B[i], pattern=" "))[1])
    }
  
    else {
      
      df$total_total[i] <- unlist(str_split(df$B[i], pattern=" "))[4]
      df$dropped[i] <- unlist(str_split(df$B[i], pattern=" "))[3]
      df$transferred[i] <- unlist(str_split(df$B[i], pattern=" "))[2]
      df$total_active[i] <- unlist(str_split(df$B[i], pattern=" "))[1]
    }
    
  }
  
  #Cleaning up A
  df <- df %>% mutate(class_name = str_sub(A, start=3))
  df <- df %>% mutate(class_name = str_remove(class_name, "ekend"))
  
  
  #Dropping messy columns
  df <- df %>% select(-B, -A)
  
  df <- df %>% select(class_name, semester, year, total_active, dropped, transferred, total_total)
  
  
  if (file == "CrystalViewer (1).pdf"){
    df_final <- df
  }
  
  else {
    
    df_final <- bind_rows(df_final, df)
  }

}

nrow(df_final)

#Getting rid of duplicates
df_final <- distinct(df_final)

#Sorting data by year, descending
df_final <- arrange(df_final, desc(year))


write.csv(df_final, "../../Database/2000-2019_student-numbers_includes-dropped-transferred.csv", row.names=FALSE)



# df <- df %>% mutate(total_total=str_split(B, pattern=" ")[[4]])
# 
# df <- df %>% separate(A, c("Class 1", "Mess"), sep=" ",)
# 
# df <- df %>% mutate(class_2 = str_split(Mess, pattern=" ", n=3))
# 
# 
# #Create class name column
# for (i in 1:nrow(df)) {
#   
#   #No extra number in class level
#   if (is.na(df$F[i])){
#   }
#   
#   else if (str_detect(df$B[i], pattern="[:alpha:][:alpha:]")){
#     df$A[i] <- paste0(df$A[i], "-", df$B[i], df$C[i])
#     
#   }
#   
#   else {
#     df$A[i] <- paste0(df$A[i],"-", df$B[i])
#     
#   }
#     
# }
# 
# test <- df %>% filter(is.na(F)) %>% lag(1)
# new <- df %>% filter((G)) %>% lag(2)
