library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(data.table)

setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")

#Total of 673 files (skipped 471 accidentally and threw off other numbers lol
#Spring 2015 through Fall 2019 Data - some semesters had little to no data
#No data on conversation classes which makes sense as no formal assessments in the class.)

files <- list.files(pattern="*.pdf")

#Loop that performs same actions on every pdf file
for (file in files) {
  
  #Gets all the text as one long string - best option
  pdf_text <- extract_text(file, encoding='latin1')
  
  #Splitting on tab and inline to get all lines of text
  pdf_text <- as.data.frame(str_split(pdf_text, pattern='\r\n'))
  colnames(pdf_text) <- 'data'
  
  #Isolating relevant information regarding assessments
  pdf_text <- pdf_text %>% filter(str_detect(data, 
                          pattern="Semester:|Student's name|ID Number|Class Name|Final Exam|Attendance|Participation|Language|TOTAL"))
  
  #Filtering each column
  semester <- pdf_text         %>% filter(str_detect(data, pattern='Semester')) 
  class_name <- pdf_text       %>% filter(str_detect(data, pattern='Class Name:'))
  student_name <- pdf_text     %>% filter(str_detect(data, pattern="Student's name")) 
  student_id <- pdf_text       %>% filter(str_detect(data, pattern='ID'))
  
  #Using 'Final Exam' for pattern search gets out of 100 score in case needed later
  final_exam_score <- pdf_text %>% filter(str_detect(data, pattern='Final Exam'))
  
  #No 'or condition' for attendance as there is unnecessary percentage information captured in the 5 point score format
  #However, when the other case isn't included there are times when there are no values for the column which throws an error
  
  #Getting rid of other, unwanted Attendance value by excluding it
  pdf_text <- pdf_text %>% filter(!str_detect(data, pattern = "Attendance %"))
  
  attendance <- pdf_text       %>% filter(str_detect(data, pattern='Attendance'))
  participation <- pdf_text    %>% filter(str_detect(data, pattern='Participation \\d|Participation'))
  language_score <- pdf_text   %>% filter(str_detect(data, pattern='Language \\d|Language'))
  total_eval_score <- pdf_text %>% filter(str_detect(data, pattern='TOTAL \\d|TOTAL'))
  
  # #Renaming each column so the merging/binding works smoothly
  colnames(semester) <- 'semester'
  colnames(class_name) <- 'class_name'
  colnames(student_name) <- 'student_name'
  colnames(student_id) <- 'student_id'
  colnames(final_exam_score) <- 'final_exam_score'
  
  #Some of the student evaluations have no info. in the box highlighting total score
  #calculations so need to add in NAs to allow for smooth binding
  #These four variables will almost always be the same amount
  
  colnames(attendance) <- 'attendance' 
  colnames(participation) <- 'participation'
  colnames(language_score) <- 'language_score'
  colnames(total_eval_score) <- 'total_eval_score'
  
  #Get the max number of rows
  row_length <- max(c(nrow(semester), nrow(class_name), nrow(student_name), 
                      nrow(student_id), nrow(final_exam_score), nrow(attendance), 
                      nrow(participation), nrow(language_score),nrow(total_eval_score)))

  #If the number of rows doesn't match semester's number of rows,
  #then we get the difference between the two and use that number with
  #tibble's add_row function to add the appropriate number of rows to the column  
  #For some reason can't get this to work in the loop so doing manually, bleh.
  
  number_semester <- (row_length - nrow(semester))
  if (number_semester > 0) {
    semester <- add_row(semester, semester=1:number_semester)
  }
  
  number_class <- (row_length - nrow(class_name))
  if (number_class > 0) {
    class_name <- add_row(class_name,class_name=1:number_class)
  }
  
  number_name <- (row_length - nrow(student_name))
  if (number_name > 0) {
    student_name <- add_row(student_name, student_name=1:number_name)
  }
  
  number_id <- (row_length - nrow(student_id))
  if (number_id > 0) {
    student_id <- add_row(student_id, student_id=1:number_id)
  }
  
  number_exam <- (row_length - nrow(final_exam_score))
  if (number_exam > 0){
    final_exam_score <- add_row(final_exam_score, final_exam_score=1:number_exam)
  }
  
  number_attendance <- (row_length - nrow(attendance))
  if (number_attendance > 0){
    attendance <- add_row(attendance, attendance=1:number_attendance)
  }
  
  # #Deals with case where no returns and so column value is integer(0)
  # if (length(number_attendance) == 0) {
  #   
  #   attendance <- attendance %>% as.character()
  #   attendance <- add_row(attendance, attendance=1:row_length)
  # }
  
  number_participation <- (row_length - nrow(participation))
  if (number_participation > 0) {
    participation <- add_row(participation, participation=1:number_participation)
  }
  
  number_language <- (row_length - nrow(language_score))
  if (number_language > 0) {
    language_score <- add_row(language_score, language_score=1:number_language)
  }
  
  number_total <- (row_length - nrow(total_eval_score))
  if (number_total > 0){
    total_eval_score <- add_row(total_eval_score, total_eval_score=1:number_total)
  } 
  
  
  #Creating list of columns to allow for efficient binding in a loop
  columns <- list(student_name, semester, class_name, final_exam_score, attendance,
                  participation, language_score, total_eval_score)
  
  #Creating empty dataframe to append pdf_text data to
  #Row number has to match
  df <- data.frame(data=numeric(), stringsAsFactors = FALSE)

  #Should use bind_rows and as it works with different values between the two dataframes, yippee!
  df <- bind_rows(student_id)
  
  #Appending each column to the dataframe
  #Not the case for bind_cols though
  for (column in columns) {
    df <- bind_cols(df, column)
    }
    
  #For the first file, we make df the base dataframe, called "df_all"
  #Otherwise, append the new dataframe to the base by using bind_rows
  #since columns are the same

  if (file == "001.pdf") {
    df_all <- df
  }

  else {
    df_all <- bind_rows(df_all, df)
  }
  
}


glimpse(df_all)

write.csv(df_all, "../Processed/2015-2019_intermediate_educational-performance-data.csv", row.names = FALSE)


#For some reason this (and trying for i in 1:nrow(columns)) didn't work
# for (column in columns) {
#   
#   print(column)
#   number_of_rows <- (row_length - nrow(column))
#   column <- add_row(column, colnames(column)=1:number_of_rows)
#   
#   #Appending each column to the dataframe
#   #Should use bind_rows and as it works with different values between the two dataframes, yippee!
#   #Not the case for bind_cols though
#   df <- bind_rows(student_id)
#   df <- bind_cols(df, column)
# }


#ALTERNATE EXTRACTION METHODS
#--------------------------------------------
# library(miniUI)
#Tedious and as effective as extract_text
# df <- extract_areas(file, output = "data.frame", method='decide',
#                     encoding='UTF-row_length', stringsAsFactors=FALSE)

#Ineffective
# df <- extract_tables(file, output = "data.frame", method='decide',
#                      encoding='UTF-row_length', stringsAsFactors=FALSE)



#ALTERNATE (Failed) DATAFRAME APPENDING METHOD
# columns <- list(semester, class_name, student_name,
#                 student_id, final_exam_score, attendance,
#                 participation, language_score, total_eval_score)
# 
# 
# #Appending each column to the dataframe
# #Should use bind_rows as it works with different values between the two dataframes, yippee!
# for  (column in columns) {
#   
#   df <- bind_rows(df, column)
# }
