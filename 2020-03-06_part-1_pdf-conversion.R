library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)


setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")

file <- list.files()[2]

#Will eventually write loop that performs same actions on every pdf file

#Gets all the text as one long string - best option
pdf_text <- extract_text(file, encoding='UTF-row_length')

#Splitting on tab and inline to get all lines of text
pdf_text <- as.data.frame(str_split(pdf_text, pattern='\r\n'))
colnames(pdf_text) <- 'data'


#Isolating relevant information regarding assessments
pdf_text <- pdf_text %>% filter(str_detect(data, pattern="Semester:|Student's name|ID
                            |Class Name|Final Exam|Attendance|Participation|Language|TOTAL"))

row_length <- nrow(pdf_text %>% filter(str_detect(data, pattern="Semester:")))

#Creating empty dataframe to append pdf_text data to
#Row number has to match
df <- data.frame(semester=character(),
                       student_name=character(),
                       student_id=numeric(),
                       class_name=character(),
                       final_exam_score=numeric(),
                       attendance=numeric(),
                       participation=numeric(),
                       language_score=numeric(),
                       exam_score_five=numeric(),
                       total_eval_score=numeric(),
                       stringsAsFactors = FALSE)

#It works to bind_rows with different values yayyyyy!
df_blank <- data.frame(test=character())

df_blank <- bind_rows(df_blank, semester)

#Currently dependent on row number being the same, but using bind_rows gets around this

df$semester <- result %>% filter(str_detect(pdf_text, pattern='Semester'))
df$class_name <- result %>% filter(str_detect(pdf_text, pattern='Class Name:'))
df$student_name <- result %>% filter(str_detect(pdf_text, pattern="Student's name"))
df$student_id <- result %>% filter(str_detect(pdf_text, pattern='ID'))
df$final_exam_score <- result %>% filter(str_detect(pdf_text, pattern='Final Exam'))
df$attendance <- result %>% filter(str_detect(pdf_text, pattern='Attendance \\d'))
df$participation <- result %>% filter(str_detect(pdf_text, pattern='Participation \\d'))
df$language_score <- result %>% filter(str_detect(pdf_text, pattern='Language \\d'))
df$total_eval_score <- result %>% filter(str_detect(pdf_text, pattern='TOTAL \\d'))


#str_split_fixed(df$total_eval_score, " ", n=2)

glimpse(df)



#ALTERNATE EXTRACTION METHODS
#--------------------------------------------
# library(miniUI)
#Tedious and as effective as extract_text
# df <- extract_areas(file, output = "data.frame", method='decide',
#                     encoding='UTF-row_length', stringsAsFactors=FALSE)

#Ineffective
# df <- extract_tables(file, output = "data.frame", method='decide',
#                      encoding='UTF-row_length', stringsAsFactors=FALSE)


