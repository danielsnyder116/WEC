library(rJava)

dyn.load('Library/Java/JavaVirtualMachines/jdk-11.0.6.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(tabulizer)
library(dplyr)
library(stringr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[1]
files
for (file in files) {
  df <- extract_tables(file, output='data.frame', method='stream',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbind_list(df, fill=TRUE)))
}
for (file in files) {
  df <- extract_tables(file, output='data.frame', method='stream',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
}
View(df)
for (file in files) {
  df <- extract_tables(file, output='data.frame', method='stream',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=FALSE)))
}
View(df)
files <- list.files()[1]
files
for (file in files) {
  df <- extract_tables(file, output='data.frame', method='stream',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  df[] <- lapply(df, as.character)
}
for (file in files) {
  df <- extract_areas(file, output='data.frame', method='decide',
                      encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  df[] <- lapply(df, as.character)
}
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[1]
files
for (file in files) {
  df <- extract_tables(file, output='data.frame', method='decide',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  df[] <- lapply(df, as.character)
}
View(df)
files <- list.files()[1]
files
for (file in files) {
  df <- extract_tables(file, output='data.frame', method='lattice',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  df[] <- lapply(df, as.character)
}
View(df)
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[1]
files
for (file in files) {
  df <- extract_text(file, output='data.frame', method='lattice',
                     encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  df[] <- lapply(df, as.character)
}
for (file in files) {
  df <- extract_text(file, encoding='UTF-8')
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  df[] <- lapply(df, as.character)
}
for (file in files) {
  df <- extract_text(file, encoding='UTF-8')
  # df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  # df[] <- lapply(df, as.character)
}
View(df)
df
str_split(df, pattern='\n')
df <- str_split(df, pattern='\n')
View(df)
library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(data.table)
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[1]
files
for (file in files) {
  #Gets all the text as one long string
  #df <- extract_text(file, encoding='UTF-8')
  df <- extract_tables(files[i], output = "data.frame", method='decide',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  df[] <- lapply(df, as.character)
}
#str_split(df, pattern='\n')
library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(data.table)
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[1]
files
for (file in files) {
  #Gets all the text as one long string
  #df <- extract_text(file, encoding='UTF-8')
  df <- extract_tables(file, output = "data.frame", method='decide',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  df[] <- lapply(df, as.character)
}
#str_split(df, pattern='\n')
View(df)
for (file in files) {
  #Gets all the text as one long string
  #df <- extract_text(file, encoding='UTF-8')
  df <- extract_tables(file, output = "matrix", method='decide',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  df[] <- lapply(df, as.character)
}
View(Df)
View(df)
files <- list.files()[2]
files
for (file in files) {
  #Gets all the text as one long string
  #df <- extract_text(file, encoding='UTF-8')
  df <- extract_tables(file, output = "data.frame", method='decide',
                       encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  df[] <- lapply(df, as.character)
}
library(miniUI)
files <- list.files()[2]
files
for (file in files) {
  #Gets all the text as one long string
  #df <- extract_text(file, encoding='UTF-8')
  df <- extract_areas(files[i], output = "data.frame", method='decide',
                      encoding='UTF-8', stringsAsFactors=FALSE)
  # df <- extract_tables(file, output = "data.frame", method='decide',
  #                      encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  df[] <- lapply(df, as.character)
}
for (file in files) {
  #Gets all the text as one long string
  #df <- extract_text(file, encoding='UTF-8')
  df <- extract_areas(file, output = "data.frame", method='decide',
                      encoding='UTF-8', stringsAsFactors=FALSE)
  # df <- extract_tables(file, output = "data.frame", method='decide',
  #                      encoding='UTF-8', stringsAsFactors=FALSE)
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  df[] <- lapply(df, as.character)
}
View(df)
df <- extract_areas(file, output = "data.frame", method='decide',
                    encoding='UTF-8', stringsAsFactors=FALSE)
library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(data.table)
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[2]
files
for (file in files) {
  #Gets all the text as one long string - best option
  df <- extract_text(file, encoding='UTF-8')
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  df[] <- lapply(df, as.character)
}
str_split(df, pattern='\n')
file <- str_split(df, pattern='\n')
file[1]
file <- as.data.frame(str_split(df, pattern='\n'))
View(file)
file <- str_split(df, pattern='\n')
file
str_wrap(file)
print(str_wrap(file))
file <- str_split(df, pattern='\n') %>% str_to_lower(.)
file <- str_to_lower(str_split(df, pattern='\n'))
files <- list.files()[2]
files
for (file in files) {
  #Gets all the text as one long string - best option
  df <- extract_text(file, encoding='UTF-8')
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  df[] <- lapply(df, as.character)
}
file <- str_to_lower(str_split(df, pattern='\n'))
file
file <- str_split(df, pattern='\n')
file
str_to_lower(file)
typeof(file)
df
file <- str_split(df, pattern='\r\n')
file
str_split(df, pattern='\r\n')
str_extract(file, "Language")
str_extract(df, "Language")
str_detect(df, "Language")
length(file)
nrow(file)
file <- str_split(df, pattern='\r\n')
nrow(file)
length(file)
file
nrow(file)
file
length(file)
nchar(file)
ncol(file)
file <- as.vector(str_split(df, pattern='\r\n'))
ncol(file)
nrow(file)
length(file)
file
file <- as.dataframe(str_split(df, pattern='\r\n'))
file <- as.data.frame(str_split(df, pattern='\r\n'))
length(file)
nrow(file)
file %>% filter(., str_detect(., pattern='Language'))
colnames(file)
colnames(file) <- data
colnames(file) <- 'data'
colnames(file)
file %>% filter(., str_detect(data, pattern='Language'))
file %>% filter(., str_detect(data, pattern="Semester|ID|Class Name|Final Exam|
Attendance|Participation|Language|Exam|TOTAL"))
file %>% filter(., str_detect(data, pattern="Semester|Student's name|ID|Class Name|Final Exam|
Attendance|Participation|Language|Exam|TOTAL"))
file %>% filter(., str_detect(data, pattern="Semester|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|Exam|TOTAL")) %>%
  str_split(., pattern='Semester: Spring 2015')
result <- file %>% filter(., str_detect(data, pattern="Semester|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|Exam|TOTAL"))
View(result)
spread(result, data)
result <- file %>% filter( str_detect(data, pattern="Semester|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|Exam|TOTAL"))
result %>% filter( str_detect(data, pattern='Semester'))
result <- file %>% filter(str_detect(data, pattern="Semester:|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|Exam|TOTAL"))
result %>% filter( str_detect(data, pattern='Semester'))
result %>% filter( str_detect(data, pattern='Semester')) %>% mutate( semester=.)
result$semester <- result %>% filter( str_detect(data, pattern='Semester'))
View(result)
df_blank <- data.frame(semester=character(),
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
View(df_blank)
df_blank$semester <- result %>% filter( str_detect(data, pattern='Semester'))
new <- rbind(df_blank, result %>% filter( str_detect(data, pattern='Semester'))
             new <- rbind(df_blank, result %>% filter( str_detect(data, pattern='Semester')))
             new <- rbind(df_blank, result %>% filter( str_detect(data, pattern='Semester')))
             View(new)
             result %>% filter( str_detect(data, pattern='Semester')))
result %>% filter( str_detect(data, pattern='Semester'))
col < -result %>% filter( str_detect(data, pattern='Semester'))
col <- result %>% filter( str_detect(data, pattern='Semester'))
df_blank$semester <- col
df_blank %>% mutate( semester_col=semester)
df_blank <- df_blank %>% mutate( semester_col=semester)
file <- as.data.frame(str_split(df, pattern='\r\n'))
nrow(file)
colnames(file) <- 'data'
result <- file %>% filter(str_detect(data, pattern="Semester:|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|Exam|TOTAL"))
df_blank <- data.frame(semester=character(),
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
semester < - result %>% filter( str_detect(data, pattern='Semester'))
semester <- result %>% filter( str_detect(data, pattern='Semester'))
df_blank <- df_blank %>% mutate( semester_col=semester)
result <- file %>% filter(str_detect(data, pattern="Semester:|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|Exam|TOTAL"))
df_blank <- data.frame(data=character(),
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
semester <- result %>% filter( str_detect(data, pattern='Semester'))
semester
df_blank <- rbind(df_blank, semester)
class_name <- result %>% filter( str_detect(data, pattern='Class Name'))
class_name
class_name <- result %>% filter( str_detect(data, pattern='Class Name:'))
df_blank <- rbind(df_blank, semester)
class_name
df_blank <- rbind(df_blank, class_name)
result <- file %>% filter(str_detect(data, pattern="Semester:|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|Exam|TOTAL"))
df_blank <- data.frame(data=character(),
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
semester <- result %>% filter( str_detect(data, pattern='Semester'))
class_name <- result %>% filter( str_detect(data, pattern='Class Name:'))
df_blank <- cbind(df_blank, semester)
library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(data.table)
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[2]
files
for (file in files) {
  #Gets all the text as one long string - best option
  df <- extract_text(file, encoding='UTF-8')
  df <- as.data.frame(bind_rows(rbindlist(df, fill=TRUE)))
  #
  df[] <- lapply(df, as.character)
}
file <- as.data.frame(str_split(df, pattern='\r\n'))
nrow(file)
colnames(file) <- 'data'
for (file in files) {
  #Gets all the text as one long string - best option
  df <- extract_text(file, encoding='UTF-8')
}
file <- as.data.frame(str_split(df, pattern='\r\n'))
nrow(file)
colnames(file) <- 'data'
result <- file %>% filter(str_detect(data, pattern="Semester:|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|Exam|TOTAL"))
df_blank <- data.frame(semester=character(8),
                       student_name=character(8),
                       student_id=numeric(8),
                       class_name=character(8),
                       final_exam_score=numeric(8),
                       attendance=numeric(8),
                       participation=numeric(8),
                       language_score=numeric(8),
                       exam_score_five=numeric(8),
                       total_eval_score=numeric(8),
                       stringsAsFactors = FALSE)
semester <- result %>% filter( str_detect(data, pattern='Semester'))
class_name <- result %>% filter( str_detect(data, pattern='Class Name:'))
df_blank <- rbind(df_blank, semester)
df_blank$semester <- semester
df <- data.frame(semester=character(8),
                 student_name=character(8),
                 student_id=numeric(8),
                 class_name=character(8),
                 final_exam_score=numeric(8),
                 attendance=numeric(8),
                 participation=numeric(8),
                 language_score=numeric(8),
                 exam_score_five=numeric(8),
                 total_eval_score=numeric(8),
                 stringsAsFactors = FALSE)
df$semester <- result %>% filter( str_detect(data, pattern='Semester'))
class_name <- result %>% filter( str_detect(data, pattern='Class Name:'))
df$class_name <- result %>% filter( str_detect(data, pattern='Class Name:'))
View(df)
df$semester <- result %>% filter( str_detect(data, pattern='Semester'))
df$class_name <- result %>% filter( str_detect(data, pattern='Class Name:'))
df$student_name <- result %>% filter( str_detect(data, pattern="Student's name"))
df$student_id <- result %>% filter( str_detect(data, pattern='ID'))
df$final_exam_score <- result %>% filter( str_detect(data, pattern='Final Exam'))
df$attendance <- result %>% filter( str_detect(data, pattern='Attendance'))
result %>% filter( str_detect(data, pattern='Attendance'))
result %>% filter( str_detect(data, pattern='Attendance \\d'))
df$attendance <- result %>% filter( str_detect(data, pattern='Attendance \\d'))
df$participation <- result %>% filter( str_detect(data, pattern='Participation \\d'))
df$language_score <- result %>% filter( str_detect(data, pattern='Language \\d'))
df$exam_score_five <- result %>% filter(str_detect(data, pattern='Exam \\d'))
result %>% filter(str_detect(data, pattern='Exam \\d'))
result %>% filter(str_detect(data, pattern='Exam'))
df$total_eval_score <- result %>% filter(str_detect(data, pattern='TOTAL \\d'))
df$promotion_class <- df$total_eval_score > 15
df$promotion_class <- df %>% filter(total_eval_score > 15)
df$total_eval_score
str_split(df$total_eval_score, " ")
str_split(str(df$total_eval_score), " ")
separate(df, total_eval_score, col1, col2, sep=" ")
separate(df, total_eval_score, col1, sep=" ")
separate(df, total_eval_score, promotion_class, sep=" ")
separate(df, total_eval_score, df$promotion_class, sep=" ")
separate(df, total_eval_score, df$semester, sep=" ")
str_split_fixed(df$total_eval_score, " ")
str_split_fixed(df$total_eval_score, " ", 2)
str_split_fixed(df$total_eval_score, " ", '2')
str_split_fixed(df$total_eval_score, " ", 3)
str_split_fixed(df$total_eval_score, " ", n=2)
glimpse(df)
df <- as.data.frame(df)
glimpse(df)
colnames(df)
library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[2]
#Will eventually write loop that performs same actions on every pdf file
#Gets all the text as one long string - best option
pdf_text <- extract_text(file, encoding='UTF-8')
#Splitting on tab and inline to get all lines of text
pdf_text <- as.data.frame(str_split(pdf_text, pattern='\r\n'))
colnames(file) <- 'data'
#Isolating relevant information regarding assessments
pdf_text <- pdf_text %>% filter(str_detect(data, pattern="Semester:|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|TOTAL"))
#Creating empty dataframe to append pdf_text data to
library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[2]
#Will eventually write loop that performs same actions on every pdf file
#Gets all the text as one long string - best option
pdf_text <- extract_text(file, encoding='UTF-8')
#Splitting on tab and inline to get all lines of text
pdf_text <- as.data.frame(str_split(pdf_text, pattern='\r\n'))
colnames(pdf_text) <- 'data'
#Isolating relevant information regarding assessments
pdf_text <- pdf_text %>% filter(str_detect(data, pattern="Semester:|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|TOTAL"))
library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[2]
#Will eventually write loop that performs same actions on every pdf file
#Gets all the text as one long string - best option
pdf_text <- extract_text(file, encoding='UTF-8')
library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
setwd("C:/Users/602770/Downloads/volunteer/wec/Education-Impact/Data/Raw")
files <- list.files()[2]
file <- list.files()[2]
#Gets all the text as one long string - best option
pdf_text <- extract_text(file, encoding='UTF-8')
#Splitting on tab and inline to get all lines of text
pdf_text <- as.data.frame(str_split(pdf_text, pattern='\r\n'))
colnames(pdf_text) <- 'data'
files
#Gets all the text as one long string - best option
pdf_text <- extract_text(file, encoding='UTF-8')
#Splitting on tab and inline to get all lines of text
pdf_text <- as.data.frame(str_split(pdf_text, pattern='\r\n'))
colnames(pdf_text) <- 'data'
#Isolating relevant information regarding assessments
pdf_text <- pdf_text %>% filter(str_detect(data, pattern="Semester:|Student's name|ID
|Class Name|Final Exam|Attendance|Participation|Language|TOTAL"))
View(pdf_text)
pdf_text %>% filter(str_detect(data, pattern="Semester:"))
length(pdf_text %>% filter(str_detect(data, pattern="Semester:")))
nrow(pdf_text %>% filter(str_detect(data, pattern="Semester:")))
row_length <- nrow(pdf_text %>% filter(str_detect(data, pattern="Semester:")))
str_split_fixed(df$total_eval_score, " ", n=2)
result %>% filter( str_detect(pdf_text, pattern='Attendance \\d'))
glimpse(df)
df_blank <- data.frame(test=character())
df_blank <- bind_rows(df_blank, semester)
View(df_blank)
