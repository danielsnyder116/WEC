#-------------------------------
# ACADEMIC REPORTING: STUDENTS
#-------------------------------

#Th - 7:00 - 10:00 = 3
# Th - 10:30 - 2:00 = 3.5
#-------------------------------
#Establish a baseline for the % of students who score 75% or higher
# on the final exam (out of those who attend 70% or more classes)
#-------------------------------

library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)
library(tibble)
library(glue)
library(ggplot2)

#Identify data we have on final exam data

setwd("/Users/Daniel/Desktop/WEC/Data/Current (2020 - Present)/Students/")

#Bring in Attendance Data and make one file
att_files <- as_tibble(list.files("Attendance", recursive = TRUE)) %>% rename(file_path = value) %>%
                mutate(base_path = "/Users/Daniel/Desktop/WEC/Data/Current (2020 - Present)/Students/Attendance/",
                       full_path = paste0(base_path, file_path))

for (path in att_files$full_path){

  #GATHERING OTHER DETAILS FOUND IN FILE NAME AND PATH
  #-------------------------------------------------------
  class_time <- str_extract(path, pattern = "Copy of .*\\-") %>%
                str_remove("Copy of") %>% str_remove(" -") %>% str_squish() %>%
                str_replace("Intro A", "IntroA") %>% str_replace("Intro B", "IntroB")

  file_class <- str_split(class_time, " ")[[1]][1]
  file_time <- str_split(class_time, " ")[[1]][2]

  #Getting Semester and Year Info from File Name
  semester_year <- str_extract(path, pattern = "FALL \\d\\d\\d\\d|WINTER \\d\\d\\d\\d|
                                                |SUMMER \\d\\d\\d\\d|SPRING \\d\\d\\d\\d|
                                                |\\d\\d\\d\\d FALL|\\d\\d\\d\\d WINTER|
                                                |\\d\\d\\d\\d SUMMER|\\d\\d\\d\\d SPRING")

  file_semester <- str_extract(semester_year, pattern = "FALL|WINTER|SPRING|SUMMER")
  file_year <- str_extract(semester_year, pattern = "\\d\\d\\d\\d")

  if(path == att_files$full_path[1]) {

    #path <- files$full_path[10]
    df_final <- read_excel(path) %>% select(1,2,4,5) %>% slice(-c(1:2)) %>% mutate(semester = file_semester,
                                                                year = file_year,
                                                                class = file_class,
                                                                time = file_time)

    names(df_final) <- c("last_name", "first_name", "email", "att_percent", "semester", "year", "class", "time")
    print(df_final)
  }

  else {

    df_new <- read_excel(path) %>% select(1,2,4,5) %>% slice(-c(1:2)) %>% mutate(semester = file_semester,
                                                              year = file_year,
                                                              class = file_class,
                                                              time = file_time)

    names(df_new) <- c("last_name", "first_name", "email", "att_percent", "semester", "year", "class", "time")
    df_final <- bind_rows(df_final, df_new)


  }
}

#Cleaning
df_att <- df_final %>% drop_na(c("last_name")) %>% mutate(att_percent = as.numeric(att_percent)) %>%

              #Ensuring all percentages are in the same format by converting percents back to decimals
               mutate(att_percent = case_when(att_percent > 1 ~ (att_percent/100),
                                            TRUE ~ att_percent))

#Filter down to students who attend 70% or more of classes
df_thresh <- df_att %>% filter(att_percent >= .700)


#A little more than 1/4 of students attend 70% or more
nrow(df_thresh) / nrow(df_att)




#Bring in student performance data
# #---------------------------------------
# perf_files <-  as_tibble(list.files("Exams", recursive = TRUE)) %>% rename(file_path = value) %>%
#                       mutate(base_path = "/Users/Daniel/Desktop/WEC/Data/Current (2020 - Present)/Students/Exams/",
#                       full_path = paste0(base_path, file_path))
# 
# 
# for (path in perf_files$full_path){
#   
#   #print(path)
#   
#   #path <- perf_files$full_path[[30]]
#   
#   #GATHERING OTHER DETAILS FOUND IN FILE NAME AND PATH
#   #-------------------------------------------------------
#   file_class <- str_extract(path, pattern = "\\d\\d\\d\\d\\/.* Final") %>% 
#                 str_remove(".*\\/") %>% str_remove(" Final") %>% str_squish()
#   
#   #Getting Semester and Year Info from File Name
#   semester_year <- str_extract(path, pattern = "Fall \\d\\d\\d\\d|Winter \\d\\d\\d\\d|
#                                                 |Summer \\d\\d\\d\\d|Spring \\d\\d\\d\\d") %>%
#                    str_to_upper()
#   
#   file_semester <- str_extract(semester_year, pattern = "FALL|WINTER|SPRING|SUMMER")
#   file_year <- str_extract(semester_year, pattern = "\\d\\d\\d\\d")
#   
#   #Need to fix Fall 2020 data columns
#   if (semester_year == "FALL 2020") {
#     
#     df <- read.csv(path) %>% select(2,3,4,7)
#   }
#   
#   else {
#     df <- read.csv(path) %>% select(2:5)
#   }
#   
#   #Add in relevant columns
#   names(df) <- c("email","score", "first_name", "last_name")
#   df <- df %>% mutate(semester = file_semester,
#                       year = file_year,
#                       class = file_class) 
#       
#   
#   if(path == perf_files$full_path[1]) {
#     
#     df_final <- df
#   }
#   
#   else {
#     df_final <- bind_rows(df_final, df)
#   }
# }
# 
# df_perf <- df_final
# 
# 
# 
# #Separate out fake/false rows (test, teachers)
# df_perf <- df_perf %>% filter(email != "jebersole@washingtonenglish.org") %>%
#                        filter(email %in% df_att$email) %>% 
#                        mutate(class = str_remove_all(class, " "))
# 
# #Only 50% of students enrolled took the final exam
# nrow(df_perf) / nrow(df_att)
# 
# df_perf <- df_perf %>% mutate(score_new = str_extract(score, pattern = "\\d\\d"))
# df_perf <- distinct(df_perf) %>% select(-score) %>% rename(score = score_new)
# 
# 
# #Trying to make name uniform for join
# df_perf <- df_perf %>% mutate(last_name = str_squish(str_to_upper(last_name)))
df_att <- df_att %>% mutate(last_name = str_squish(str_to_upper(last_name)),
                            email = str_to_lower(email))

df_att <- df_att %>% mutate(last_name = case_when(str_detect(last_name, "DEVI") ~ "ACHARYA",
                                                    str_detect(last_name, "LUGO$") ~ "LUGO GARCIA",
                                                    str_detect(last_name, "NOUEL$") ~ "NOUEL DE VERAS",
                                                    str_detect(last_name, "CALDERÓN") ~ "CALDERON",
                                                    str_detect(last_name, "ROMAN CUELLAR") ~ "ROMAN",
                                                    str_detect(last_name, "J RODRIGUEZ INFANTE") ~ "RODRIGUEZ INFANTE",
                                                    str_detect(last_name, "NUNEZ") ~ "NUÑEZ",
                                                    str_detect(last_name, "COOK") ~ "ZAMORA COOK",
                                                    str_detect(last_name, "TELENOVA") ~ "TELNOVA",
                                                    str_detect(last_name, "GONAZALEZ") ~ "GONZALEZ",
                                                    str_detect(last_name, "HERNANDEZ RODRIGUEZ") ~ "HERNÁNDEZ RODRIGUEZ",
                                                    str_detect(last_name, "AREVALO DE RYKKEN") ~ "AREVALO C.",
                                                    str_detect(last_name, "LEMINE BEDAH") ~ "BEDAH",
                                                    str_detect(last_name, "GARCÍA") ~ "GARCIA",
                                                    str_detect(last_name, "INFANTE CAMPOS") ~ "INFANTE",
                                                    str_detect(last_name, "ARGUELLO") ~ "MARIN",
                                                    str_detect(last_name, "RODRÍGUEZ") ~ "RODRIGUEZ",
                                                    str_detect(last_name, "GIRALDO ALZATE") ~ "ALZATE",
                                                    str_detect(last_name, "RIVAS$") ~ "VALENCIA",
                                                    str_detect(last_name, "BASTUG$") ~ "BAŞTUĞ",
                                                    str_detect(last_name, "MOTATO$") ~ "MOTATO RAMIREZ",
                                                    str_detect(last_name, "WANTIER") ~ "WANTIER FOLLIET",
                                                    str_detect(last_name, "BELTRAN") ~ "BELTRAN CUELLAR",
                                                    str_detect(last_name, "LING$") ~ "WANG",
                                                    str_detect(last_name, "^SALAH$") ~ "BEN SALAH",
                                                    str_detect(last_name, "RODRIGUEZ-TORRES") ~ "RODRIGUEZ TORRES",
                                                    str_detect(last_name, "CASANOVA GONZALES") ~ "CASANOVA",
                                                    str_detect(last_name, "CANALES") ~ "MIRANDA CANALES",
                                                    str_detect(last_name, "RUIZ OTERO") ~ "RUIZ",
                                                    str_detect(last_name, "GOMEZ$") ~ "GOMEZ BARRIOS",
                                                    str_detect(last_name, "ACEVEDO CALLE") ~ "ACEVEDO",
                                                    str_detect(last_name, "FUNEZ$") ~ "FUNEZ LARIOS",
                                                    str_detect(last_name, "MEADE$") ~ "MEABE FRANZ",
                                                    str_detect(last_name, "ALVES") ~ "ANDRADE",
                                                    str_detect(last_name, "ELBUKHARI TAHA") ~ "TAHA",
                                                    str_detect(last_name, "MARTINEZ ARAYA") ~ "ARAYA",
                                                    str_detect(last_name, "^TOCA$") ~ "SANTIAGO TOCA",
                                                    str_detect(last_name, "FELIPE CLEVES DAZA") ~ "CLEVES",
                                                    str_detect(last_name, "MOTATO RAMIREZ") ~ "LUNA",
                                                    str_detect(last_name, "CORREDOR") ~ "CORREDOR PINEDA",
                                                    str_detect(last_name, "GUERRA SANDOVAL") ~ "GUERRA",
                                                    str_detect(last_name, "PINO SANDOVAL") ~ "PINO",
                                                  # str_detect(last_name, "") ~ "",
                                                  # str_detect(last_name, "") ~ "",
                                                  # str_detect(last_name, "") ~ "",
                                                  # str_detect(last_name, "") ~ "",
                                                  # str_detect(last_name, "") ~ "",
                                                  
                                                  
                                                  
                                                    TRUE ~ last_name
                                                    ),
                            
                             email = case_when(str_detect(email, "martinch25") ~ "calderon24nelly@gmail.com",
                                               str_detect(email, "ueno-takanori") ~ "jepic15264@gmail.com",
                                               str_detect(email, "olgutia") ~ "olguita.castillo987@gmail.com",
                                               (is.na(email) & str_detect(first_name, "Vera")) ~ "venera50000@gmail.com",
                                               (is.na(email) & str_detect(first_name, "Sylvia")) ~ "syl.azul@gmail.com",
                                               (is.na(email) & str_detect(first_name, "Maureen")) ~ "mjuica03@gmail.com",
                                               
                                               
                                               str_detect(email, "fraidoon") ~ "zarifi.romah@yahoo.com",
                                               str_detect(email, "gayemaec") ~ "gayemaec@yahoo.fr",
                                               str_detect(email, "rvargas1") ~ "olgaroerikvideocafe@hotmail.com",
                                               str_detect(email, "mecruzv@hotmail") ~ "mecruzv2015@gmail.com",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               # str_detect(email, "") ~ "",
                                               
                                               TRUE ~ email
                                                 
                                                 ))

#write.csv(df_perf, "df_perf needs to manually be cleaned of multiple scores.csv", row.names = FALSE)

df_perf <- read.csv("df_perf_final.csv", stringsAsFactors = FALSE) %>% 
                mutate(year = as.character(year),
                       email = str_to_lower(email))


#Does a decent job but will need to fill in the rest manually ugh
df_all <- left_join(df_perf, df_att, by=c("last_name","email", "semester", "year", "class"))  #%>%
            # select(-c("first_name.y", "email.y"))



#sum(is.na(df_all$att_percent)) / nrow(df_all)

#Filter down to students who attend 70% or more of classes
df_current <- df_all %>% drop_na(att_percent) %>% select(-c('first_name.y')) %>% 
                       rename(first_name = `first_name.x`) %>% 
                       filter(att_percent >= .7)

df_1519 <- read.csv("2015-2019_student-assessment-data.csv", stringsAsFactors = FALSE) %>%
                  select(c(semester, year, last_name, first_name, class_name, attendance, final_exam_score)) %>%
                  filter(attendance >=4) 

df_1519 <- df_1519 %>% mutate(success_indicator = case_when(final_exam_score >= 75 ~ "75 or Higher", 
                                                            final_exam_score < 75 ~  "Lower Than 75")) %>%
              drop_na(final_exam_score) %>% filter(final_exam_score > 0)
                 

df_1519_1 <- df_1519 %>% group_by(semester, year, success_indicator) %>% summarize(num_students = n())

df_1519_2 <- df_1519 %>% group_by(semester, year) %>% summarize(total_students = n())

df_1519_3 <- left_join(df_1519_1, df_1519_2, by=c("semester", "year")) %>% 
                      mutate(percent_students = paste0(round((num_students / total_students)*100, 0),"%")) %>%
                      mutate(year = as.character(year))

glimpse(df_1519_3)
df_final <- df_final %>% mutate(success_indicator = case_when(score >= 27 ~ "75 or Higher", 
                                                              score < 27 ~  "Lower Than 75")) %>%
            select(4,5,2,3,1,6,9,8, 7, 10)

df_1 <- df_final %>% group_by(semester, year, success_indicator) %>% summarize(num_students = n())

df_2 <- df_final %>% group_by(semester) %>% summarize(total_students = n())

df_3 <- left_join(df_1, df_2, by=c("semester")) %>% mutate(percent_students = paste0(round((num_students / total_students)*100, 0),
                                                                                     "%"))


df_ultimate <- bind_rows(df_1519_3, df_3) %>% filter(success_indicator == "75 or Higher") %>% 
                                mutate(semester = factor(semester, levels = c("WINTER", "SPRING", "SUMMER", "FALL"))) %>%
                                arrange(year, semester)

write.csv(df_ultimate, "Student Final Exam Performance Given Good Attendance FY2015 - FY2020.csv", row.names = FALSE)




#Get percentage of students who score 75 or higher on exam



