library(tidyverse)
library(janitor)

setwd("C:/Users/602770/downloads/volunteer/wec/messy_data")

#PART 1: Messy Data
#--------------------

#Gets all file names 
temp <- list.files()

#Reading in csvs all together (using lapply) and using rbindlist to delist and combine into dataframe
#bind_rows did not work due to issues with column types that rbindlist avoids

messy_data <- lapply(temp, read.csv, stringsAsFactors=FALSE) %>% rbindlist(., fill=TRUE)

#Only rows where the last name (X.1 column) is present
good_data <- messy_data[which(!is.na(messy_data$X.1)), ] %>% remove_empty(., which='cols')

colnames(good_data)

good_data <- good_data[, c(2:8,10:12,14:15)]

colnames(good_data) <- c("student_id", "last_name", "first_name","age",
                                         "gender", "ethnicity", "child_under_22","employment_status", 
                                         "zip_code","education_years", "semester","year")


#Gets rid of rows with blank strings that aren't NAs

bad_rows1 <- which(nchar(good_data$last_name) < 2 | nchar(good_data$student_id) < 7)
good_data <- good_data[-c(bad_rows1), ]


#Gets rid of rows with headers by checking if age column string is >2 (nchar("Age") == 3)
good_data$age <- str_squish(good_data$age)
bad_rows2 <- which(nchar(good_data$age) == 3)
good_data <- good_data[-c(bad_rows2), ]

#Formatting ethnicity and semester
good_data$ethnicity <- gsub("Hispanic or", "Hispanic/Latino", good_data$ethnicity)
good_data$semester <- toupper(good_data$semester)

good_data <- good_data[order(good_data[, c("last_name")]), ]

#write.csv(good_data, "../good_data.csv", row.names=FALSE)


#PART 2: Clean Data
#--------------------

setwd("C:/Users/602770/downloads/volunteer/wec/processed_data")

temp <- list.files()

df <- lapply(temp, read.csv, stringsAsFactors=FALSE)  %>% rbindlist(., fill=TRUE)

df <- df[, -c(1)]


bad_rows <- which((nchar(df$student_id) < 7) |
                  (nchar(df$last_name) < 2 ) |
                  (nchar(df$age) == 3) |
                  (df$student_id == "Student ID"))

df <- df[-c(bad_rows), ]


df$ethnicity <- gsub("Hispanic or", "Hispanic/Latino", df$ethnicity)
df$semester <- toupper(df$semester)

df <- df[order(df[, c("last_name")]), ]

df_final <- rbind(df, good_data)


write.csv(df, "../basic_demographics_2000_2019.csv", row.names=FALSE)


