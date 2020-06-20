library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

setwd("C:/Users/602770/Downloads/volunteer/wec/Volunteers/Employment_Birthday")

######## BASIC INFORMATION ########
file <- list.files()

#Thankfully the pdf is organized well so able to extract tables rather than starting at string level
pdf_text <- extract_tables(file[2], output="data.frame", method="stream",
                           encoding="UTF-8")

#Binding all the lists together
df <- as.data.frame(bind_rows(pdf_text))

#Getting rid of unnecessary columns
df <- df %>% select(-c("X", "Home.Phone", "E.mail.Home.Phone"))

colnames(df) <- c("last_name", "first_name", "email",
                  "cell_phone", "employment_status", "employer")

#Getting rid of empty rows
df <- df %>% filter(!(last_name=="" | first_name==""))


#Making all names Title format
df <- df %>% mutate(last_name=str_to_title(last_name),
                    first_name=str_to_title(first_name),
                    email=str_to_lower(email))


#Dropping duplicates
df <- distinct(df)

#Filling in Blank values with NA
df <- na_if(df, "")

df <- df %>% mutate(last_name = str_to_upper(last_name),
                    first_name = str_to_upper(first_name))


df <- distinct(df)

#write.csv(df, "../../Database/2000-2020_volunteer-information.csv", row.names=FALSE)


######## BIRTHDAY ########
bday_text <- extract_text(file[1], encoding="UTF-8")
bday_text <-  as.data.frame(str_split(bday_text, pattern="\r\n"))

colnames(bday_text) <- "data"

#Getting rid of garbage text
bday_text <- bday_text %>% filter(!str_detect(data, pattern="Washington|Staff|Report|Birth|Last"))

#Creating clean columns
df_bday <- bday_text %>% separate(data, c("last_name", "first_name", "month", "day", "year"), sep=" ")

#Getting rid of extra comma
df_bday <- df_bday %>% mutate(day=str_replace(day, ",", ""),
                              year=as.integer(year))


#Excluding clearly incorrect values
df_bday <- df_bday %>% filter(year > 1930 & year < 2004)
df_bday <- df_bday %>% mutate(year=as.character(year))


#Creating cleaner date column
df_bday <- df_bday %>% mutate(date=str_c(month, day, year, sep="/"))
df_bday <- df_bday %>% mutate(date=mdy(date))

df_bday <- df_bday %>% mutate(last_name = str_to_upper(last_name),
                              first_name = str_to_upper(first_name))


df_bday <- distinct(df_bday)


#Merging together data
df_volunteers <- full_join(df, df_bday, by=c("last_name", "first_name"))

#Get rid of admin row
df_volunteers <- df_volunteers %>% slice(-16)

df_volunteers <- df_volunteers %>% mutate(last_name = str_to_title(last_name),
                                          first_name = str_to_title(first_name))


#Checking amount of missing data
nrow(df_volunteers %>% filter(is.na(date)))


df_volunteers <- distinct(df_volunteers)


#Unfortunately this data doesn't have info. on when the volunteers were
#volunteers so need to bring that information in. 


write.csv(df_volunteers, "volunteer-contact-and-employment-info_no-year.csv", row.names=FALSE)











