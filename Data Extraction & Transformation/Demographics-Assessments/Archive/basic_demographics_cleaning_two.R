library(dplyr)

setwd("/Users/Daniel/Documents/wec")

df <- read.csv("basic_demographics_2000_2019_0110.csv", stringsAsFactors = FALSE)


#FIRST TIDYVERSE Coding Example

#Overhead View
count(df, vars=ethnicity)
glimpse(df)


#-----------------------AGE-#Fixing missing/incorrect age rows#------------------------------------
#Getting mean age to replace missing or incorrect values
df %>% select(age) %>% summarize(., mean(age, na.rm = TRUE))


#Getting average age for each year
df_average_age <- df %>% group_by(year) %>%
                     summarize(., age_avg=mean(age, na.rm=TRUE)) %>%
                     round(.)

#Adding to df 
df <- left_join(df, df_average_age, by='year')

#First filling Age NAs with zero
df <- df %>% replace_na(.,list(age=0))

#So 130 missing/incorrect out of 35,024 so .4% missing, noice!
nrow(df %>% filter(age < 15))

#Replacing cases where age is less than 15 to average age by year - using avg_age column 
for (i in 1:nrow(df)) {
  if (df$age[i] < 15) {
    df$age[i] <- df$age_avg[i]
  }
}           

#------EDUCATION_YEARS---#Fixing missing/incorrect education_year rows#------------------

#Getting average education_year for each year
df_average_educ_yr <- df %>% group_by(year) %>% 
                          summarize(., educ_yr_avg=mean(education_years, na.rm=TRUE)) %>%
                          round(.)

#Adding avg_educ_yr to df 
df <- left_join(df, df_average_educ_yr, by='year')

#Can't figure out tidy way right now so using base R
# df$education_years <- df %>% filter(education_years > 31 | is.na(education_years)) %>% select(education_years, educ_yr_avg) %>%  case_when(.$education_years == 0 ~ .$educ_yr_avg)

#First filling Age & Education NAs with zero
df <- df %>% replace_na(., list(education_years=500))

#Finding NA values and large incorrect values for EDUCATION_YEAR to then change to year average
#2375 values missing/incorrectly entered
nrow(df %>% filter(education_years > 31))

for (i in 1:nrow(df)) {
  if (df$education_years[i] > 31) {
      df$education_years[i] <- df$educ_yr_avg[i]
  }
}                                         


#-------------------------------------------------------------

View(df %>% filter(is.na(gender) | gender =="Unknown"))

#Manually Identifying some names as Female or Male based on first name and Google

df$gender[2472]  <- "Female"   #Yetmgeta *
df$gender[6040]  <- "Female"   #Elena
df$gender[8719]  <- "Male"     #Luis
df$gender[10562] <- "Female"  #Yordanos *
df$gender[10732] <- "Male"    #Mboumgnen
df$gender[12261] <- "Male"    #Einhard
df$gender[19278] <- "Female"  #Suratmi *
df$gender[23302] <- "Male"    #Salek 

df$gender[25369] <- "Female"  #Yesenia
df$gender[25583] <- "Female"  #Isabel
df$gender[25714] <- "Female"  #Faiza
df$gender[25757] <- "Female"  #Hyesun *
df$gender[26871] <- "Male"    #Edwin

df$gender[29173] <- "Female"  #Abadia *
df$gender[30149] <- "Male"    #Neri * - Unisex name
df$gender[30288] <- "Male"    #Alcides
df$gender[31001] <- "Female"  #Yesenia
df$gender[33191] <- "Male"    #Osmin


write.csv(df, "basic_demographics_2000_2019_0107.csv", row.names=FALSE)
