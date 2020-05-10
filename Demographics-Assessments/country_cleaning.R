library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(purrr)
library(glue)
#library(rlist)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Core-Demographics/Country-of-Origin/Data/Processed")

df <- read.csv("country_class.csv", stringsAsFactors = FALSE, header=1, encoding="UTF-8")

names(df)[1] <- "Fall.2000"
names(df)[42] <- "Spring.2019"
names(df)

#Labeling each row by semester_year
df <- df %>% gather(., names(df), key="semester_year", value="cases")

#Getting rid of invalid or unusable rows (wanted to get teacher info. but not worth it)
strings <- c("Teacher", "Report", "Class Roster", "Washington English")
df <- df %>% filter(., !str_detect(df$cases, paste(strings, collapse = "|")))

#Getting rid of empty rows
df <- df %>% filter(., cases!="")

#Splits semester and year into two columns
df <- df %>% separate(., semester_year, c("semester", "year"))

#Segmenting cases column into individual columns
df <- df %>% separate(., cases, c("student_id", "last_name", "first_name", "country"),
                                    extra="merge", sep=" ")

#changing order of columns
df <- df %>% select(3:6, everything())

#Getting rid of commas
df <- df %>% mutate(., last_name=str_replace_all(df$last_name, ",", ""))

#Way to preview what will be picked up by regex shown in Viewer
#str_view_all(df$country, "\\d|\\(|\\)|\\-")

#Getting rid of phone numbers using regex: \\d - all digits, | - or, need double slash
df <- df %>% mutate(., country=str_replace_all(df$country, "\\d|\\(|\\)|\\-|\\.|\\/|\\+|\\=|\\,", ""))

#Getting rid of extra spaces
df <- df %>% mutate(., country=str_trim(df$country))

#Getting rid of rows with blank or NA country
df <- df %>% filter(., country!="")


#Best way to get rid of mismatch at this point is manual correction -

df$country[10319] <- 'Afghanistan'
df$country[15407] <- 'Burkina Faso'
df$country[29992] <- 'Niger'
df$country[31415] <- 'Colombia'
df$country[27541] <- 'Cameroon'
df$country[42190] <- 'Cameroon'
df$country[1902]  <- 'Brazil'
df$country[15480] <- 'Brazil'
df$country[23755] <- 'Brazil'
df$country[13308] <- 'Brazil'
df$country[13949] <- 'Brazil'
df$country[24874] <- 'El Salvador'
df$country[42418] <- 'El Salvador'
df$country[23791] <- 'Guatemala'
df$country[13948] <- 'South Korea'

#Just kidding it was worse than I thought
View(count(df, vars=country))

#Before, 1043 countries - 
#So now just deal with ones that are mismatches or no country data
#After, 244 countries
#Will go in and manually fix the rest



#Automation Take 2 

#Bring in official country names to aid in data cleaning
countries <- read.csv("../countries-crosswalk.csv", stringsAsFactors = FALSE)
countries <- countries %>% select(Country) %>% unlist(.) %>% as.vector(.) %>% str_trim(., side="both")

# %in% does not work like in python, need to use stringr
str_detect('asdasd Albania', regex('Albania', ignore_case = TRUE))

#If a correct country name is within other gobbledygook in a row,
# this double for loop replaces that row with just the country name
# deals with the issue of misparsed names included as part of the country

#Takes about 12 minutes to run
for (i in 1:length(countries)) {
  for (k in 1:nrow(df)) {
    if (str_detect(df$country[k], regex(countries[i], ignore_case = TRUE))) {
      df$country[k] <- countries[i]
    }
    
  }
}

write.csv(df, 'in_process.csv', row.names=FALSE)

df <- read.csv('in_process.csv', stringsAsFactors = FALSE)
#Fixing any of the values with errors or different country names
# (eg. Republic of Korea  vs 'South Korea')
View(count(df, vars=country))

#Checking cases out
# df %>% filter(., country=='Verde')

#Replacing with correct country
df <- df %>% mutate(., country=replace(country, country=='Ai   Viet Nam', 'Vietnam'))
df <- df %>% mutate(., country=replace(country, country=='Ahmed', 'Mauritania'))   
df <- df %>% mutate(., country=replace(country, country=='African Republic', 'Central African Republic'))
df <- df %>% mutate(., country=replace(country, country=='Almeida Laiz', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Amiel', 'Mexico'))
df <- df %>% mutate(., country=replace(country, country=='Araujo Veras Ana', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Bernardo', 'Spain'))
df <- df %>% mutate(., country=replace(country, country=='Britto Gabarron', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Cabizuca', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Camargo Claudia', 'Brazil'))

df <- df %>% mutate(., country=replace(country, country=='Carla Macedo', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Carvalho Braz', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Carvalho Veiga', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Central African Rep.', 'Central African Republic'))

df <- df %>% mutate(., country=replace(country, country=='Cheikh', 'Mauritania'))
df <- df %>% mutate(., country=replace(country, country=='Christina   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Congo The Democratic Republic Of The Formerly  Bikindou Mpembe Judith', 'Congo'))
df <- df %>% mutate(., country=replace(country, country=='Congo The Democratic Republic Of The Formerly  Makeo Abdallah', 'Congo'))
df <- df %>% mutate(., country=replace(country, country=='Côte D’ivoire Ivory Coast', 'Côte D’ivoire'))
df <- df %>% mutate(., country=replace(country, country=='CruzVazquez', 'Mexico'))
df <- df %>% mutate(., country=replace(country, country=='das', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='De Los', 'El Salvador'))
df <- df %>% mutate(., country=replace(country, country=='de Souza', 'Brazil'))

df <- df %>% mutate(., country=replace(country, country=='del', 'Spain'))
df <- df %>% mutate(., country=replace(country, country=='Del', 'Colombia'))
df <- df %>% mutate(., country=replace(country, country=='dos Santos', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='El', 'Mauritania'))
df <- df %>% mutate(., country=replace(country, country=='Ely Mohamed', 'Mauritania'))
df <- df %>% mutate(., country=replace(country, country=='Fernandez', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Francisco', 'Honduras'))
df <- df %>% mutate(., country=replace(country, country=='Freitas Fihlo', 'Brazil'))

df <- df %>% mutate(., country=replace(country, country=='German Democratic Republic No Longer Exists', 'Germany'))
df <- df %>% mutate(., country=replace(country, country=='Gia   Viet Nam', 'Vietnam'))              
df <- df %>% mutate(., country=replace(country, country=='Goes Monteiro', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Hee   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Heung   Korea Democratic People’s Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Htet   Myanmar', 'Myanmar'))
df <- df %>% mutate(., country=replace(country, country=='Burma', 'Myanmar'))
df <- df %>% mutate(., country=replace(country, country=='hyun   Korea Democratic People’s Republic Of', 'North Korea'))
df <- df %>% mutate(., country=replace(country, country=='Hyun   Korea Republic Of', 'South Korea'))

df <- df %>% mutate(., country=replace(country, country=='Jae   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Jeong   Korea Republic Of', 'South Korea'))              
df <- df %>% mutate(., country=replace(country, country=='Ji   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Jin   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Joffer Brito', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Jose LCuhiile', 'Mexico'))
df <- df %>% mutate(., country=replace(country, country=='Judith', 'Congo'))
df <- df %>% mutate(., country=replace(country, country=='Jung   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Kay   Myanmar', 'Myanmar'))


df <- df %>% mutate(., country=replace(country, country=='Koffi     Côte D’ivoire Ivory Coast', 'Côte D’ivoire'))
df <- df %>% mutate(., country=replace(country, country=='Korea Democratic People’s Republic Of', 'North Korea'))              
df <- df %>% mutate(., country=replace(country, country=='Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Kyu   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Kyung   Korea Democratic People’s Republic Of', 'North Korea'))
df <- df %>% mutate(., country=replace(country, country=='La Puente', 'Peru'))
df <- df %>% mutate(., country=replace(country, country=='Lao People’s Democratic Republic', 'Laos'))
df <- df %>% mutate(., country=replace(country, country=='Le', 'Vietnam'))
df <- df %>% mutate(., country=replace(country, country=='Leon', 'Spain'))


df <- df %>% mutate(., country=replace(country, country=='Luis', 'Venezuela'))
df <- df %>% mutate(., country=replace(country, country=='Mahmood', 'Dominican Republic'))
df <- df %>% mutate(., country=replace(country, country=='Mariela del', 'Panama'))
df <- df %>% mutate(., country=replace(country, country=='Mi   Korea Republic Of', 'South Korea'))

df <- df %>% mutate(., country=replace(country, country=='Mj   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Moctar', 'Mauritania'))              
df <- df %>% mutate(., country=replace(country, country=='Mohamed', 'Mauritania'))
df <- df %>% mutate(., country=replace(country, country=='Nam   Viet Nam', 'Vietnam'))
df <- df %>% mutate(., country=replace(country, country=='Ni   Myanmar', 'Myanmar'))
df <- df %>% mutate(., country=replace(country, country=='Ntsa Epse', 'Cameroon'))
df <- df %>% mutate(., country=replace(country, country=='O Taleb', 'Mauritania'))
df <- df %>% mutate(., country=replace(country, country=='Oliveira', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Oliveira Alves', 'Brazil'))

df <- df %>% mutate(., country=replace(country, country=='Oliveira Filho', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Pablo', 'Spain'))              
df <- df %>% mutate(., country=replace(country, country=='Peccinini', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Phoo   Myanmar', 'Myanmar'))
df <- df %>% mutate(., country=replace(country, country=='Pranjna', 'Bangladesh'))
df <- df %>% mutate(., country=replace(country, country=='Ramy   Palestine', 'Palestine'))
df <- df %>% mutate(., country=replace(country, country=='Rocha Prestes', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Rodriguez', 'El Salvador'))
df <- df %>% mutate(., country=replace(country, country=='Ronaldo', 'Brazil'))


df <- df %>% mutate(., country=replace(country, country=='Sai Ebaa   Côte D’ivoire Ivory Coast', 'Côte D’ivoire'))
df <- df %>% mutate(., country=replace(country, country=='Santos Alves', 'Brazil'))              
df <- df %>% mutate(., country=replace(country, country=='SantosLopez', 'Dominican Republic'))
df <- df %>% mutate(., country=replace(country, country=='SantosTrier', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Saravia', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Seung Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Souza', 'Brazil'))
df <- df %>% mutate(., country=replace(country, country=='Sun   Korea Democratic People’s Republic Of', 'North Korea'))
df <- df %>% mutate(., country=replace(country, country=='Te   Lao People’s Democratic Republic', 'Laos'))

df <- df %>% mutate(., country=replace(country, country=='Thi   Viet Nam', 'Vietnam'))
df <- df %>% mutate(., country=replace(country, country=='Thin   Myanmar', 'Myanmar'))              
df <- df %>% mutate(., country=replace(country, country=='Viet Nam', 'Vietnam'))
df <- df %>% mutate(., country=replace(country, country=='Won   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Woo   Korea Democratic People’s Republic Of', 'North Korea'))
df <- df %>% mutate(., country=replace(country, country=='Yeong   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Yoon   Korea Democratic People’s Republic Of', 'North Korea'))
df <- df %>% mutate(., country=replace(country, country=='Young   Korea Republic Of', 'South Korea'))
df <- df %>% mutate(., country=replace(country, country=='Camargo Claudia', 'Brazil'))

#Delete these rows as student_id is cell number
strings <- c('Lanka','Republic','Salvador','Verde')
df <- df %>% filter(., !country %in% strings)


#Replacing unclear ones with NA
unknown <- c('Al','Benitez','Bouha','Cecilia','del Carmen','Nobre Carvalho',
             'Orn','Silva Verena','Soukantha','Torres Solange')

df <- df %>% mutate(., country=replace(country, country %in% unknown, NA))

#Making sure it worked 
View(df %>% filter(., is.na(country)))

#Final Check - let's see!
View(count(df, vars=country))

#Time to add it back into what we already have 
#-------------------------------------------------------------

#Pairing down dataframe and getting unique matches
df_country <- df %>% select(student_id, country)  %>% unique(.)

#16474 unique students 
#14908 - ESL students
#1566 unique students at other classes




##ATTEMPT 1 without tidyr, needs more TIDYR! 

# n <- names(df)
# ready <- list()
# for (i in 1:length(df)) {
# 
# ready[i] <- df %>% separate(., col=n[i], into=c(glue("student_id_{n}")[i], glue("last_name_{n}")[i], 
#glue("first_name_{n}")[i], glue("country1_{n}")[i], 
#glue("country2_{n}")[i]),
# 
# extra='merge', sep=, remove=TRUE, convert=TRUE, fill="right") %>% 
# select(., c(i:(i+4))) %>% list(.)
# }
# newest <- bind_cols(list.cbind(ready))
# #converts all columns to character type without having to then convert list to data frame - thanks Hadley!
# df[] <- lapply(newest, as.character)

#Function 1  - determine if good country is in df$country - returns TRUE
# has_country <- function(good_vec, frame_column) {
#   for (i in 1:nrow(df)){
#     
#     for (j in 1:length(countries)) {
#     
#       if (good_vec[j] %in% frame_column[i]){
#           
#           return(i,j)
#       }
#     } 
#   }
# }
# for (i in 1:nrow(df)){
#   
#   for (j in 1:length(countries)) {
#     
#     if (countries[j] %in% df$country[i]){
#       
#       print(c(TRUE, j, i ))
#     }
#   } 
# }
# 
# if (TRUE) {
#   print('hi')
# }
# 
# which(countries %in% df$country)
# 
# if (countries %in% df$country){
#   
#   return(TRUE)
# }
# results <- has_country(countries, df$country)
# 
# #Function 2
# replace_value <- function(boolean, good_vec_index) {
# 
#   for (k in 1:nrow(df)) {
#     
#     if (boolean == 1) {
#       
#       return (good_vec_index)
#     }
#   }
# }
# replace_value("TRUE", countries)
# df %>% mutate_if(., has_country(countries, df$country), country=replace_value() )
# 
# mutate_if(first function, second function)
#   str_detect(df$country, list(countries))
# 
# # detect yes or no
# # then - what you want to change it to
# #   get the index of the country
