library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/602770/downloads/volunteer/wec/Students/Demographics-Assessments/Core-Demographics/Country-of-Origin/Processed")

df <- read.csv("additional-demographics.csv", stringsAsFactors = FALSE)

#Getting rid of one NA row that somehow snuck in...more grr!
df <- df %>% slice(-14676)

glimpse(df)

#View(count(df, vars=country))

#Dealing with Congo issue not solved in previous code grrr
for (i in 1:nrow(df)) {
  
  if (str_detect(df$name[i], pattern = "Congo")) {
    
    df$country[i] <- "Democratic Republic of the Congo"
    df$name[i] <- str_replace(df$name[i], pattern = "Congo, The Democratic Republic Of The Formerly Zaire", "")
    
  }
  
}

#Cleaning up some country names - add extra | to ensure the spaces are not included in evaluating each pattern element
df <- df %>% mutate(country = str_replace_all(country, pattern = "\\, United Republic Of|
                                            | Formerly\\, Democratic Kampuchea|Islamic Republic Of |
                                            | Formerly Byelorussian Soviet Socialist Republic| 
                                            | Formerly Yugoslav Republic Of|n Arab Republic|\\, Republic Of|
                                            |\\, Province Of China| 
                                            | Formerly Ukrainian Soviet Socialist Republic|n Federation|
                                            |Zambia No Longer ExistsSee | Of America|
                                            |n Arab Jamahiriya| The Formerly Zaire| Ivory Coast|
                                            | No Longer Exists", replacement = ""))
#To help with cleaning
df <- df %>% mutate(country = str_pad(country, 6, side = "right", pad = " "))

View(count(df, vars=country))

df <- df %>% mutate(country = case_when(str_detect(country, pattern = "Viet Nam") ~ "Vietnam",
                                    str_detect(country, pattern = "Burma") ~ "Myanmar",
                                    str_detect(country, pattern = "Congo\\, The Democratic Republic Of") ~ "Democratic Republic of the Congo",
                                    str_detect(country, pattern = "Congo") ~ "Republic of the Congo",
                                    str_detect(country, pattern = "Côte D'ivoire") ~ "Côte D'Ivoire",
                                    str_detect(country, pattern = "Czech Republic") ~ "Czechia",
                                    str_detect(country, pattern = "German Democratic Republic") ~ "Germany",
                                    str_detect(country, pattern = "GuineaBissau") ~ "Guinea-Bissau",
                                    str_detect(country, pattern = "Korea\\, Democratic People[:punct:]s Republic Of") ~ "North Korea",
                                    str_detect(country, pattern = "Korea ") ~ "South Korea",
                                    str_detect(country, pattern = "Lao People[:punct:]s Democratic Republic") ~ "Laos",
                                    str_detect(country, pattern = "Macedonia") ~ "North Macedonia",
                                    str_detect(country, pattern = "Unknown Or Unspecified Country") ~ NA_character_,
                                    TRUE ~ country))

df <- df %>% mutate_all(str_squish)

#Checking our results
View(count(df, vars=country))

#759 NA or 1.7% missing - nice
nrow(df %>% filter(is.na(country)))
nrow(df)

759 / 43769

#Only 34 NA for class_name out of 43769 yesss!
nrow(df %>% filter(is.na(class_name)))

nrow(df)

#Getting rid of rows with invalid class names
df <- df %>% filter(!str_detect(class_name, pattern = "Books"))

#Fix a couple of class_names that are 6:00 AM rather than 6AM
df <- df %>% mutate(class_name = str_remove_all(class_name, pattern = "[:punct:]00"))


#Manually Fix 432817319	La Rosa, Lucy Uruguay and 846699342	Muñoz, Emilia El Salvador
df[14667, "name"] <- "La Rosa, Lucy"
df[14667, "country"] <- "Uruguay"

df[31211, "name"] <- "Muñoz, Emilia"
df[31211, "country"] <- "El Salvador"

write.csv(df, "../Output/additional-demographics_2020-05-23.csv", row.names = FALSE)







#The NAs are truly NAs in Proactive - can figure it out at times by language but not worth the time

# #Filling in Data
# df <- df %>% mutate(country = case_when(str_detect(student_id, pattern = "2086850607") ~ "El Salvador",
#                                         str_detect(student_id, pattern = "2132424952") ~ "Laos",
#                                         str_detect(student_id, pattern = "2128579328") ~ "Vietnam",
#                                         str_detect(student_id, pattern = "2125832232") ~ "Vietnam",
#                                         str_detect(student_id, pattern = "2066104404") ~ "Vietnam",
#                                         str_detect(student_id, pattern = "2064852315") ~ "Vietnam",
#                                         str_detect(student_id, pattern = "") ~ "",
#                                         str_detect(student_id, pattern = "") ~ "",
#                                         str_detect(student_id, pattern = "") ~ "",
#                                         str_detect(student_id, pattern = "") ~ "",
#                                         str_detect(student_id, pattern = "") ~ "",
#                                         str_detect(student_id, pattern = "") ~ "",
#                                         str_detect(student_id, pattern = "") ~ "",
#                                         str_detect(student_id, pattern = "") ~ "",
#                                         TRUE ~ country))




