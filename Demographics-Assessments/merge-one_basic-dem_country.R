library(dplyr)
library(stringr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Core-Demographics")


##### Merges in the Country data with the other demographic data 

df_add <- read.csv("Country-of-Origin/Output/additional-demographics_2020-05-23.csv", stringsAsFactors=FALSE)
df_add <- df_add %>% mutate(student_id = as.character(student_id), year=as.character(year))

df_bd <- read.csv("Basics/Output/basic-demographics_2020-05-23.csv", stringsAsFactors = FALSE)
df_bd <- df_bd %>% mutate(student_id = as.character(student_id), year=as.character(year))

#View(count(df_add, vars=country))

# country_by_semester_year <- df_add %>% group_by(semester, year) %>%
#                                        filter(country == "El Salvador") %>%
#                                         summarize(result = n())
# View(country_by_semester_year)

glimpse(df_add)
glimpse(df_bd)

nrow(df_add)
nrow(df_bd)

df_final <- full_join(df_add, df_bd, by=c("student_id", "semester", "year"))

df_final <- distinct(df_final)

#Write tiny loop to fill in name.x when it is NA and name.y contains value
for (i in 1:nrow(df_final)) {
  
  if (is.na(df_final$name.x[i]) & !is.na(df_final$name.y[i])) {
    df_final$name.x[i] <- df_final$name.y[i]
    
  }
  
}

df_final <- df_final %>% select(-name.y) %>% rename(name = name.x)

df_final <- df_final %>% select(student_id, name, gender, age, country, 
                                education_years, zip_code,  ethnicity, 
                                employment_status, class_name, teacher, 
                                semester, year)



write.csv(df_final, 'final-demographics_2020-05-23.csv', row.names=FALSE)
