library(dplyr)
library(stringr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students")

df_dem <- read.csv("Demographics-Assessments/final_demographics-and-assessment-data.csv", stringsAsFactors = FALSE)
df_class <- read.csv("Totals_Transfers_Dropped/Output/2000-2020_final-class-details_includes-dropped-transfers-totals.csv", stringsAsFactors = FALSE)

df_class <- df_class %>% mutate(semester=str_to_upper(semester))

#Getting rid of 2020 data since it will be treated separately
df_class <- df_class %>% filter(year <= 2019)

#Pairing down class
# df_class <- df_class %>% select(class_id, class_name, semester, year, DAYS, 
#                                 TIME, simple_standard_class, complete_standard_class)

#Capitalizing, getting rid of extra spaces, and replacing in-between spaces with dashes 
df_dem <- df_dem %>% mutate(class_name = str_replace_all(class_name, pattern=" \\- ", replacement=" "))

#Dealing with weird spaces causing multiple dashes in a row
df_dem <- df_dem %>% mutate(class_name = str_squish(str_replace_all(str_to_upper(class_name), " ", "\\-")))
df_dem <- df_dem %>% mutate(class_name = str_replace_all(class_name, pattern="\\-\\/\\-|\\-\\/", replacement = "\\/"))


#Getting rid of extra spaces
df_dem <- df_dem %>% mutate(class_name = str_squish(class_name))

#Replacing empty class names with NA
df_dem <- df_dem %>% mutate(class_name = na_if(class_name, "\\s+"))

#Looks like df_class covers all the df_dem classes which is good
View(count(df_class, vars=class_name))
View(count(df_dem, vars=class_name))


#Creating database of student and class information
#Since the demographics data doesn't have a unique class identifier, just the name, this leads to a lot of duplicate creation
#since a class like 1A might be offered 3-5 times during one semester, with no indicator in the old name.

df_all <- full_join(df_dem, df_class, by=c("class_name", "semester", "year"))

#To deal with this, after merging, we drop "duplicate" rows by looking at 
# student_id, class_name, semester, and year - there's no way a student was taking multiple of the same class
# during one semester.

df_all <- distinct(df_all, student_id, class_name, semester, year, .keep_all = TRUE)

#Dropping cases where there is no match (indicated by NA for student_id)
df_all <- df_all %>% filter(!is.na(student_id))


#Number of cases where class_name is null: 41, or .094%
#There were no matches between the two files
nrow(df_all %>% filter(is.na(class_name))) / nrow(df_all)
#View(df_all %>% filter(is.na(class_name)))

#Number of cases where class_id is null: 100, or .014%
nrow(df_all %>% filter(is.na(class_id))) / nrow(df_all)
#View(df_all %>% filter(is.na(class_id)))

#Cases where class existed but didn't have a class_id - None woohoo!
nrow(df_all %>% filter(is.na(class_id) & !is.na(class_name)))

write.csv(df_all, "student-class-blackbaud-input_2000-2019.csv", row.names = FALSE)




