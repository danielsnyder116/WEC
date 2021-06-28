#-------------------------------
# ACADEMIC REPORTING: VOLUNTEERS
#-------------------------------

#-------------------------------
# Recruit enough volunteers to fulfill all our virtual programming.
# How many unique vols were active each term? How many vol roles were active each term (some vols may have more than one role, i.e. co-teach two days)
# Establish a baseline number of volunteers needed per virtual term.

# Increase year-over-year retention rate to 50%
# Note: Pandemic is wildcard - this might not be reasonable  
# Establish a baseline for the average # of terms volunteers serve within one FY

# What is percent of non-native English speaking teachers? What is percent of native English Speaking teachers
# Racial/ethnic diversity - increase % of non-white teachers from Spring 2020 baseline
# What is percentage breakdown by race (fed gov’t categories) and by ethnicity (hispanic/latino vs. non-hispanic/non-latino)?
#   

#-------------------------------

#Bring in all volunteer data 

library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tibble)
library(openxlsx)

setwd("/Users/Daniel/Desktop/WEC/Data/All (2006 - Present)")

#Needed to split Spring and Summer since we didn't differentiate
#Writing tutors - 391 so 196 for spring and 195 summer
#Conversation tutors - 361 so 180 for spring and 181 for summer
#Did manually using google sheets and download as same file _final

#Bringing in data & Ensuring semesters are treated as factors
#Also excluding 2006 because only Fall Data
df <- read.csv("all_volunteers_2006_2021_final.csv", stringsAsFactors = FALSE) %>% 
                mutate(semester=factor(semester, levels = c("WINTER", "SPRING", "SUMMER", "FALL"))) %>%
                filter(year != 2006)

#---------------------------------------#
#---------- UNIQUE VOLUNTEERS ----------#
#---------------------------------------#

#Isolate columns that reveal duplicates 
#(teachers doing more than one class or day within one semester and year)

#DATA SOURCE
#------------------------------------------------------------------------
df_unique_vols <- df %>% select(c(name, semester, year)) %>% distinct()

#BY YEAR
#----------------------------------------------
#Group by year only for total
df_year_vols <- df_unique_vols %>% count(year) %>% rename(num_unique_volunteers = n)

#INITIATING TABLE DATAFRAME TO SEND TO JESSIE
df_table_year_vols <- df_year_vols %>% rename(Year = year, `Number of Unique Volunteers` = num_unique_volunteers)

# #2007-2020 Average: 828
# all_avg_volunteers_per_year <- df_year_vols %>% filter(year < 2021) %>% 
#   summarize(average = round(mean(num_unique_volunteers), 0))
# 
# #2016-2020 Average: 1048
# five_year_avg_volunteers_per_year <- df_year_vols %>% filter(year >= 2016 & year < 2021) %>%
#   summarize(average = round(mean(num_unique_volunteers), 0))
# 

#BY SEMESTER & YEAR
#----------------------------------------------
#Group by semester and year
df_sem_year_vols <- df_unique_vols %>% select(c(name, semester, year)) %>% distinct() %>% 
                                       count(year, semester) %>% rename(num_unique_volunteers = n)

# #2007-2021 Average: 218
# all_avg_volunteers_per_sem <- df_sem_year_vols %>% filter(year < 2021) %>% 
#                                                   summarize(average = round(mean(num_unique_volunteers), 0))
# 
# #2016-2020 Average: 262
# five_year_avg_volunteers_per_sem_ALL <- df_sem_year_vols %>% filter(year >= 2016 & year < 2021) %>%
#                                                    summarize(average = round(mean(num_unique_volunteers), 0))

#AVERAGE BY SEASON
#------------------------------------
#FALL 2016-2020 AVERAGE: 271
five_year_avg_volunteers_per_sem_fall <- df_sem_year_vols %>% filter(year >= 2016 & 
                                                                     year < 2021 & 
                                                                     semester == "FALL") %>%
                                                              summarize(average = round(mean(num_unique_volunteers), 0))

#WINTER 2017-2021: 332
five_year_avg_volunteers_per_sem_winter <- df_sem_year_vols %>% filter(year >= 2016 & 
                                                                       semester == "WINTER") %>%
                                                                summarize(average = round(mean(num_unique_volunteers), 0))

#SPRING 2017-2021: 298
five_year_avg_volunteers_per_sem_spring <- df_sem_year_vols %>% filter(year >= 2017 &
                                                                       semester == "SPRING") %>%
                                                                summarize(average = round(mean(num_unique_volunteers), 0))

#SUMMER 2016-2020: 152
five_year_avg_volunteers_per_sem_summer <- df_sem_year_vols %>% filter(year >= 2016 & 
                                                                       year < 2021 & 
                                                                       semester == "SUMMER") %>%
                                                                summarize(average = round(mean(num_unique_volunteers), 0))


#Setting colors for each semester
seasons <- c("#4E79A7", "#59A14F", "#EDC948","#F28E2B")

num_volunteers_by_sem_year <- ggplot(data=df_sem_year_vols, aes(x=year, y=num_unique_volunteers, fill=semester)) + geom_point(shape=21, size=2) +
                                  facet_grid(.~ semester) + stat_smooth(method="loess", span=.95, color="#76B7B2") +
                                  scale_fill_manual("Semester", values=seasons) +
                                  scale_x_continuous(breaks=c(2004, 2007,2010,2015,2020, 2022)) +
                                  scale_y_continuous(breaks = c(0,100,200,300,400,500,600)) +
                                  #ylim(0,600) +
                                  labs(title="Number of Unique Volunteers by Semester Over Time",subtitle = "2007-2021",
                                       x="Year", y="Number of Volunteers") + 
                                  theme(panel.spacing = unit(5, "mm"),
                                        axis.text.x = element_text(angle = 30),
                                        #trouble = Top, Right, Bottom, Left
                                        axis.title.y = element_text(margin = unit(c(0,5,0,0),"mm")))
                                

#--------------------------------------------#
#---------- UNIQUE VOLUNTEER ROLES ----------#
#--------------------------------------------#
#Isolate columns that reveal duplicates 
#(teachers doing more than one class or day within one semester and year)

#DATA SOURCE
#------------------------------------------------------------------------
df_roles <- df 

#BY YEAR
#----------------------------------------------
#Group by year only for total
df_year_roles <- df_roles %>% count(year) %>% rename(num_unique_roles = n)

#2007-2020 Average: 905 vs. 828
all_avg_roles_per_year <- df_year_roles %>% filter(year < 2021) %>% 
                                            summarize(average = round(mean(num_unique_roles), 0))

#2016-2020 Average: 1141 vs 1048
five_year_avg_roles_per_year <- df_year_roles %>% filter(year >= 2016 & year < 2021) %>%
                                                  summarize(average = round(mean(num_unique_roles), 0))

#BY SEMESTER & YEAR
#----------------------------------------------
#Group by semester and year
df_sem_year_roles <- df_roles %>% count(year, semester) %>% rename(num_unique_roles = n)

#2007-2020 Average: 238 vs 218
all_avg_roles_per_sem <- df_sem_year_roles %>% filter(year < 2021) %>% 
                                               summarize(average = round(mean(num_unique_roles), 0))

#2016-2020 Average: 285 vs 262
five_year_avg_roles_per_sem_ALL <- df_sem_year_roles %>% filter(year >= 2016 & year < 2021) %>%
                                               summarize(average = round(mean(num_unique_roles), 0))

#AVERAGE BY SEASON
#------------------------------------
#FALL 2016-2020 AVERAGE: 282 vs 271
five_year_avg_roles_per_sem_fall <- df_sem_year_roles %>% filter(year >= 2016 & 
                                                                  year < 2021 & 
                                                                  semester == "FALL") %>%
                                                          summarize(average = round(mean(num_unique_roles), 0))

#WINTER 2017-2021: 353 vs 332
five_year_avg_roles_per_sem_winter <- df_sem_year_roles %>% filter(year >= 2016 & 
                                                                   semester == "WINTER") %>%
                                                            summarize(average = round(mean(num_unique_roles), 0))

#SPRING 2017-2021: 339 vs 298
five_year_avg_roles_per_sem_spring <- df_sem_year_roles %>% filter(year >= 2017 &
                                                                   semester == "SPRING") %>%
                                                            summarize(average = round(mean(num_unique_roles), 0))

#SUMMER 2016-2020: 175 vs 152
five_year_avg_roles_per_sem_summer <- df_sem_year_roles %>% filter(year >= 2016 & 
                                                                   year < 2021 & 
                                                                   semester == "SUMMER") %>%
                                                             summarize(average = round(mean(num_unique_roles), 0))


# #Setting colors for each semester
# seasons <- c("#4E79A7", "#59A14F", "#EDC948","#F28E2B")
# 
# num_roles_by_sem_year <- ggplot(data=df_sem_year_roles, aes(x=year, y=num_unique_roles, fill=semester)) + geom_point(shape=21, size=2) +
#   facet_grid(.~ semester) + stat_smooth(method="loess", span=.95, color="#76B7B2") +
#   scale_fill_manual("Semester", values=seasons) +
#   scale_x_continuous(breaks=c(2004, 2007,2010,2015,2020, 2022)) +
#   scale_y_continuous(breaks = c(0,100,200,300,400,500,600)) +
#   #ylim(0,600) +
#   labs(title="Number of Unique Volunteer Roles by Semester Over Time",subtitle = "2007-2021",
#        x="Year", y="Number of Volunteer Roles") +
#   theme(panel.spacing = unit(5, "mm"),
#         axis.text.x = element_text(angle = 30),
#         #trouble = Top, Right, Bottom, Left
#         axis.title.y = element_text(margin = unit(c(0,5,0,0),"mm")))



#Generating Average Row - ensuring year shows up correctly and then rounding volunteer numbers
get_avg_sem_row <- function(dataframe, semester_type) {
  
  df_avg <- dataframe %>% filter(semester == semester_type) %>% summarize(across(3:6, mean)) %>% 
                    
                          add_column(year = "2007-2021 Average:", .before = "num_unique_volunteers") %>%
                          add_column(semester = semester_type, .before = "num_unique_volunteers") %>%
                          mutate(across(3:6, ~round(., 0)))
  
  return(df_avg)
}


#BY YEAR TABLE - UNIQUE VOLUNTEERS AND ROLES
#------------------------------------------------------------------------------------------------

#Generating table 
df_table_year <- inner_join(df_year_vols, df_year_roles, by = c("year")) %>%
                 mutate(margin_diff = (num_unique_roles - num_unique_volunteers),
                       percent_diff = round((num_unique_roles - num_unique_volunteers) / num_unique_roles, 2)*100,
                       year = as.character(year)) 
 
  
df_avg_rows_year <- df_table_year %>% summarize(across(2:5, mean)) %>%
                              mutate(year = "2007-2021 Average:") %>%
                              mutate(across(2:5, ~round(., 0)))


#Putting everything together
df_table_year_final <- bind_rows(df_table_year, df_avg_rows_year) %>%
                       mutate(percent_diff = paste0(percent_diff, "%")) %>% 
                       rename(Year = year,
                             `Total Number of Unique Volunteers` = num_unique_volunteers,
                             `Total Number of Volunteer Roles` = num_unique_roles,
                             `Number of Roles Filled by Volunteers Covering More Than One Role` = margin_diff,
                             `Percentage of Roles Filled by Volunteers Covering More Than One Role` = percent_diff)
          


#BY SEMESTER TABLE - UNIQUE VOLUNTEERS AND ROLES
#------------------------------------------------------------------------------------------------
#Generating table 
df_table_sem <- inner_join(df_sem_year_vols, df_sem_year_roles, by = c("semester", "year")) %>%
                mutate(margin_diff = (num_unique_roles - num_unique_volunteers),
                       percent_diff = round((num_unique_roles - num_unique_volunteers) / num_unique_roles, 2)*100,
                       year = as.character(year)) 

#Semester-level of granularity has four average rows
df_winter <- get_avg_sem_row(df_table_sem, "WINTER")
df_spring<- get_avg_sem_row(df_table_sem, "SPRING")
df_summer <- get_avg_sem_row(df_table_sem, "SUMMER")
df_fall <- get_avg_sem_row(df_table_sem, "FALL")

df_avg_rows_sem <- bind_rows(df_winter, df_spring, df_summer, df_fall)

#Putting everything together
df_table_sem_final <- bind_rows(df_table_sem, df_avg_rows_sem) %>%
                      mutate(percent_diff = paste0(percent_diff, "%")) %>% 
                      rename(Year = year,
                             Semester = semester,
                             `Total Number of Unique Volunteers` = num_unique_volunteers,
                             `Total Number of Volunteer Roles` = num_unique_roles,
                             `Number of Roles Filled by Volunteers Covering More Than One Role` = margin_diff,
                             `Percentage of Roles Filled by Volunteers Covering More Than One Role` = percent_diff)
                    

list_of_tables <- list("By Year" = df_table_year_final,
                       "By Semester" = df_table_sem_final)
write.xlsx(list_of_tables, file = "./Output/WEC Unique Volunteers and Roles By Semester and Year: 2007-2021.xlsx", row.names = FALSE)


