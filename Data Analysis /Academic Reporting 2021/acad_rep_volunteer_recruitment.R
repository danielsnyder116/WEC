#-------------------------------------------
# ACADEMIC REPORTING: VOLUNTEER RECRUITMENT
#-------------------------------------------

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
                mutate(semester=factor(semester, levels = c("SUMMER", "FALL","WINTER", "SPRING")),
                       year = as.double(year)) %>%
                filter(year != 2006) %>%
                arrange(across(c(year, semester))) %>% 
  
                #To create FY and get rid of two semester from partial FY 2006
                mutate(fy = case_when((semester %in% c("WINTER", "SPRING")) ~ (year - 1), TRUE ~ year)) %>%
                select(-year) %>%
                rename(year = fy) %>%
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

#2007-2020 Average: 906 vols
all_avg_volunteers_per_year <- df_year_vols %>% summarize(average = round(mean(num_unique_volunteers), 0))

#2016-2020 Average: 1057 vols
five_year_avg_volunteers_per_year <- df_year_vols %>% filter(year >= 2016) %>%
                                                      summarize(average = round(mean(num_unique_volunteers), 0))


#BY SEMESTER & YEAR
#----------------------------------------------
#Group by semester and year
df_sem_year_vols <- df_unique_vols %>% select(c(name, semester, year)) %>% distinct() %>% 
                                       count(year, semester) %>% rename(num_unique_volunteers = n)

#2007-2021 Average: 227
all_avg_volunteers_per_sem <- df_sem_year_vols %>% summarize(average = round(mean(num_unique_volunteers), 0))

#2016-2020 Average: 264
five_year_avg_volunteers_per_sem <- df_sem_year_vols %>% filter(year >= 2016) %>%
                                                   summarize(average = round(mean(num_unique_volunteers), 0))

#AVERAGE BY SEASON
#------------------------------------
#FALL 2016-2020 AVERAGE: 271
five_year_avg_volunteers_per_sem_fall <- df_sem_year_vols %>% filter(year >= 2016 & semester == "FALL") %>%
                                                              summarize(average = round(mean(num_unique_volunteers), 0))

#WINTER 2016-2020: 335
five_year_avg_volunteers_per_sem_winter <- df_sem_year_vols %>% filter(year >= 2016 & semester == "WINTER") %>%
                                                                summarize(average = round(mean(num_unique_volunteers), 0))

#SPRING 2016-2020: 298
five_year_avg_volunteers_per_sem_spring <- df_sem_year_vols %>% filter(year >= 2016 & semester == "SPRING") %>%
                                                                summarize(average = round(mean(num_unique_volunteers), 0))

#SUMMER 2016-2020: 152
five_year_avg_volunteers_per_sem_summer <- df_sem_year_vols %>% filter(year >= 2016 & semester == "SUMMER") %>%
                                                                summarize(average = round(mean(num_unique_volunteers), 0))


#Setting colors for each semester
seasons <- c("#4E79A7", "#59A14F", "#EDC948","#F28E2B")

num_volunteers_by_sem_year <- ggplot(data=df_sem_year_vols, aes(x=year, y=num_unique_volunteers, fill=semester)) + geom_point(shape=21, size=2) +
                                  facet_grid(.~ semester) + stat_smooth(method="loess", span=.95, color="#76B7B2") +
                                  scale_fill_manual("Semester", values=seasons) +
                                  scale_x_continuous(breaks=seq(2007, 2022, by=3)) +
                                  scale_y_continuous(breaks = c(0,100,200,300,400,500,600)) +
                                  #ylim(0,600) +
                                  labs(title="Number of Unique Volunteers by Semester Over Time (FY)",subtitle = "FY2007-FY2020",
                                       x="Fiscal Year", y="Number of Volunteers") + 
                                  theme(panel.spacing = unit(5, "mm"),
                                        axis.text.x = element_text(angle = 30),
                                        #trouble = Top, Right, Bottom, Left
                                        axis.title.y = element_text(margin = unit(c(0,5,0,0),"mm")))
                                
#num_volunteers_by_sem_year
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

#2007-2020 Average: 906 vols vs. 997 roles
all_avg_roles_per_year <- df_year_roles %>% summarize(average = round(mean(num_unique_roles), 0))

#2016-2020 Average: 1057 vols vs 1152 roles
five_year_avg_roles_per_year <- df_year_roles %>% filter(year >= 2016) %>%
                                                  summarize(average = round(mean(num_unique_roles), 0))

#BY SEMESTER & YEAR
#----------------------------------------------
#Group by semester and year
df_sem_year_roles <- df_roles %>% count(year, semester) %>% rename(num_unique_roles = n)

#2007-2020 Average: 227 vols vs 249 roles
all_avg_roles_per_sem <- df_sem_year_roles %>% summarize(average = round(mean(num_unique_roles), 0))

#2016-2020 Average: 264 vs 288
five_year_avg_roles_per_sem <- df_sem_year_roles %>% filter(year >= 2016) %>%
                                               summarize(average = round(mean(num_unique_roles), 0))

#AVERAGE BY SEASON
#------------------------------------
#FALL 2016-2020 AVERAGE: 271 vs 282
five_year_avg_roles_per_sem_fall <- df_sem_year_roles %>% filter(year >= 2016 & semester == "FALL") %>%
                                                          summarize(average = round(mean(num_unique_roles), 0))

#WINTER 2017-2020: 335 vs 356
five_year_avg_roles_per_sem_winter <- df_sem_year_roles %>% filter(year >= 2016 & semester == "WINTER") %>%
                                                            summarize(average = round(mean(num_unique_roles), 0))

#SPRING 2017-2020: 298 vs 339
five_year_avg_roles_per_sem_spring <- df_sem_year_roles %>% filter(year >= 2016 & semester == "SPRING") %>%
                                                            summarize(average = round(mean(num_unique_roles), 0))

#SUMMER 2016-2020: 152 vs 175
five_year_avg_roles_per_sem_summer <- df_sem_year_roles %>% filter(year >= 2016 & semester == "SUMMER") %>%
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
#   labs(title="Number of Unique Volunteer Roles by Semester Over Time",subtitle = "2007-2020",
#        x="Year", y="Number of Volunteer Roles") +
#   theme(panel.spacing = unit(5, "mm"),
#         axis.text.x = element_text(angle = 30),
#         #trouble = Top, Right, Bottom, Left
#         axis.title.y = element_text(margin = unit(c(0,5,0,0),"mm")))



#Generating Average Row - ensuring year shows up correctly and then rounding volunteer numbers
get_avg_sem_row <- function(dataframe, semester_type) {
  
  df_avg <- dataframe %>% filter(semester == semester_type) %>% summarize(across(3:6, mean)) %>% 
                    
                          add_column(year = "2007-2020 Average:", .before = "num_unique_volunteers") %>%
                          add_column(semester = semester_type, .before = "num_unique_volunteers") %>%
                          mutate(across(3:6, ~round(., 0)))
  
  return(df_avg)
}


#BY YEAR TABLE - UNIQUE VOLUNTEERS AND ROLES
#------------------------------------------------------------------------------------------------

#Generating table 
df_table_year <- inner_join(df_year_vols, df_year_roles, by = c("year")) %>%
                 mutate(margin_diff = (num_unique_roles - num_unique_volunteers),
                       percent_diff = as.integer(round((num_unique_roles - num_unique_volunteers) / num_unique_roles, 2)*100),
                       year = as.character(year)) 
 
glimpse(df_table_year)
df_avg_rows_year <- df_table_year %>% summarize(across(2:5, mean)) %>%
                              add_column(year = "2007-2020 Average:", .before = "num_unique_volunteers") %>%
                              mutate(across(2:5, ~round(., 0)))


#Putting everything together
df_table_year_final <- bind_rows(df_table_year, df_avg_rows_year) %>%
                       mutate(percent_diff = paste0(percent_diff, "%")) %>% 
                       rename(`Fiscal Year` = year,
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
                      rename(`Fiscal Year` = year,
                             Semester = semester,
                             `Total Number of Unique Volunteers` = num_unique_volunteers,
                             `Total Number of Volunteer Roles` = num_unique_roles,
                             `Number of Roles Filled by Volunteers Covering More Than One Role` = margin_diff,
                             `Percentage of Roles Filled by Volunteers Covering More Than One Role` = percent_diff)
                    

list_of_tables <- list("By Year" = df_table_year_final,
                       "By Semester" = df_table_sem_final)
write.xlsx(list_of_tables, file = "./Output/WEC Unique Volunteers and Roles By Semester and Year: FY2007-FY2020.xlsx", row.names = FALSE)


