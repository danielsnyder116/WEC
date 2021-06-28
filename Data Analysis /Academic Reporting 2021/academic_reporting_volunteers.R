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

setwd("/Users/Daniel/Desktop/WEC/Data/All (2006 - Present)")

#Needed to split Spring and Summer since we didn't differentiate
#Writing tutors - 391 so 196 for spring and 195 summer
#Conversation tutors - 361 so 180 for spring and 181 for summer
#Did manually using google sheets and download as same file _final

#Bringing in data & Ensuring semesters are treated as factors
df <- read.csv("all_volunteers_2006_2021_final.csv", stringsAsFactors = FALSE) %>% 
                mutate(semester=factor(semester, levels = c("WINTER", "SPRING", "SUMMER", "FALL")))

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
df_year <- df_unique_vols %>% count(year) %>% rename(num_unique_volunteers = n)

#2006-2021 Average: 828
all_avg_volunteers_per_year <- df_year %>% filter(year < 2021) %>% 
  summarize(average = round(mean(num_unique_volunteers), 0))

#2016-2020 Average: 1048
five_year_avg_volunteers_per_year <- df_year %>% filter(year >= 2016 & year < 2021) %>%
  summarize(average = round(mean(num_unique_volunteers), 0))

#BY SEMESTER & YEAR
#----------------------------------------------
#Group by semester and year
df_sem_year <- df_unique_vols %>% select(c(name, semester, year)) %>% distinct() %>% 
                   count (year, semester) %>% rename(num_unique_volunteers = n)

#2006-2021 Average: 218
all_avg_volunteers_per_sem <- df_sem_year %>% filter(year < 2021) %>% 
                                                  summarize(average = round(mean(num_unique_volunteers), 0))

#2016-2021 Average: 262
five_year_avg_volunteers_per_sem_ALL <- df_sem_year %>% filter(year >= 2016 & year < 2021) %>%
                                                   summarize(average = round(mean(num_unique_volunteers), 0))
#FALL 2016-2021 AVERAGE: 271
five_year_avg_volunteers_per_sem_fall <- df_sem_year %>% filter(year >= 2016 & 
                                                                year < 2021 & 
                                                                semester == "FALL") %>%
                                                         summarize(average = round(mean(num_unique_volunteers), 0))
#WINTER 2017-2021: 325
five_year_avg_volunteers_per_sem_winter <- df_sem_year %>% filter(year >= 2016 & 
                                                                  semester == "WINTER") %>%
                                                           summarize(average = round(mean(num_unique_volunteers), 0))
#SPRING 2017-2021: 300
five_year_avg_volunteers_per_sem_spring <- df_sem_year %>% filter(year >= 2017 &
                                                                  semester == "SPRING") %>%
                                                           summarize(average = round(mean(num_unique_volunteers), 0))
#SUMMER 2016-2020: 152
five_year_avg_volunteers_per_sem_summer <- df_sem_year %>% filter(year >= 2016 & 
                                                                  year < 2021 & 
                                                                  semester == "SUMMER") %>%
                                                           summarize(average = round(mean(num_unique_volunteers), 0))


#Setting colors for each semester
seasons <- c("#4E79A7", "#59A14F", "#EDC948","#F28E2B")

A <- ggplot(data=df_sem_year, aes(x=year, y=num_unique_volunteers, fill=semester)) + geom_point(shape=21, size=2) +
          facet_grid(.~ semester) + stat_smooth(method="loess", span=.95, color="#76B7B2") +
          scale_fill_manual("Semester", values=seasons) +
          scale_x_continuous(breaks=c(2005,2010,2015,2020, 2022)) +
          ylim(0,900) +
          labs(title="Number of Unique Volunteers by Semester Over Time",subtitle = "2006-2021",
               x="Year", y="Number of Volunteers") + 
          theme(panel.spacing = unit(5, "mm"),
                axis.text.x = element_text(angle = 30),
                #trouble = Top, Right, Bottom, Left
                axis.title.y = element_text(margin = unit(c(0,5,0,0),"mm")))
        

df_avg_semester <- df_indiv %>% group_by(semester) %>% 
                                summarize(average = round(mean(num_unique_volunteers), 0))





#--------------------------------------------#
#---------- UNIQUE VOLUNTEER ROLES ----------#
#--------------------------------------------#
