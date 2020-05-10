library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Education-Impact/Data/Output")

df <- read.csv("2015-2019_student-assessments.csv")

###################### Visualizing Data (EDA) ###########################

#Attendance distribution from 2015-2019 overall
p <- ggplot(df, aes(x=attendance)) + geom_histogram(bins=5, binwidth = .5)
p

#Attendance grouped by Semester
p_color <- ggplot(df, aes(x=attendance, fill=semester)) + 
  geom_histogram(bins=5, binwidth = .5) +
  facet_grid(.~ semester) + theme_light()
p_color


#By Year
ggplot(df, aes(x=attendance, fill=year)) + 
  geom_histogram(bins=5, binwidth = .5) +
  facet_grid(.~ semester) + theme_light() + facet_grid(.~year)

count(df, vars=year)