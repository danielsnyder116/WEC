library(dplyr)
library(ggplot2)
#Gets rid of scientific notation
options(scipen = 999)
setwd("/Users/Daniel/Documents/wec")

df_finance <- read.csv('financial_data_2000_to_20200115.csv')

#Exploratory Data Analysis / Time Series Analysis
#-----------------------------------
#Summary statistics
# Visualize using ggplot enrollment times - day of the week; look over year to

glimpse(df_finance)

df_19 <- df_finance %>% filter(., year==2019)

#Overall days
ggplot(df_finance, aes(x=day, fill=day)) + geom_bar(color='darkblue')

#2019
ggplot(df_19, aes(x=day, fill=day)) + geom_bar(color='black')

# Eventually combine with previous data on demographics


# df_finance %>% aggregate(.,  by=student_id, FUN=sum)
View(df_finance %>% group_by(course, year) %>% summarize(n()))

#Look at how many classes average student takes at WEC 

#How many payments student has made to WEC
# View(count(df_finance, vars=student_id, name='num_pmts_made'))


#Grouping and counting number of students who fall into
#each category of number of payments made
View(count(count(df_finance, vars=student_id), n))

#n is number of classes, nn is number of students in category,
#and prop is proportion of all students
test <- df_finance %>% count(student_id) %>% count(n) %>% mutate(prop=nn/sum(nn))


#------------------------------------------


#How many classes each student has taken

#Grouping and counting number of students who fall into
#each category of number of classes taken







#Get start date for each semester and see how enrollment falls compared to start date


#DATA SCIENCE
#See if we can create a predictive model regarding which students are more likely
# to A. Not fully pay their course amount
#    B. Have low attendance
#    C. Not continue classes
#    D. Drop out mid-semester












