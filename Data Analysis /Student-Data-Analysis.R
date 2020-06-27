library(ggplot2)
library(dplyr)

setwd("/Users/Daniel/Desktop")
data <- read.csv("student-class-blackbaud-input_2000-2019.csv")

glimpse(data)

data <- data %>% mutate(semester=factor(semester, levels = c("WINTER", "SPRING", "SUMMER", "FALL")))


#### NUMBER OF STUDENTS BY SEASON OVER TIME ####

#Each row is a unique student which is why just doing n() in summarize gives us that number
student_count <- data %>% group_by(semester, year) %>% summarize(num_students=n())
seasons <- c("#4E79A7", "#59A14F", "#EDC948","#F28E2B")
ggplot(data=student_count, aes(x=year, y=num_students, fill=semester)) + geom_point(shape=21, size=2) +
  facet_grid(.~ semester) + stat_smooth(method="loess", span=.95, color="#76B7B2") +
  scale_fill_manual("Semester", values=seasons)


#### TOTAL NUMBER OF MALE AND FEMALE STUDENTS BY SEASON OVER TIME ####
#count(data, vars=gender)
gender_ratio <- data %>% filter(!is.na(gender)) %>% group_by(semester, year, gender) %>%
                    summarize(num_gender = n())
                      
gender_colors <- c("red", "blue")

ggplot(data=gender_ratio, aes(x=year, y=num_gender, color=gender)) + geom_point(alpha=.5) +
  stat_smooth(method = "loess", span = .95, se=TRUE, alpha=.3) + facet_grid(.~semester) +
       scale_color_manual("Gender", values=gender_colors) 

#Add in title, clean axis labels and add in more ticks


# # Bar graph of gender over time - not the best way but interesting to make
# ggplot(data=gender_ratio, aes(x=gender, y=num_gender, fill=gender)) + geom_col() +
#   scale_fill_manual("Gender", values=gender_colors) + facet_grid(.~year) 
#   #use facet_wrap when one wants to have multiple x axes

#geom_bar - allow ggplot to count frequency for you
#geom_col - set the value to compare (frequency, proportion, length, etc.)




##### AGE DISTRIBUTION OVER TIME ####

#This gives the actual frequency distribution over time but since we aren't concerned 
#about total counts and more about percentages we will do another version to
#isolate that change

#Warning of removing non-finite values refers to NA values excluded - no worries

filtered_data <- data %>% filter(year > 2010)

#Basic histogram with counts of students by age
ggplot(data=filtered_data, aes(x=age)) + 
  geom_histogram(bins = 40, color='black', fill='blue') +
  facet_wrap(semester~year)

#Density histogram
ggplot(data=filtered_data, aes(x=age, fill=factor(year))) + 
  geom_histogram(aes(y=..density..), bins=20, color='black') + 
  facet_wrap(.~year) #+ geom_density()

#Clearer density plot with area under the curve
ggplot(data=filtered_data, aes(x=age)) + geom_density(fill="lightblue", alpha=.3) + 
      facet_wrap(.~year)



