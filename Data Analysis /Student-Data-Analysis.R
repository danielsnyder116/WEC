library(ggplot2)
library(dplyr)
library(forcats)
library(RColorBrewer)
library(ggthemes)
#devtools::install_github("bbc/bbplot")
library(bbplot)


setwd("/Users/Daniel/Desktop")
data <- read.csv("student-class-blackbaud-input_2000-2019.csv")

data <- data %>% mutate(semester=factor(semester, levels = c("WINTER", "SPRING", "SUMMER", "FALL")))

glimpse(data)

#### NUMBER OF STUDENTS BY SEASON OVER TIME ####

#Each row is a unique student which is why just doing n() in summarize gives us that number
student_count <- data %>% group_by(semester, year) %>% summarize(num_students=n())
seasons <- c("#4E79A7", "#59A14F", "#EDC948","#F28E2B")

A <- ggplot(data=student_count, aes(x=year, y=num_students, fill=semester)) + geom_point(shape=21, size=2) +
        facet_grid(.~ semester) + stat_smooth(method="loess", span=.95, color="#76B7B2") +
        scale_fill_manual("Semester", values=seasons) +
        scale_x_continuous(breaks=c(2000,2005,2010,2015,2019)) +
        ylim(0,1050) +
        labs(title="Number of Students by Semester Over Time",subtitle = "2000-2019",
        x="Year", y="Number of Students") + theme_stata() +
        theme(panel.spacing = unit(5, "mm"),
              axis.text.x = element_text(angle = 30),
                                            #trouble = Top, Right, Bottom, Left
              axis.title.y = element_text(margin = unit(c(0,5,0,0),"mm")))
A
#.5 is the default for vjust and hjust
#Theme changes have to come after external theme to come through

#### TOTAL NUMBER OF MALE AND FEMALE STUDENTS BY SEASON OVER TIME ####
#count(data, vars=gender)
gender_ratio <- data %>% filter(!is.na(gender)) %>% group_by(semester, year, gender) %>%
                    summarize(num_gender = n())
                      
gender_colors <- c("red", "blue")

B <- ggplot(data=gender_ratio, aes(x=year, y=num_gender, fill=gender)) + geom_point(shape=21, alpha=.8, color='black') +
          stat_smooth(method = "loess", span = .95, se=TRUE, alpha=.3) + facet_grid(.~semester) +
          scale_color_manual("Gender", values=gender_colors) +
          scale_x_continuous(breaks=c(2000,2005,2010,2015,2019)) +
          labs(title="Student Gender Ratio By Semester Over Time",
               x="Year", y="Number of Students",fill="Gender", subtitle="2000-2019") + theme_stata() +
          theme(panel.spacing = unit(4, "mm"),
                axis.text.x = element_text(angle=30))
          #+ bbc_style()

B

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

#Basic histogram with counts of students by age
C1 <- ggplot(data=data, aes(x=age)) + 
          geom_histogram(bins = 40, color='black', fill='lightgreen') +
          facet_wrap(.~semester) + #semester~year to get every case
          theme_stata() +
          labs(title="Student Age Distribution ", x="Age", y="Number of Students") +
          theme(axis.title.y = element_text(margin = unit(c(0,4,0,0), "mm")))
C1

#Density histogram
# Without year as factor it colors by intensity - categorical vs numeric 
C2 <- ggplot(data=data, aes(x=age, fill=factor(year))) + 
        geom_histogram(aes(y=..density..), bins=20, color='black') + 
        facet_wrap(.~year) + #+ geom_density()
        scale_y_continuous(breaks=c(0, 0.025, 0.05)) +
        theme_stata() +
        labs(title="Percentage Distribution of Student Age Over Time",
             subtitle="2000-2019", x="Age", y="Percent of Student Population",
             fill="Year") +
        theme(axis.title.y = element_text(margin = unit(c(0,5,0,0), "mm")),
              axis.text.y = element_text(angle = 0),
              panel.spacing.y = unit(5, "mm"))

C3

#Clearer density plot with area under the curve
C4 <- ggplot(data=data, aes(x=age)) + geom_density(fill="lightblue", alpha=.3) + 
        facet_wrap(.~year, scales = "free_x") + theme_stata() +
        labs(title="Percentage Distribution of Student Age Over Time",
              subtitle="2000-2019", x="Age", y="Percent of Student Population",
              fill="Year") +
        theme(axis.title.y = element_text(margin = unit(c(0,5,0,0), "mm")),
              axis.text.y = element_text(angle = 0),
              panel.spacing.y = unit(5, "mm"))


C4

#### MEDIAN AGE OVER TIME ####

#By Semester
median_age_sem <- data %>% group_by(semester, year) %>% filter(!is.na(age)) %>%
  summarize(med_age=median(age, na.rm = TRUE))

D <- ggplot(data=median_age_sem, aes(x=year, y=med_age)) + geom_point(shape=21, fill="#56B4E9") + 
        facet_grid(.~semester) + stat_smooth(method = "loess", span=.95) +
        theme_stata() +
        scale_x_continuous(breaks = c(2000,2005,2010,2015,2019)) +
  labs(title="Median Student Age Over Time",
       subtitle="2000-2019", x="Year", y="Age",fill="Year") +
  theme(axis.title.y = element_text(margin = unit(c(0,5,0,0), "mm")),
        panel.spacing = unit(5, "mm"),
        axis.text.x = element_text(angle=30))


D

#Year Average
median_age_year <- data %>% group_by(year) %>% filter(!is.na(age)) %>%
  summarize(med_age=median(age, na.rm=TRUE))

ggplot(data=median_age_year, aes(x=year, y=med_age)) + 
    geom_point(shape=21, fill='#48a072') +
    stat_smooth(method="loess", span=.95, color='#48a072')


#### TOP 10 COUNTRIES OF ORIGIN OVER THE YEARS ####
student_countries <- data %>% filter(!is.na(country)) %>%
                          group_by(country) %>%
                          summarize(num_country=n()) %>%
                          arrange(desc(num_country)) %>%
    #While not necessary to sort the countries from greatest to least
    #this sorts the levels of the factor, which ensures that colors
    #are assigned not in alphabetical order but by order of country
    #value which makes the graph clearer.
                          mutate(country=fct_inorder(country))

#Slice_max does the sorting for you if you want
# slice_max(student_countries, order_by=num_country, n=10)

#Overall Top 10 Countries
student_countries %>% slice(1:10) %>%
  ggplot(aes(x=reorder(country, num_country), y=num_country, fill=country)) +
  geom_bar(stat="identity", color='black') + 
  scale_fill_brewer(palette = "Set3") + coord_flip() +
  #theme(legend.position = "None") + 
  geom_text(aes(label = paste0(100*round(num_country/sum(num_country), 3), "%")),
            hjust = 1.1, size=3)

## Top 10 Countries OVER THE YEARS ##
student_country_year <- data %>% filter(!is.na(country)) %>%
                             group_by(year, country) %>%
                             summarize(num_country=n()) %>%
                    #Sort so each year has country in descending #order based on counts
                             arrange(year, desc(num_country)) #%>%
                             #mutate(country=fct_inorder(country))

#Looking at the top 10 countries over the last 20 years
#Can't use facet_wrap as y axis elements change each year
for (i in 2000:2019) {

  result <- student_country_year %>% filter(year == i) %>% slice(1:10) %>%
                ggplot(., aes(x=reorder(country, num_country), y=num_country, fill=country)) +
                geom_col() + scale_fill_brewer(palette = "Set3") + coord_flip() +
          #Trick - since it's only one year, can use facet_grid to #add the title
                facet_grid(.~year) +
                geom_text(aes(label = paste0(100*round(num_country/sum(num_country), 3), "%, ", num_country)), hjust = 1, size=3)

  print(result)

}


#### MEDIAN YEARS OF EDUCATION OVER TIME ####
median_edu_sem <- data %>% filter(!is.na(education_years)) %>%
                      group_by(year, semester) %>% 
                      summarize(med_edu = median(education_years))

ggplot(median_edu_sem, aes(x=year, y=med_edu)) + geom_point(shape=21, fill="orange") +
      geom_smooth(method="loess", span=.95, color="orange") 




#Diverging - gradient with at least two hues
#Qualitative - apples vs oranges vs bananas
#Sequential - gradient with one dominant hue

#Displays individual palette
display.brewer.pal(6, "Oranges")

wec_palette <- c("#0C0D0D","#5E605E","#FDFEFB","#91B9C","#3BE3F")

brewer.pal.info

#Displays all the palettes
display.brewer.all()

