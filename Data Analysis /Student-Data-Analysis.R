library(ggplot2)

setwd("/Users/Daniel/Desktop")
data <- read.csv("student-class-blackbaud-input_2000-2019.csv")

data <- data %>% mutate(semester=factor(semester, levels = c("FALL", "WINTER", "SPRING", "SUMMER")))

unique(data$semester)

by_sem_year <- data %>% group_by(semester, year) %>% summarize(test = count(unique(class_id)))
                                                               

seasons <- c("#F28E2B", "#4E79A7", "#59A14F", "#EDC948")

# NUMBER OF CLASSES BY SEASON OVER THE YEARS
ggplot(data=by_sem_year, aes(x=year, y=num_classes, fill=semester)) + geom_point(shape=21, size=2) +
  facet_grid(.~ semester) + stat_smooth(method="loess", span=.95, color="#76B7B2") +
  scale_fill_manual("Semester", values=seasons)



