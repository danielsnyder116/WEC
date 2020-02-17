library(dplyr)
library(tidyr)
library(nomine)

setwd("C:/Users/602770/downloads/volunteer/wec/output_data")

df <- read.csv("basic_demographics_2000_2019_0110.csv", stringsAsFactors = FALSE)

#Filling in NAs for employment - forgot to earlier
df <- df %>% mutate(., employment_status = na_if(employment_status, ''))

#Using predictive model to input ethnicity of students 

#Identifying which students have no data on ethnicity or have unclear "Non-Hispanic/Latino"

#Number of NAs, Blanks, or "Non-Hispanic/Latino" = 19,284 (55.1%)

#Total Students - 35,024
nrow(df)

#Unique Total Students - 14384
View((df %>% select(first_name, last_name) %>% unique(.)))

#Number of each incorrect/missing value

nrow(df %>% filter(is.na(ethnicity)))
nrow(df %>% filter(ethnicity == "Non-Hispanic/Latino"))
nrow(df %>% filter(ethnicity == ""))

df_ethnicity <- df %>% filter(is.na(ethnicity) | ethnicity == "" | ethnicity == "Non-Hispanic/Latino") %>%
  select(., first_name, last_name)

#----------------------------------------------------------
#Formatting to "First_Name Last_Name" for the algorithm
df_ethnicity_unique <- df %>% filter(is.na(ethnicity) | ethnicity == "" | ethnicity == "Non-Hispanic/Latino") %>%
  select(., first_name, last_name) %>% unique(.)

df_ethnicity_unique <- df_ethnicity_unique %>% unite(., col=total, first_name, last_name, sep=" ")


df_ethnicity_unique <- df_ethnicity_unique %>% mutate(., eth_1="", prob_1="",
                                                      eth_2="", prob_2="")



ethnic_result <- get_ethnicities(ethnicity_NAs, warnings=TRUE)

