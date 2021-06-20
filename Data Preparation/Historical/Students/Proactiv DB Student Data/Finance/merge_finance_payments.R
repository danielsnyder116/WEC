library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Finance-Payments/")

#Bringing in Payment Method Data
df_method <- read.csv("Payment Methods/2000-2020_payment-methods.csv", stringsAsFactors = FALSE)


#Bringing in previous financial information to create more complete data set
df_finance <- read.csv("Finance/Output/2000-2020-01-15_financial-data.csv", stringsAsFactors = FALSE, encoding = 'UTF-8')


#Ensuring data types match
df_method <- df_method %>% mutate(year=as.integer(year), 
                                month=as.character(month), 
                                day=as.character(day),
                                student_id = as.character(student_id))


#Confirming matching data types
glimpse(df_finance)
glimpse(df_method)


#Not using names as there are issues with double last names when cleaning up the long string
df_combined <- full_join(df_finance, df_method, by=c("student_id","year", "month", "day"))

#Getting rid of .y names since they had more issues
df_combined <- df_combined %>% select(-c(last_name.y, first_name.y))

#Renaming columns YAY this is so easy with tidyverse
df_combined <- df_combined %>% rename(last_name = last_name.x,
                                      first_name = first_name.x)


#Get rid of duplicates
df_combined <- distinct(df_combined)

#Drop rows where no name (most of these didn't match because the transactions were
#from after January 15th when I pulled the original financial data)
df_combined <- df_combined %>% filter(!is.na(last_name))


nrow(df_combined)

534 / 54507

View(count(df_combined, vars=pmt_method))


write.csv(df_combined, "final_financial-data-with-payment-method_2000-2020.csv", row.names=FALSE)


