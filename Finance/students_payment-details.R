library(rJava)
library(tabulizer)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(lubridate)

setwd("C:/Users/602770/Downloads/volunteer/wec/Payment Methods")

file <- list.files()

#Gets all the text as one long string - best option
pdf_text <- extract_text(file, encoding="UTF-8")

#Splitting on Totals to separate data for each payment method
pdf_text <- as.data.frame(str_split(pdf_text, pattern='Total:'), stringsAsFactors = FALSE)

colnames(pdf_text) <- 'data'

#Slicing the dataframe to isolate each payment type string
nrow(pdf_text)

one <-   slice(pdf_text, 1)
two <-   slice(pdf_text, 2)
three <- slice(pdf_text, 3)
four <-  slice(pdf_text, 4)
five <-  slice(pdf_text, 5)
six <-   slice(pdf_text, 6)
seven <- slice(pdf_text, 7)
eight <- slice(pdf_text, 8)
nine <-  slice(pdf_text, 9)
ten <-   slice(pdf_text, 10)
eleven <- slice(pdf_text, 11)
twelve <- slice(pdf_text, 12)


#Wanted to do this so it would loop through a list of dataframes and
# take care of this all at once but getting stuck and not worth trying 
# to figure it out at the moment. Blargh but carry on we shall.

cleaner <- function(frame) {
    #frame <- five
  
    #Isolating relevant information regarding assessments
    frame <- as.data.frame(str_split(frame, pattern="\r\n"))
    
    colnames(frame) <- "data"
    
    #Getting rid of columns with student id and names (student id is at least 5 digits thus the pattern)
    frame <- frame %>% filter(str_detect(data, pattern="\\d\\d\\d\\d\\d"))
    
    #Geting year column separately using regex/str_detect
    frame <- frame %>% mutate(date=str_extract(data, pattern="\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d"))
  
    #Also being mindful of cases with students who have two last names not connected by a hypen
    #Thankfully, we have dates to use in merge along with student id so works out well
    #Split the student id, last name, and first name up into new columns by " ", and get rid of everything else
    frame <- frame %>% separate(data, c("student_id", "last_name", "first_name", "extra"), sep = " ", extra="drop")
  
    #Simplify year column - convert year to lubridate form using mdy to match format
    # keeping it as full date to avoid issues with duplicates
    frame <- frame %>% mutate(date= mdy(date), pmt_method="")
    
return (frame)

}

one <- cleaner(one)
two <- cleaner(two)
three <- cleaner(three)
four <- cleaner(four)
five <- cleaner(five)
six <- cleaner(six)
seven <- cleaner(seven)
eight <- cleaner(eight)
nine <- cleaner(nine)
ten <- cleaner(ten)
eleven <- cleaner(eleven)
twelve <- cleaner(twelve)

#Adding in appropriate payment method and name
df_missing      <- one %>% mutate(pmt_method=NA)
df_amer_express <- two %>% mutate(pmt_method="American Express")
df_cash         <- three %>% mutate(pmt_method="Cash")
df_check        <- four %>% mutate(pmt_method="Check")
df_credit       <- five %>% mutate(pmt_method="Credit Card")
df_debit        <- six %>% mutate(pmt_method="Debit Card")
df_mastercard   <- seven %>% mutate(pmt_method="Credit Card")
df_other        <- eight %>% mutate(pmt_method="Other")
df_scholarship  <- nine %>% mutate(pmt_method="Scholarship")
df_third_party  <- ten %>% mutate(pmt_method="Third Party")
df_visa         <- eleven %>% mutate(pmt_method="Credit Card")
df_waiver       <- twelve %>% mutate(pmt_method="Waiver")


#Setting it up to bind each dataframe together
df_final <- df_missing

dataframes <- list(df_amer_express, df_cash, df_check,
                   df_credit, df_debit,df_mastercard,
                   df_other,df_scholarship, df_third_party,
                   df_visa, df_waiver)

#Concatenating dataframes together
for (dataframe in dataframes) {
  df_final <- bind_rows(df_final, dataframe)
}

#Distribution for each payment method
count(df_final, vars=pmt_method) 

#Adding date information in to aid in merge with financial payments dataframe
df_final <- df_final %>% mutate(year=year(df_final$date),
                                month=month(df_final$date, label=TRUE),
                                day=wday(df_final$date, label=TRUE))

df_final <- df_final %>% select(student_id,last_name,first_name,
                                pmt_method, year, month, day)

write.csv(df_final, "../Database/2000-2020_payment-methods.csv", row.names=FALSE)

