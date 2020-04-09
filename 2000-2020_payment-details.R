library(dplyr)
library(stringr)
library(rJava)
library(tabulizer)
library(tibble)
library(tidyr)
library(lubridate)

setwd("C:/Users/602770/Downloads/volunteer/wec/Payment Methods")

file <- list.files()[1]

#Gets all the text as one long string - best option
pdf_text <- extract_text(file, encoding="latin1")

#Splitting on Totals to separate data for each payment method
pdf_text <- as.data.frame(str_split(pdf_text, pattern='Total:'), stringsAsFactors = FALSE)


##### Getting strings in a clean format #####
#Issue with setting first split string as header so for now, saving as new row and appending back on 
extra_row <- as.data.frame(colnames(pdf_text), stringsAsFactors = FALSE)

colnames(extra_row) <- "V1"
colnames(pdf_text) <- "V1"

pdf_text <- bind_rows(pdf_text, extra_row)
colnames(pdf_text) <- 'data'


#Slicing the dataframe to isolate each payment type string
nrow(pdf_text)

one <- slice(pdf_text, 1)
two <- slice(pdf_text, 2)
three <- slice(pdf_text, 3)
four <- slice(pdf_text, 4)


#dataframes <- list(one, two, three, four)
# for (frame in dataframes) {
#
#}

###### THIS WILL EVENTUALLY BE WITHIN A FOR LOOP ###### 
###### TO TAKE CARE OF EACH PAYMENT TYPE STRING  ###### 
###### AND THEN MERGE BACK TOGETHER AT THE END   ###### 


#Isolating relevant information regarding assessments
one <- as.data.frame(str_split(one, pattern="\r\n"))

colnames(one) <- "data"

#Getting rid of columns with student id and names (student id is at least 5 digits thus the pattern)
one <- one %>% filter(str_detect(data, pattern="\\d\\d\\d\\d\\d"))

#Geting year column separately using regex/str_detect
one <- one %>% mutate(year=str_extract(data, pattern="\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d"))

#Split the student id, last name, and first name up into new columns by " ", and get rid of everything else
one <- one %>% separate(data, c("student_id", "last_name", "first_name"), sep = " ", extra="drop")


#Simplify year column - convert year to lubridate form using mdy to match format
# and then use the year method to select the year part, and then put that under the 'year' column
one <- one %>% mutate(year= year(mdy(year)))


