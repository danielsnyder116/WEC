library(googledrive)
library(googlesheets4)
library(cronR)
library(dplyr)

#--------------------------
#     VOLUNTEER DATA
#--------------------------

#INITIAL INTAKE INFO
url_address <- "https://docs.google.com/spreadsheets/d/1u9QMnrfzPAZjMOvFBatvq5HJPRr_LyByzdfKDeZLIn0/edit#gid=1089355021"

read_she

df <- read_sheet(url_address, sheet="Form Responses 1", col_names = as.character(1:95))

#Getting questions to convert to good column names
orig_col_names <- df %>% slice(1)

#Getting rid of first row with questions
df <- df %>% slice(-1)


#------------------------------------------------------------------------

#CURRENT ROSTER


#------------------------------------------------------------------------
