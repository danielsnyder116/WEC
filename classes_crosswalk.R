library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Totals_Transfers_Dropped")

df_numbers <- read.csv("Processed/2000-2019_student-numbers_includes-dropped-transferred.csv", stringsAsFactors = FALSE)
df_details <- read.csv("Processed/2020-04-16_manual-class-info.csv", encoding='UTF-8', stringsAsFactors = FALSE)

glimpse(df_numbers)
glimpse(df_details)


##### Cleaning up df_details #####

df_details <- df_details %>% select(Class.ID., Class.Name., Term., DAY, TIME)
colnames(df_details) <- c("class_id", "class_name", "term", "DAYS", "TIME")


#Dropping NA rows due to copy and paste spacing from proactive
df_details <- df_details %>% drop_na(class_id)
rownames(df_details) <- 1:nrow(df_details)

#Replace blank class names with NA
df_details <- df_details %>% mutate(class_name=na_if(class_name, ""))
  #df_details %>% filter(is.na(class_name))


#Splitting up semester and year
df_details <- df_details %>% separate(term, c("semester", "year"), sep=" ", extra="drop")
df_details <- df_details %>% mutate(year=as.integer(year))

#Formatting class name to match df_numbers - need to use _all so it replaces not just first intance of pattern
df_details <- df_details %>% mutate(class_name=str_replace_all(class_name, pattern=" ", replacement = "-"))


#Clarifying some values - more explicit to avoid confusion/error
  # MTH PM = MTWTH 7PM-9PM
  # TTH AM = TTH   10AM-1PM
  # WKND PM = WKND 2PM-5PM
  # MSUN = These should only be two days a week - still need to determine which days over time ugh
  #   for now: MSUN = ?
  # NA NA = SUN 9AM-12PM

#Most of the NAs are cases where it was listed as MSUN 9AM-12AM
#NA NA = SUN 9AM-12PM -Specifically only one day long each week

#DAYS Fixes
df_details <- df_details %>% mutate(DAYS = str_replace_all(DAYS, c("MTH" = "MTWTH", "MSUN" = "?",
                                                                   "TTHU" = "TTH", "NA-12" = NA_character_)))
#Replacing NAs in DAYS
df_details <- df_details %>% mutate(DAYS = replace_na(DAYS, "SUN"))

#TIME Fixes, Have to use other case to avoid other data in column from being converted to NA
df_details <- df_details %>% mutate(TIME = case_when((str_detect(DAYS, pattern="WKND") & str_detect(TIME, pattern="^PM")) ~ "2PM-5PM",
                                        (str_detect(DAYS, pattern="MTWTH|MTW|TTH|\\?") & str_detect(TIME, pattern="^PM")) ~ "7PM-9PM",
                                         str_detect(TIME, pattern="^AM") ~ "10AM-1PM", TRUE ~ TIME))


#Replacing 12AMs in TIME
df_details <- df_details %>% mutate(TIME = str_replace_all(TIME, pattern="12AM", replacement="12PM"))

#Replacing NAs in TIME
df_details <- df_details %>% mutate(TIME = replace_na(TIME, "9AM-12PM"))

#Filling in NAs for error entries - need to use NA_character_ for replacement
df_details <- df_details %>% mutate(TIME = str_replace_all(TIME, pattern="12PM-12PM|NA-12", replacement=NA_character_))
                                    

View(count(df_details, vars=TIME))

# df_details %>% filter(DAYS=='NA')
# df_details %>% filter(TIME=="12PM")


#Filling in time values for Winter 2015 to Present classes (not hard-coded like pre-2015 data)
#NEEDS TO BE DONE











#Combining df_details (course info)  with df_numbers (enrolled, transferred, dropped info)
#Note that since there was no data for conversation classes in pdfs, df_numbers is smaller
# than df_details, so left_join makes the most sense.

df <- left_join(df_details, df_numbers, by=c("class_name","semester","year"))

#Because df_numbers does not have unique class ids, there are cases when multiple classes of the 
#same level are offered in the same semester and so this is causing duplicates.
#Best way to treat this is to drop duplicates based on class_id, semester, year, and total students

#Lost one row, not sure how that happened but 3,033 out of 3,034 is good.
df <- distinct(df, class_id, class_name, semester, year, .keep_all = TRUE)




####### CREATING CLEAN CLASS NAME STANDARDS #######

#Getting rid of rows with now class name
df <- df %>% filter(class_name != "")

#Standardize capitalization to streamline replacement process
df <- df %>% mutate(standard_class = str_pad(str_to_upper(class_name), width=20, side="right"),
                    class_name = str_to_upper(class_name))


#Mini-Custom For Loop to Make sure order of elements is correct

for (i in 1:nrow(df)) {
  
  #Anchor symbol indicates beginning of string
  if (str_detect(df$standard_class[i], "^SC")) {
    
    #Using the C to ensure we don't get rid of SPA for Spanish
    df$standard_class[i] <- str_replace(df$standard_class[i], pattern="^SC", replacement = "C")
    
    #Getting rid of the extra spaces added at the end to help with replacement
    df$standard_class[i] <- str_squish(df$standard_class[i])
    
    #Putting S on the end to later convert
    df$standard_class[i] <- paste0(df$standard_class[i], "-S")
    
    #Adding back in space to work below
    df$standard_class[i] <- str_pad(df$standard_class[i], width=20, side="right")
  }
  
}


df <- df %>% mutate(standard_class = str_replace_all(standard_class, 
                                      c("EVE" = "PM", "ADVANCED" = "ADV", "-W " = "-WKND",
                                        "-TTH" = "-TTH-AM", "-E " = "-PM", "-MW" = "-MW-AM",
                                        "ESL-" = "", "CONVA" = "CONV-A", "CONVB" = "CONV-B",
                                                       
                                        "CONVI" = "CONV-I", "BEGINNERS" = "BEG","INTERMEDIATE" = "INT", 
                                        "SPANISH" = "SPA","ENGLISH" = "ENG", "BEGCONV" = "CONV-BEG",
                                        "INTROCONV" = "CONV-INTRO", "EL/" = "", "LITERACY" = "LIT", 
                                        "COMPUTER" = "COMP", "VPLUS" = "V-PLUS", "SUNAM" = "SUN-AM",
                                        "SPAN" = "SPA", "-S " = "-SUN-AM","VLOWADV" = "V-LOW-ADV", 
                                        
                                        "\\(|\\)" = "", "MAR " = "MAR-SUN-AM", "BASIC" = "INTRO",
                                        "ADV-N|ADV-A" = "5A", "ADV-R|ADV-B" = "5B",
                                        "ADV-PLUS-N|ADV-PLUS-A" = "6A","ADV-PLUS-R|ADV-PLUS-B" = "6B",
                                        
                                        "^ADV-PLUS " = "6", "^ADV-PLUS-I" = "6-I", "^ADV-PLUS-II" = "6-II"
                                                       #### "^ADVPLUS|^ADV-PLUS" = "6",                                            
                                                     )))

#Taking out the extra spaces used to help with replacement
#Identifying class names without day / time
df <-  df %>% mutate(standard_class=str_squish(standard_class),
                     time=str_extract(standard_class, pattern="AM|PM"),
                     day=str_extract(class_name, pattern="MW|MTH|TTH|SUN|WKND"))                                  
                                                 

#We know that weekend classes always take place during the AM so adding in

for (i in 1:nrow(df)) {
  
  if(str_detect(df$standard_class[i], pattern="WKND")) {
    
    df$time[i] <- "PM"
    
  }
}

result <- distinct(df, class_name, year)




#write.csv(result, "data-for-crosswalk_classes.csv", row.names=FALSE)

# df <- df %>% mutate(standard_class_name = case_when(
#           
#         str_detect(class_name, pattern="EVE|Eve") ~ str_replace(class_name, pattern="EVE|Eve", replacement ="PM" ),
#         
# ))
#                                                     
