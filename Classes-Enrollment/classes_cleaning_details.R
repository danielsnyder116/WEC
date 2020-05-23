library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Totals_Transfers_Dropped")

df_details <- read.csv("Processed/2020-04-16_manual-class-info.csv", encoding='UTF-8', stringsAsFactors = FALSE)

glimpse(df_details)

########### PART 1: CLEANING UP DF_DETAILS ###########
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
  # MSUN = These should only be two days a week 
  #   for now: MSUN = ?

#Replacing NAs in DAYS
df_details <- df_details %>% mutate(DAYS = replace_na(DAYS, "SUN"))

#DAYS Fixes
df_details <- df_details %>% mutate(DAYS = str_replace_all(DAYS, c("MTH" = "MTWTH", "MSUN" = "?",
                                                                   "TTHU" = "TTH", "NA-12" = NA_character_)))

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
                                    
#Checking results of the efforts
#View(count(df_details, vars=TIME))
# df_details %>% filter(DAYS=='NA')
# df_details %>% filter(TIME=="12PM")


#Finish filling in day and time columns for 2015-present data based on class name 
# (rather than hard-coding like for 2000 to 2014 - took 16+ hours yuck)

#Standardize capitalization to streamline replacement process
df_details <- df_details %>% mutate(class_name= str_pad(str_to_upper(class_name), width=20, side="right"))

#Taking care of DAYS
df_details <- df_details %>% mutate(DAYS=case_when( str_detect(class_name, pattern = "-E|-EVE|-MTH") ~ "MTWTH",
                                                    str_detect(class_name, pattern = "-S |-SUN") ~ "SUN",
                                                    str_detect(class_name, pattern = "-SAT") ~ "SAT",
                                                    str_detect(class_name, pattern = "-W |-WK |-WKND") ~ "WKND",
                                                    str_detect(class_name, pattern = "MW") ~ "MW",
                                                    str_detect(class_name, pattern = "TTH") ~ "TTH",
                                                    #Preserves pre-2015 days values
                                                    TRUE ~ DAYS))

#So 8.5% of classes don't have day info. in name ->->-> back to Proactive woooo
nrow(df_details %>% filter(DAYS==""))
nrow(df_details)
259/3034

write.csv(df_details, "Processed/TEMP_manual-class-info.csv", row.names=FALSE)

# Based on values seen in data, filled in most ? with SUN. There are some other cases that could be filled in
# based on the name of the class, the time, or its relationship to other classes offered during that semester
# some could not be determined (mostly before 2009) 

#So after everything, about 6% (182 / 3034) data where we don't know the days - overall better!
