library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Totals_Transfers_Dropped")

#Reading in data on total enrolled students along with number transferred and dropped
df_numbers <- read.csv("Processed/2000-2019_student-numbers_includes-dropped-transferred.csv", stringsAsFactors = FALSE)

#Formatting for join
df_numbers <- df_numbers %>% mutate(class_name = str_to_upper(class_name))

#Reading the filled in data back in 
df_details <- read.csv("Processed/FILLED_manual-class-info.csv", stringsAsFactors = FALSE)

#Getting rid of extra space used in cleaning
df_details <- df_details %>% mutate(class_name = str_squish(class_name),
                                    semester = as.character(semester))

#Zero values missing for 2015-2019 data, nice!
nrow(df_details %>% filter( year >= 2015 & DAYS=="\\?"))


########### PART 1A: VISUALIZING NUMBER OF CLASSES ########### 
########### OFFERED BY SEMESTER FROM 2000 TO 2019 ########### 

#Using factor to create levels of semester so the seasons go in correct order
df_viz <- df_details %>% mutate(semester=factor(semester, levels = c("Fall", "Winter", "Spring", "Summer")))

#For group_by in R, have to save as a data.frame and then use summarize, whereas
# python does all at once: df.groupby(['semester', 'year])'['desired column'].sum()
by_sem_year <- df_viz %>% group_by(semester,year) %>% summarize(num_classes=n()) %>% 
  filter(year < 2020)

#Filling in time values for Winter 2015 to Present classes (not hard-coded like pre-2015 data)
seasons <- c("#F28E2B", "#4E79A7", "#59A14F", "#EDC948")

ggplot(data=by_sem_year, aes(x=year, y=num_classes, fill=semester)) + geom_point(shape=21, size=2) +
  facet_grid(.~ semester) + stat_smooth(method="loess", span=.95, color="#76B7B2") +
  scale_fill_manual("Semester", values=seasons)

##-----------------------------------------------------------------------------------

########### PART 2: Combining df_details (course info) with ########### 
########## df_numbers (enrolled, transferred, dropped info) ########### 
#Note that since there was no data for conversation classes in pdfs, df_numbers is smaller
# than df_details, so left_join makes the most sense.

df <- left_join(df_details, df_numbers, by=c("class_name","semester","year"))

#Because df_numbers does not have unique class ids, there are cases when multiple classes of the 
#same level are offered in the same semester and so this is causing duplicates.
#Best way to treat this is to drop duplicates based on class_id, semester, year, and total students

#Lost one row, not sure how that happened but 3,033 out of 3,034 is good.
df <- distinct(df, class_id, class_name, semester, year, .keep_all = TRUE)

#How many classes don't have enrollment data: 14%  (426 / 3033), not too bad
nrow(df %>% filter(is.na(total_active)))


####### CREATING CLEAN CLASS NAME STANDARDS #######
#Preserving dataframe with missing class names before creating standard name
df_with_missing <- df

#Getting rid of rows with no class name
df <- df %>% filter(!is.na(class_name))

#Standardize capitalization to streamline replacement process
df <- df %>% mutate(simple_standard_class = str_pad(class_name, width=20, side="right"))

#Creating a cleaner standard
df <- df %>% mutate(simple_standard_class = str_squish(str_replace_all(simple_standard_class, 
                                           c("EVE" = "PM", "ADVANCED" = "ADV", "-W |-WK " = "-WKND",
                                             "-E " = "-PM", "CONVERSATION" = "CONV", "GRAMMAR" = "GRAM",
                                             "ESL-" = "", "CONVA" = "CONV-A", "CONVB" = "CONV-B",
                                             
                                             "CONVI" = "CONV-I", "BEGINNERS|BEGINNER" = "BEG","INTERMEDIATE" = "INTER", 
                                             "SPANISH" = "SPAN", "ENGLISH" = "ENG", "BEGCONV" = "CONV-BEG",
                                             "INTROCONV" = "CONV-INTRO",  "LITERACY" = "LIT", 
                                             "COMPUTERS" = "COMP","COMPUTER" = "COMP", "VPLUS" = "V-PLUS", 
                                             "SUNAM" = "SUN-AM", "-S " = "-SUN-AM", "VLOWADV" = "V-LOW-ADV", 
                                             "^SC" = "C", "^SB" = "B", "-SUMMIT" = "", "-SOUT|SOUT" = "",
                                             
                                             "EL/|\\(|\\)" = "", "MAR " = "MAR-SUN-AM", "BASIC" = "INTRO",
                                             "ADV-N" = "5A", "ADV-A" = "5A", "ADV-R" = "5B", "ADV-B" = "5B",
                                             "ADV-PLUS-N" = "6A", "ADV-PLUS-A" = "6A",
                                             "ADV-PLUS-R" = "6B", "ADV-PLUS-B" = "6B",
                            
                                             "ADV-PLUS-CONV" = "CONV-ADV-PLUS", "ADV-PLUSCONV" = "CONV-ADV-PLUS",
                                             "ADVCONV" = "CONV-ADV", "ADV-CONV" = "CONV-ADV",
                                             "^ADV-PLUS" = "6", "^ADV-PLUS-I" = "6-I", "^ADV-PLUS-II" = "6-II",
                                             
                                             "FOR-" = "", "MANAGEMENT" = "MGMT", "^ADV-I" = "5-I", "^ADV-II" = "5-II",
                                             "CITIZENSHIP" = "CITIZEN", "CAREER-EXPLORATION" = "CAREER-EXPLOR",
                                             "MICROSOFT" = "MS", "OFFICE-ASSISTANT" = "OFFICE-ASSIST",
                                             "POWER-POINT" = "PP", "INTRODUCTION" = "INTRO",
                                             
                                             "-I |-II |-III " = "", "-I-|-II-|-III-" = "-", "AI" = "A", "BI" = "B",
                                             "6-II" = "6", "CIVICS-INTRO-A-II" = "CIVICS-INTRO-A", "6N" = "6A",
                                             "^EL-" = "", "EXPLORATIONS" = "EXPLOR", "INTCONV" = "CONV-INT",
                                             
                           #Although we standardized it in the above replacements, getting
                           #rid of the day/time for a simple version with simply content level
                           #Will create a more complete class name with all information below.
                                            
                                             "-AM|-PM|-WKND|-SAT|-SUN|-TTH|-MTH|-MW|-E |-P |-WK |\\.|'S" = "",
                                             "-E-" = "-", "6BR" = "6B","-PREP" = "", "PREINTRO" = "PRE-INTRO",
                                             "-PREP-" = "-", "INTRO-TO-COMP" = "COMP-INTRO", "INT-COMP" = "COMP-INTER",
                                             "-INT-" = "-INTER-", "-INT " = "-INTER", "COMP-101" = "COMP-INTRO",
                                             "COMP-102" = "COMP-INTER", "^ADV  " = "5", "-TRANING" = "",
                                             "MS-WORD-OFFICE-ASSISTS" = "MS-WORD-OFFICE-ASSIST", " " = ""
                                                       
                                           ))))

#Down to 116 unique class names from 504
length(unique(df$simple_standard_class))

#Bringing back in data after Claudia filled in info.
df <- read.csv("Processed/2000-2020_updated-class-details_includes-dropped-transfers-totals.csv", stringsAsFactors = FALSE)

#Replacing class name that is unclear
df <- df %>% mutate(simple_standard_class = str_replace(simple_standard_class, "PENDING-AGRICULTURE/FOOD", NA_character_))

df <- df %>% mutate(complete_standard_class = paste0(simple_standard_class, "_", DAYS, "_", TIME))



#So now back up to 3,033 classes with 27 NA class names brought back in 
#df <- full_join(df, df_with_missing)

#Creating classification column to allow for grouping
df <- df %>% mutate(class_category = case_when(  
                      str_detect(simple_standard_class, pattern = "CONV|ADV-GRAM|ADV-FINAL") ~ "ESL Conversation",
                      str_detect(simple_standard_class, pattern = "WRITING") ~ "ESL Conversation & Writing",
                      str_detect(simple_standard_class, pattern = "ADV-WORKPLACE|ASSIST|BANK|CAREER|CONSTR|CUST|FOOD|GED|HOSP|RESTAURANT") ~ "Career Education",
                      str_detect(simple_standard_class, pattern = "CIVIC|CITIZEN") ~ "Civic Education",
                      str_detect(simple_standard_class, pattern = "[:digit:]|^INTRO|PRE-INTRO") ~ "ESL Comprehensive",
                      str_detect(simple_standard_class, pattern = "COMP|INTERNET|MS|WINDOWS") ~ "Computer & Internet Literacy",
                      str_detect(simple_standard_class, pattern = "BOOK|LIT") ~ "English & Spanish Literacy",
                      str_detect(simple_standard_class, pattern = "DC|HEALTHY|KITCHEN|MGMT") ~ "Personal Well-being"))                                   
                   





#FINAL SAVE
write.csv(df, "Output/2000-2020_final-class-details_includes-dropped-transfers-totals.csv", row.names=FALSE)




#It is finished...for now haha
#write.csv(df, "Output/2000-2020_class-details_includes-dropped-transfers-totals.csv", row.names=FALSE)

#Review dataframe for Claudia
#df_missing <- df %>% filter(is.na(class_name) | is.na(DAYS) | DAYS=="?")

#write.csv(df_missing, "Processed/missing-data-for-claudia.csv", row.names = FALSE)



############# OLD CODE GRAVEYARD - RIP IN PIECES #############
# df <- df %>% mutate(standard_class_name = case_when(
#         str_detect(class_name, pattern="EVE|Eve") ~ str_replace(class_name, pattern="EVE|Eve", replacement ="PM" ),
#         
# ))
#                                                     
# #Mini-Custom For Loop to Make sure order of elements is correct - not needed since S was for Summer not Sunday
# 
# for (i in 1:nrow(df)) {
#   
#   #Anchor symbol indicates beginning of string
#   if (str_detect(df$standard_class[i], "^S")) {
#     
#     #Using the C to ensure we don't get rid of SPA for Spanish
#     df$standard_class[i] <- str_replace(df$standard_class[i], pattern="^SC", replacement = "C")
#     
#     #Getting rid of the extra spaces added at the end to help with replacement
#     df$standard_class[i] <- str_squish(df$standard_class[i])
#     
#     #Putting S on the end to later convert
#     df$standard_class[i] <- paste0(df$standard_class[i], "-S")
#     
#     #Adding back in space to work below
#     df$standard_class[i] <- str_pad(df$standard_class[i], width=20, side="right")
#   }
#   
# }
#Taking out the extra spaces used to help with replacement
#Identifying class names without day / time
# df <-  df %>% mutate(standard_class=str_squish(standard_class),
#                      time=str_extract(standard_class, pattern="AM|PM"),
#                      day=str_extract(class_name, pattern="MW|MTH|TTH|SUN|WKND"))                                  
# 
# 
# #We know that weekend classes always take place during the AM so adding in
# 
# for (i in 1:nrow(df)) {
#   
#   if(str_detect(df$standard_class[i], pattern="WKND")) {
#     
#     df$time[i] <- "PM"
#     
#   }
# }
# 
# result <- distinct(df, class_name, year)
