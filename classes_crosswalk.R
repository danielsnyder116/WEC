library(dplyr)
library(stringr)

setwd("C:/Users/602770/Downloads/volunteer/wec/Students/Totals_Transfers_Dropped")

df <- read.csv("Processed/2000-2019_COMPLETE-manual-student-numbers_includes-dropped-transferred.csv")

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
