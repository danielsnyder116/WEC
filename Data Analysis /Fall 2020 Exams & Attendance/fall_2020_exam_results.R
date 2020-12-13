library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

setwd("/Users/Daniel/Desktop/Fall 2020 Analysis")

files <- list.files(pattern="*.csv$")

for (file in files) {
  
  if (file == files[1]) {
    df_base <- read.csv(file, stringsAsFactors = FALSE)
    
    #Renaming columns by replacing all spaces (converted to dots) as one "_"
    df_base <- rename_with(df_base, ~str_replace_all(., "\\.+", "_"))
  }
  
  else {
    df <- read.csv(file, stringsAsFactors = FALSE)
    
    #Renaming columns by replacing all spaces (converted to dots) as one "_"
    df <- rename_with(df, ~str_replace_all(., "\\.+", "_"))
    
    df_base <- bind_rows(df_base, df)
  }
}

df <- df_base



names(a)

names(df)

name.vector




