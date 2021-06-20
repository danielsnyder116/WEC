library(ggplot2)
library(dplyr)
library(forcats)
library(RColorBrewer)
library(gganimate)

setwd("/Users/Daniel/Desktop")
data <- read.csv("student-class-blackbaud-input_2000-2019.csv")




anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP per Year : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data")


# For GIF
animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))


# For MP4
animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4
anim_save("animation.mp4", animation = for_mp4 )