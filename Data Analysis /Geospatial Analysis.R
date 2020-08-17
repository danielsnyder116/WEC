library(ggplot2)
library(dplyr)
library(sf) #Newer version of sp and better
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(RColorBrewer)
library(leaflet)


setwd("/Users/Daniel/Desktop")
data <- read.csv("student-class-blackbaud-input_2000-2019.csv")

data <- data %>% mutate(semester=factor(semester, levels = c("WINTER", "SPRING", "SUMMER", "FALL")))

#### CHOROPLETH MAP OF STUDENTS BY ZIP CODE ####
#Can do this in Tableau as well, but good exercise
#to be able to make static maps as well as interactive ones

#https://ggplot2-book.org/index.html

theme_set(theme_bw())

world <- ne_countries(scale="medium", returnclass = "sf")
#class(world)

#Base map of the world
ggplot(world) + geom_sf() + coord_sf()

country_data <- data %>% filter(!is.na(country)) %>%
                    group_by(country) %>% summarize(count=n())




plot_data <- left_join(world, country_data, by=c("name" = "country")) 

plot_data <- plot_data %>% select(name, count, geometry)

#Chloropleth of Country Total
ggplot(plot_data) + geom_sf(aes(fill=count)) + coord_sf() +
  scale_fill_gradient(low="lightblue", high="blue", 
                      na.value="white", limits=c(10,4000),
                      breaks=c(seq(100,5000, 300)))



#By maps package
map("state")
map("county")

states <- st_as_sf(map("state", plot=FALSE, fill=TRUE))

#Need to work on zoom
ggplot(states) + geom_sf() #+ coord_sf(xlim=c(-80,-70), ylim =c(38.5, 39), expand=FALSE)


map_1 <- leaflet() %>% addTiles() %>% addMarkers(lat=38.915645,lng=-77.0512195)

map_1












## HADLEY WICKHAM EXAMPLE - WATCH AND LEARN ##
#Except it barely showed anything ARGHGHGHHH
# library(ozmaps)
# library(rmapshaper)
# oz_states <- ozmap_states %>% filter(NAME != "Other Territories") 
# ggplot(oz_states) + geom_sf()
# oz_votes <- ms_simplify(abs_ced)
# ggplot(oz_states) + geom_sf(mapping=aes(fill=NAME))
# ggplot() + geom_sf(data=oz_states, mapping=aes(fill=NAME)) + geom_sf(data=oz_votes, fill=NA)
