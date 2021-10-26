library(tidyverse)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)
library(tidycensus)
options(tigris_class = "sf") #this option ensures the tigris geospatial files are in sf (simple feature) format


#pull geographic boundary files for the all districts using the tigris package
#for national map, 20m likely better here. For single states, 5m. 
counties <- counties(cb = TRUE, resolution = "5m") 
states <- states(cb = TRUE, resolution = "5m") 

glimpse(counties)

#select just the target states needed using its FIPS code
# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
counties <- counties %>% 
  filter(STATEFP %in% c("17", "29"))

states <- states %>% 
  filter(STATEFP %in% c("17", "29"))


### Creating the map with TMAP ##### -----------------------------------------

#generate the map of our one selected state's congressional districts
tm_shape(states) +
  tm_polygons(id = "GEOID") +
tm_shape(counties) +
  tm_polygons(id = "GEOID") +
  tm_text("NAME", size = .2)
