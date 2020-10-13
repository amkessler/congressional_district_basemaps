library(tidyverse)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)
library(tidycensus)
options(tigris_class = "sf") #this option ensures the tigris geospatial files are in sf (simple feature) format


#pull geographic boundary files for the all districts using the tigris package
#for national map, 20m likely better here. For single states, 5m. 
cd <- congressional_districts(cb = TRUE, resolution = "5m")  

#select just the target state needed using its FIPS code
# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
cd_onestate <- cd %>% 
  filter(STATEFP == "06")

#let's see what we've got
cd_onestate


#bring in fips table from tidycensus to add state abbrev
head(fips_codes) #built into tidycensus

#create subset of unique state name, code combos
fips_statelookup <- fips_codes %>% 
  as_tibble() %>% 
  select(state, state_code) %>% 
  distinct()

fips_statelookup

#join
cd <- inner_join(cd, fips_statelookup, by = c("STATEFP" = "state_code"))

#join just for the one state
cd_onestate <- inner_join(cd_onestate, fips_statelookup, by = c("STATEFP" = "state_code"))


#Could we do this differently? Yes, could filter from the full cd table into the one state AFTER joining.
#Now we have the state abbreviation to use instead of just the fips code
cd_onestate <- cd %>% 
  filter(state == "CA")

cd_onestate



### Creating the map with TMAP ##### -----------------------------------------

#generate the map of our one selected state's congressional districts
tm_shape(cd_onestate) +
  tm_polygons(id = "GEOID") 


#generate it again but this time adding district number labels
tm_shape(cd_onestate) +
  tm_polygons(id = "GEOID") +
  tm_text("CD116FP", size = .5) #this line adds the labels


#let's save the step above into a new object name instead
mymap <-tm_shape(cd_onestate) +
  tm_polygons(id = "GEOID") +
  tm_text("CD116FP", size = .5)

#run it
mymap


#we can either use the "export" button directly from the RStudio viewer to save as pdf...
#...or do it using the following code:
tmap_save(mymap, "mymap.pdf")







