library(tidyverse)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)
library(tidycensus)
options(tigris_class = "sf") 


#pull geographic boundary files for the all districts using the tigris package
#for national map, 20m likely better here. For single states, 5m. 
cd <- congressional_districts(cb = TRUE, resolution = "5m")  

#select just the target state needed using its FIPS code
# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
cd_onestate <- cd %>% 
  filter(STATEFP == "48")

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
cd_onestate <- inner_join(cd_onestate, fips_statelookup, by = c("STATEFP" = "state_code"))

cd_onestate


## now just pick a single district to highlight in the state
cd_onedist <- cd_onestate %>% 
  filter(CD116FP == "33")



### Creating the map with TMAP ##### -----------------------------------------

#generate the map of our one selected state's congressional districts
tm_shape(cd_onestate) +
  tm_polygons(id = "GEOID") 

#now overlay the one district to highlight
tm_shape(cd_onestate) +
  tm_polygons(id = "GEOID") +
tm_shape(cd_onedist) +
  tm_polygons(id = "GEOID", col = "purple") +
  tm_text("CD116FP", size = .5)


#let's save the step above into a new object name instead
mymap <- tm_shape(cd_onestate) +
           tm_polygons(id = "GEOID") +
         tm_shape(cd_onedist) +
           tm_polygons(id = "GEOID", col = "purple") +
           tm_text("CD116FP", size = .5)

#run it
mymap

#we can either use the "export" button directly from the RStudio viewer to save as pdf...
#...or do it using the following code:
tmap_save(mymap, "output_locators/mymap.svg")






### Creating maps for MULTIPLE STATES at the same time ##### --------------------------

#function

make_state_map <- function(stateabbr, distnum){
  #choose state
  cd_onestate <- cd %>%
    filter(state == stateabbr)
  #choose district number
  cd_onedist <- cd_onestate %>% 
    filter(CD116FP == distnum)
  #create map
  mymap_locator <- tm_shape(cd_onestate) +
    tm_polygons(id = "GEOID") +
    tm_shape(cd_onedist) +
    tm_polygons(id = "GEOID", col = "purple") +
    tm_text("CD116FP", size = .5)
  #export file to svg
  filename = paste0("output_locators/", stateabbr, "-", distnum, ".svg")
  tmap_save(mymap_locator, filename)
}

#try for just one at a time
make_state_map("CA", "03")
make_state_map("CT", "01")
make_state_map("OH", "10")
make_state_map("TX", "33")
make_state_map("UT", "02")
make_state_map("WA", "09")


#try for all at once

#create vector pairs for desired districts
states_vector <- c("CA", "CT", "OH", "TX", "UT", "WA")
dists_vector  <- c("03", "01", "10", "33", "02", "09")

#now loop through them all using purrr's walk2() function
walk2(states_vector, dists_vector, make_state_map)






