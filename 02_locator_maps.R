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
  filter(STATEFP == "09")

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
  filter(CD116FP == "01")



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





# 
# ### Creating maps for MULTIPLE STATES at the same time ##### --------------------------
# 
# #if there's a need to create congressional maps of more than one state, we can do that
# #all at once by creating a function that we'll then feed our state choices into
# 
# #create the function
# #we'll use the state ABBREVIATION for clarity this time
# 
# make_state_map <- function(stateabbr){
#   #choose state
#   cd_onestate <- cd %>% 
#     filter(state == stateabbr)
#   # create cd map for the state
#   mymap_test <-  tm_shape(cd_onestate) +
#     tm_polygons(id = "GEOID") +
#     tm_text("CD116FP", size = .5)
#   #export file to pdf
#   filename = paste("output/districtmap_", stateabbr, ".pdf")
#   tmap_save(mymap_test, filename)
# }
# 
# #try for just one state
# make_state_map('CA')
# 
# 
# #try for all
# 
# #create a state code vector for all states
# us_states_vector <- unique(fips_codes$state)[1:51]
# 
# #first make vector of fips codes for the states we want
# targetstates <- us_states_vector
# #now loop through them all using purrr's walk() function
# walk(targetstates, make_state_map)
# 
# #if all goes well, you should now see all of the generated pdfs in the "output" directory
# 
# 
# 
# 
