# Daycare Access Scores with floating catchment areas for Arlington, VA at census block group level 
# in 2021 using 2019 ACS (for daycare demand approximation)
# packages
library(tigris)
library(sf)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidycensus)
library(osrm)
library(catchment)
library(leaflet)
library(htmlwidgets)
library(webshot)

# set working folder
setwd("~/daycare_fca_va")

#######################
# DAYACRE DATA PREP
#######################
# Daycare data is collected from VA Department of Social Services 
# The geographical coordinates of the daycares location are approximated using tidygeocoder with Google Maps API

# load daycare data (collecated from https://dss.virginia.gov/facility/search/cc2.cgi)
daycare_va <- read_csv("data/daycare_lonlat.csv")

# find which census block group a daycare located in
va_bg <- block_groups("VA")

# drop if coordinates are missing
daycare_va <- daycare_va[!is.na(daycare_va$latitude), ]
daycare_va <- daycare_va[!is.na(daycare_va$longitude), ]
# lon and lat to geo-points
geopts <- daycare_va %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

# indeces of bgs which contain a geopoint
inds <- st_within(geopts$geometry, va_bg$geometry, sparse=T)
bg_list <- c()
# add block group GEOIDs 
for (i in inds){
  if (identical(va_bg$NAME[i[1]],character(0))){
    bg_list <- append(bg_list, NA)}
  else{
    bg_list <- append(bg_list, va_bg$GEOID[i[1]])}
}
daycare_va['geoid'] <- bg_list

# keep only Arlington county
daycare013 <- daycare_va[substr(daycare_va$geoid,1,5) == "51013",]

############################
# SUPPLY (DAYCARE CAPACITY)
############################

# drop if capacity is missing
daycare013 <- daycare013[is.na(daycare013$capacity) == F,]

# create new supply
supply <- data.frame(daycare013$longitude, daycare013$latitude, 
                     daycare013$geoid, daycare013$capacity, daycare013$...1)

colnames(supply) <- c("lon", "lat", "GEOID", "capacity", "daycare_id")

# drop wrong capacities
supply <- supply %>% subset(nchar(as.character(capacity)) < 5)

#####################################
# DEMAND (CHILDREN UNDER AGE OF 15)
#####################################

# The Census American Community Survey data is accessed using the Census API with tidycensus
# To access ACS a free public access API key is needed 
# It can be obtained at https://api.census.gov/data/key_signup.html (needs to be activated at first access)

# installed census api key
Sys.getenv("CENSUS_API_KEY")

# population under 15 years in NCR
va.bg <- get_acs(geography = "block group",
                  year = 2019,
                  variables = c(male_under5 = "B01001_003",
                                male_5_9 = "B01001_004", 
                                male_10_14 = "B01001_005", 
                                female_under5 = "B01001_027", 
                                female_5_9 = "B01001_028", 
                                female_10_14 = "B01001_029"), # population under 15
                  state = c("VA"),
                  county = "Arlington",
                  survey = "acs5",
                  output = "wide",
                  geometry = TRUE)

va.bg <- va.bg %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  pop_under15 = (male_under5E + male_5_9E + male_10_14E +
                   female_under5E + female_5_9E + female_10_14E),
  geometry = geometry
)
# find coordinates of the center of block groups
bg_centroid <- va.bg %>% 
  st_centroid()
demand <- bg_centroid %>% extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)', 
                                  convert = TRUE) 
demand <- as.data.frame(demand)

# drop where the block group coordinates are NaN
demand <- demand[is.na(demand$lon) == FALSE,]
demand <- demand[is.na(demand$lat) == FALSE,]

############################
# DRIVE TIMES (Open Street)
############################

supply$daycare_id <- as.character(supply$daycare_id) 
demand_res <- demand %>% select(c("GEOID", "lon", "lat"))

# to use the public OSRM server split data in smaller chunks
demand_splits <- split(demand_res, (seq(nrow(demand_res))-1) %/% 100) 

# matrix to save travel times

col <- matrix(nrow=0, ncol=nrow(supply))
  
for (i in 1:length(demand_splits)){
  print(paste("Demand chunk", i, "/", length(demand_splits)))
  row <- matrix(nrow=nrow(demand_splits[[i]]), ncol=0)
  for (j in 1:nrow(supply)){
    #print(paste("Supply entry", j, "/", nrow(supply)))
    matrix <- osrmTable(
      src = demand_splits[[i]][c("GEOID", "lon", "lat")],
      dst = supply[j, c("daycare_id", "lon", "lat")]
    )$duration
    row <- cbind(row, matrix)
  }
  col <- rbind(col, row)
}
traveltimes_matrix <- col
# replace NAs in travel times matrix as zeros 
# 'catchment' packages treats zeros as NAs
traveltimes_matrix[is.na(traveltimes_matrix)] <- 0

###################################
# COMPUTE FLOATING CATCHMENT AREA
##################################
# weights for 30 and 60 mins drive times
step_weights_30 <- list(c(30, .22), c(20, .68), c(10, 1))
step_weights_60 <- list(c(60, .042), c(30, .377), c(20, .704), c(10, .962))

# two-step enhanced FCA using 30 min weights 
demand['e2sfca'] <- catchment_ratio(
  demand, supply, 
  cost=traveltimes_matrix, 
  step_weights_30,
  consumers_value = "pop_under15", 
  providers_id = "daycare_id",
  providers_value = "capacity", verbose = TRUE
) * 1000 # per 1000 of population

# 2 step FCA
demand['fca2s'] <- catchment_ratio(
  demand, supply, 
  cost=traveltimes_matrix, 
  30,
  consumers_value = "pop_under15", 
  providers_id = "daycare_id",
  providers_value = "capacity", verbose = TRUE
) * 1000

# 3-step FCA
va.bg['fca3s'] <- catchment_ratio(
  demand, supply, 
  cost=traveltimes_matrix, 
  step_weights_30,
  normalize_weight = TRUE,
  consumers_value = "pop_under15", 
  providers_id = "daycare_id",
  providers_value = "capacity", verbose = TRUE
)

#####################
# LEFALET 
#####################
# initilize the map
map_block_groups <- leaflet(va.bg, options = leafletOptions(attributionControl = FALSE)) %>%
  setView(-77.01, 38.88, 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addScaleBar("bottomleft") %>%
  addMapPane("lines", zIndex = 410) %>%
  addMapPane("points", zIndex = 411) %>%
  addLayersControl(position = "topleft", overlayGroups = "Daycare") %>%
  addCircles(
    data = supply, color = "#000", fillColor = "#000", opacity = .8, lng = ~lon, lat = ~lat,
    label = ~ paste0("ID: ", daycare_id, ", Capacity: ", capacity),
    group = "Daycare", options = pathOptions(pane = "points")
  ) %>%
  addPolygons(data = va.bg$geometry,
  fillColor = colorNumeric("RdYlBu", va.bg$pop_under15)(va.bg$pop_under15),
  fillOpacity = 1, stroke = FALSE, group = "Kids", label = va.bg$pop_under15
  ) %>%
  hideGroup("Kids") 

# add the layer of access scores to the map
pal <- colorBin("RdYlBu", va.bg$fca3s)
map_block_groups %>%
  addControl("Daycare (3-Step FCA 30 mins cutoff)", "topright") %>%
  addLegend("bottomright", pal, va.bg$fca3s, opacity = .7) %>%
  addPolygons(data=va.bg$geometry,
    fillColor = pal(va.bg$fca3s), fillOpacity = .7, weight = 1, color = "#000",
    highlightOptions = highlightOptions(color = "#fff"), group = "Access",
    label = paste0(
      "GEOID: ", va.bg$GEOID, ", Kids: ", va.bg$pop_under15,
      ", Per 1k People: ", round(va.bg$fca3s, 4), ", In Region: ",
      round(va.bg$pop_under15 * va.bg$fca3s/ 1000, 4)
    )
  )

# saving a map as an image 
# webshot("va013_daycare_fca_map.html", file = "va013_daycare_fca.png",
#        cliprect = "viewport")
