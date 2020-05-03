library(tidyverse)
library(sp)
library(sf)
require(ggmap)

#read in shapefile annd then converts it to sp object. don't need to add coords because it's a shapefile
zipsf<- st_read("ZIP_CODE_040114.shp")
zipdf<- sf::as_Spatial(zipsf)

#cleans and reads in csv need to add coords because it's not a spatial format
Hospitals <-read.csv("Map_of_NYC_Health_and_Hospitals.csv")

#clean and coverts csv to sf object
Hospitals_df<- Hospitals %>%
  separate(Location.1, sep="[()]", into=c("Address", "Coordinates"), remove=T) %>%
  separate(Coordinates, sep=", ", into=c("Lat", "Long"), remove=T)  %>%
  st_as_sf(coords=c("Long", "Lat"))

#convert to sf to sp object
Hospitals_df_sp <- sf::as_Spatial(Hospitals_df)

#reads in csv
FoodStores <-read.csv("NYS_Retail_Food_Stores.csv")

#convert it to sf object
Food_sf <- FoodStores %>%
  rename(County ="ï..County") %>%
  filter(County %in% c("Kings", "Queens", "New York", "Richmond", "Bronx")) %>%
  separate(Location, sep="[()]", into=c("Address", "Coordinates"), remove=T) %>%
  separate(Coordinates, sep=", ", into=c("Lat", "Lon"), remove=T)  %>%
  drop_na(Lon) %>%
  st_as_sf(coords=c("Lon", "Lat"))

#convert to sf to sp object
Food_sp <- sf::as_Spatial(Food_sf)

#plot basemap
basemap <-Food_sf %>%
  #gets coordinates of bounding box
  st_bbox() %>%
  #converts coordinates to vector
  as.vector() %>%
  #creates map
  ggmap::get_stamenmap(zoom=11)

#plots other map features
ggmap(basemap) +
  geom_point(aes(x=X, y=Y), 
             data = Food_sf %>% st_coordinates() %>% 
               tibble::as_tibble(),
             color = 'brown',
             size = 1,
             alpha = .5
  ) +
  geom_point(aes(x=X, y=Y), 
           data = Hospitals_df %>% st_coordinates() %>% 
             tibble::as_tibble(),
           color = 'green',
           size = 1,
           alpha = .5
  )