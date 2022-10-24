## Pulling daymet solar rad data for Fourmile watershed

# Used some code from here: 
#https://tmieno2.github.io/R-as-GIS-for-Economists/daymet-with-daymetr-and-feddata.html

library(tidyverse)
library(lubridate)
library(rgdal)
library(sp)
library(sf)
library(stars)
library(daymetr)
library(here)
library(mapview)
library(FedData)
library(lubridate)
library(terra)

#load in Fourmile watershed polygon
fourmile <- st_read(here('./data/GIS/Fourmile Watershed.shp'))

#view it
mapview(fourmile)

#download srad data for Fourmile polygon
(
  srad <- FedData::get_daymet(
    #--- supply the vector data in sp ---#
    template = as(fourmile, "Spatial"),
    #--- label ---#
    label = "fourmile",
    #--- variables to download ---#
    elements = c("srad"),
    #--- years ---#
    years = 2010:2020
  )
)

#srad as stars
rad_stars <- st_as_stars(srad$srad)

#get into date values
date_values <- rad_stars %>% 
  #--- get band values ---#
  st_get_dimension_values(., "band") %>% 
  #--- remove X ---#
  gsub("X", "", .) %>% 
  #--- convert to date ---#
  ymd(.)

#get into 3d
st_set_dimensions(rad_stars, 3, values = date_values, names = "date" ) 

#view it
mapview(rad_stars)

#move it back to sf as points instad of polygon
rad <- st_as_sf(rad_stars, as_points = T)

#change the crs
rad <- st_transform(rad, 4326)

#match the crs to rad data
fourmile <- st_transform(fourmile, 4326)

#intersect the points w/ polygon
rad_filter <- st_intersection(rad, fourmile)

#view points within watershed
mapview(rad_pivot) + mapview(fourmile)

rm(srad) #not using anymore (taking up memory)
rm(rad_stars) #not using anymore (taking up memory)

#pivot data
rad_pivot <- rad_filter %>%
  mutate(id = seq_len(nrow(.))) %>%
  pivot_longer(!c(geometry, id), names_to = 'date', values_to = 'solrad') %>%
  mutate(date = gsub('X', '', date),
         date = ymd(date))

#saved the downloaded as Rdata so don't have to download everytime
load('daymet_rad.Rdata')

#pivot back, format date for Ages
rad_pivot_new <- rad_pivot %>%
  #mutate(date = format(as.Date(date), '%d.%m.%Y')) %>%
  mutate(id_new = paste0('daymet_', id)) %>%
  as_tibble() %>%
  select(-c(geometry, id)) %>%
  pivot_wider(names_from = id_new, values_from = solrad)

#csv ready for Ages
write.csv(rad_pivot_new, 'daymet_srad_all.csv')

## below code is only to pull the lat lon out of R to view pixel locations
#create a df of geometry and id to load into GIS
daymet_points <- rad_pivot %>%
  select(c(id, geometry)) %>%
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>%
  distinct()

#this can be added to the chunk above
daymet_points <- daymet_points %>%
  select(-c(geometry))

#GIS file for daymet ready
write.csv(daymet_points, 'daymet_points.csv')

