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
  daym <- FedData::get_daymet(
    #--- supply the vector data in sp ---#
    template = as(fourmile, "Spatial"),
    #--- label ---#
    label = "fourmile",
    #--- variables to download ---#
    elements = c("srad", 'dayl'),
    #--- years ---#
    years = 2010:2020 #update
  )
)

#srad as stars
rad_stars <- st_as_stars(daym$srad)
dayl_stars <- st_as_stars(daym$dayl)

#get into date values
date_values <- rad_stars %>% 
  #--- get band values ---#
  st_get_dimension_values(., "band") %>% 
  #--- remove X ---#
  gsub("X", "", .) %>% 
  #--- convert to date ---#
  ymd(.)

date_values <- dayl_stars %>% 
  #--- get band values ---#
  st_get_dimension_values(., "band") %>% 
  #--- remove X ---#
  gsub("X", "", .) %>% 
  #--- convert to date ---#
  ymd(.)


#get into 3d
st_set_dimensions(rad_stars, 3, values = date_values, names = "date" ) 
st_set_dimensions(dayl_stars, 3, values = date_values, names = "date" ) 

#view it
mapview(rad_stars)

#move it back to sf as points instead of polygon
rad <- st_as_sf(rad_stars, as_points = T)
dayl <- st_as_sf(dayl_stars, as_points = T)

#change the crs
rad <- st_transform(rad, 4326)
dayl <- st_transform(dayl, 4326)

#match the crs to rad data
fourmile <- st_transform(fourmile, 4326)

#intersect the points w/ polygon
rad_filter <- st_intersection(rad, fourmile)
dayl_filter <- st_intersection(dayl, fourmile)

#view points within watershed
mapview(rad_pivot) + mapview(fourmile)

#pivot data
rad_pivot <- rad_filter %>%
  mutate(id = seq_len(nrow(.))) %>%
  pivot_longer(!c(geometry, id), names_to = 'date', values_to = 'solrad') %>%
  mutate(date = gsub('X', '', date),
         date = ymd(date)) 

dayl_pivot <- dayl_filter %>%
  mutate(id = seq_len(nrow(.))) %>%
  pivot_longer(!c(geometry, id), names_to = 'date', values_to = 'dayl') %>%
  mutate(date = gsub('X', '', date),
         date = ymd(date)) 

day_pivnew <- dayl_pivot %>%
  select(-c(geometry)) %>%
  as_tibble()

rad_pivnew <- rad_pivot %>%
  select(-c(geometry)) %>%
  as_tibble()
  
pivot <- full_join(rad_pivnew, day_pivnew, by = c('date', 'id'))

pivot <- pivot %>% #(srad (W/m2) * dayl (s/day)) / l,000,000)
  mutate(sol_use = (solrad * dayl)/1000000)
  

#pivot back, format date for Ages
rad_pivot_new <- pivot %>%
  mutate(date = format(as.Date(date), '%d.%m.%Y')) %>%
  mutate(id_new = paste0('daymet_', id)) %>%
  as_tibble() %>%
  select(-c(geometry.x, geometry.y, id, solrad, dayl)) %>%
  pivot_wider(names_from = id_new, values_from = sol_use) %>%
  select(c(date, daymet_54, daymet_36, daymet_33, daymet_30, daymet_26, daymet_9, daymet_6))

#csv ready for Ages
write.csv(rad_pivot_new, 'daymet_srad.csv')

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

