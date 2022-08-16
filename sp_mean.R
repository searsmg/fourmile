library(raster)
library(sp)
library(tidyverse)
library(rgdal)
library(osmdata)
library(sf)
library(mapview)


setwd('D:/CPF/snowpersistence_2010_2020')

#pull in all filenames with .tif
names <- list.files('.', pattern='.tif', full.names=F)

#stack, stack, stack it up
allrasties <- stack(names)

meanrastie <- calc(allrasties, fun = mean, na.rm=T)

#clip to CO
states <- raster::getData("GADM", country = "United States", level = 1)
states

state_proj <- spTransform(states, crs(meanrastie)) %>%
  st_as_sf(state_proj)

co <- state_proj %>%
  filter(NAME_1 == 'Colorado')

co_sp <- crop(meanrastie, extent(co))

plot(co_sp)

writeRaster(co_sp, filename = 'co_sp_2020to2020.tif')


mapview(co_sp, col.regions = list("brown", 'purple', 'blue', 'white'), at = seq(0, 100, 25))
