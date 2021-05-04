#########Focal Yambol

# libraries

library(raster)
library(mapview)
library(tidyverse)
library(sf)


# get elevation data
bg_el <- getData("alt", country = "BG", mask = TRUE)
plot(bg_el)
hist(bg_el)

# get admin data
bg_borders <- getData("GADM", country = "BG", level = 0)
bg_regions <- getData("GADM", country = "BG", level = 1)
plot(bg_el); plot(bg_regions, add=TRUE)


bg_regions <- st_as_sf(bg_regions)
Yambol <- bg_regions %>% 
  dplyr::filter(NAME_1=="Yambol")

Yam_el <- crop(bg_el, Yambol)
plot(Yam_el)


# Calculate prominence
library(FSA)
# run f.prom function first
f.prom <- function(x) perc(x,x[length(x)/2],"lt")

# Apply it in focal environment on pixels rather than meters of radius (as it's un-projected)
yam_prompix21 <- focal(Yam_el, w=matrix(1,nrow=21,ncol=21), fun=f.prom) #10 pixel radius 
plot(yam_prompix21)
hist(yam_prompix21)

bg_prompix21 <- focal(bg_el, w=matrix(1,nrow=21,ncol=21), fun=f.prom)
plot(bg_prompix21)


# Load mounds

mounds <- st_read("landscape_prominence/data/mounds.shp")
mounds$prom21pix <- raster::extract(bg_prompix21, st_transform(mounds, 4326))
hist(yam_prompix21); hist(mounds$prom21pix, add =TRUE)

st_write(mounds, "landscape_prominence/output/moundsprom.shp")
writeRaster(yam_prompix21, "landscape_prominence/output/Yamprom21pix.tif", format = "GTiff")
writeRaster(bg_prompix21,"landscape_prominence/output/BGprom21pix.tif", format = "GTiff")
