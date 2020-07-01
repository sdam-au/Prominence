# Sampling rasters at coordinates

# Libraries
library(raster)
library(rgdal)
library(FSA)
library(sf)
methods(raster)

# load data
Yam <- raster("E:/TRAP Workstation/Shared GIS/Satellite imagery/ASTER/DEM/ASTGTM_N42E026/prjYAM_DEM_N42E026.tif")
Yam # projected WGS84 35N raster 27m resolution, 4112x3053


mounds <- st_read("C:/Users/Adela/Documents/Professional/Projects/MQNS/GIS/Vectors/200910Verified.shp")
st_crs(mounds)


# Plot
plot(mounds$geometry)
plot(Yam)
plot(mounds$geometry, pch = 17, add = TRUE)


# Prerequisites to sampling - Coordinate system alignment

all.equal(crs(Yam),st_crs(mounds)) # does not look so, but maybe we should interpret
crs(Yam)
crs(mounds)
bbox(Yam)
st_bbox(mounds)

st_coordinates(mounds)
?st_coordinates
mound_coordinates <- data.frame(st_coordinates(mounds))
tail(mound_coordinates)
dim(mounds)

# Sample elevations at mound locations

elevation <- raster::extract(Yam, mound_coordinates)
elev_Yammnds <- data.frame(mound_coordinates, mounds$TRAP,elevation=
                           raster::extract(Yam, mound_coordinates))


# Sample prominence at mound locations
# https://geocompr.robinlovelace.net/spatial-operations.html#spatial-raster-subsetting
# https://mgimond.github.io/megug2017/


# the prominence function

f_prom = function(x){perc(x,x[length(x)/2],"lt")}



# Point Approach: define points and create buffers (300m, 600m, 1000, and 5000m), then use teh 'extract' function to compute the f_prom from Yam raster within the polygons defined by buffers.

mnd_buff100 <- st_buffer(mounds, dist = 100)
plot(mnd_buff100$geometry, add = TRUE)

mnd_buff1000 <- st_buffer(mounds, dist = 1000)
plot(mnd_buff1000$geometry, add=TRUE)

# point approach of getting a single statistic from a buffer
?perc
?focal
?extract

mnd_perc = function(x){
  perc(x,x[length(x)/2],"lt", 
       na.rm = FALSE,  # making this TRUE will bring the result up
       digits = 2)
}

# USING A BUFFER WORKS WITH COMMON FUNCTIONS
mound100_avg <- raster::extract(Yam, mnd_buff100, fun=mean, sp = TRUE )
mound100_pixels <- raster::extract(Yam, mnd_buff100, fun=length, sp = TRUE ) # does not work with buffer polygon
# it gives the error 2 arguments passed to 'length' which requires 1



# USING COORDINATES WORKS BETTER 

mound100_pixels <- raster::extract(Yam, 
                                mound_coordinates, # centroids
                                buffer = 100, # buffer size, units depend on CRS
                                fun = length, # Works!
                                df = TRUE)
test_100buff<- raster::extract(Yam, 
                                   mound_coordinates, # centroids
                                   buffer = 100, # buffer size = 3 cells, units depend on CRS
                                   fun = function(x){perc(x,x[length(x)/2],"lt", na.rm = FALSE, digits = 2)}, # PERCENT lower than coordinate
                                   df = TRUE)

# lets add raster extracted data directly to the dataframe                               
mounds$prom100mbuff<- raster::extract(Yam, 
                                      mound_coordinates, # centroids
                                      buffer = 100, # 3 cell kernel, units depend on CRS
                                      fun = function(x){perc(x,x[length(x)/2],"lt", na.rm = FALSE, digits = 2)}) # PERCENT lower than coordinate
                  
mounds$prom1000mbuff <- raster::extract(Yam, 
                                          mound_coordinates, # centroids
                                          buffer = 1000, # 30 cell kernel, units depend on CRS
                                          fun = function(x){perc(x,x[length(x)/2],"lt", na.rm = FALSE, digits = 2)})



#####################################################
# analyze results in Mounds: TENTATIVE
library(tidyverse)

summary(mounds$prom100mbuff)
hist(mounds$prom100mbuff, col)
hist(mounds$prom1000mbuff, add=TRUE)

# difference between the 100m and 1000m buffer prominence 
mounds <- mounds %>% 
  mutate(diff = prom1000mbuff - prom100mbuff, fct_prom = cut(prom1000mbuff, breaks= 5))

# count the ratios per quantile
mounds %>% 
  group_by(fct_prom) %>% 
  count()

# plot the quantiles
ggplot() +
  geom_bar(data = mounds, aes(fct_prom))


## Compare results with raster
summary(Yam)

Yam_df <- as.data.frame(Yam, xy = TRUE)
tail(Yam_df)
ggplot() +
  geom_histogram(data=Yam_df, aes(prjYAM_DEM_N42E026))

  

Yam_df <- Yam_df %>%
  mutate(fct_elevation = cut(prjYAM_DEM_N42E026, breaks = 10))

ggplot() +
  geom_bar(data = Yam_df, aes(fct_elevation)) +
  geom_bar(elev_Yammnds, aes(elevation)) # fix this second line

plot(Yam)
plot(mounds$geometry, col = mounds$prom1000mbuff, add = TRUE)

# creating lists of all vaues inside buffer
#https://www.neonscience.org/extract-values-rasters-r

plot(Yam)

# does not work
ggplot() + 
  geom_raster(Yam_df, aes(x = x, y = y, fill = prjYAM_DEM_N42E026)) + 
  scale_fill_viridis_c() +
  coord_quickmap()

  geom_sf(mounds$geometry, color = mounds$prom1000mbuff)

# HW for students ; FIX THIS FUNCTION; it does not work now
mnd_perc =function(x){perc(x,length[x]/2,"lt", na.rm = FALSE)}
mound100_prom <- raster::extract(Yam, mnd_buff100, fun = mnd_perc, sp = TRUE ) # OF COURSE LENGTH IS NOT WORKING ## well, that's because you are usign wrong brackets

