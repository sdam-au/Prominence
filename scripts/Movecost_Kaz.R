library(easypackages)

install.packages(c("raster", "FSA"))
install.packages("movecost")
libraries("raster", "FSA", "movecost") #easypackages

# read in text DEM from Blue Mountains (2m res, 1000 cells a side)
Kat <- raster("landscape_prominence/data/Katoomba201804-LID2-AHD_2486260_56_0002_0002_2m.asc")

# read in DEM from Kazanlak (30m res, 2000 cells a side)
Kaz <- raster("landscape_prominence/data/Aster.tif")
Kaz_sm <- crop(Kaz, extent(Kaz,  500, 1700, 900, 2100))


############################ MOVECOST
library(sf)
library(tidyverse)

## Let's load points
cities <- st_read("landscape_prominence/data/kaz_cities.shp")
cities <- cities %>% select(TYPE, MUN_CODE, REG_CODE, LNAME, LMUN_NAME, TOTAL1994) 
Kazanlak <- cities %>% filter(LNAME == "Kazanlak")

mounds <- st_read("landscape_prominence/data/mounds.shp")

Kaz_sm <- crop(Kaz, mounds)
plot(Kaz_sm); plot(mounds$geometry, add=TRUE)

## Let's get Euclidian distance
mounds$DistToKaz <- st_distance(mounds, Kazanlak, "Euclidean") # takes 1 sec
summary(mounds$DistToKaz)

## Let's simulate moving cost from Kazanlak to a few mounds, point to point
# The full Aster takes 10 mins to process; now trying with much smaller raster (extent clipped to mounds extent, and shapes ) to cut down on the 10 mins processing
MndToKazTobler <- movecost(Kaz_sm, as(Kazanlak, "Spatial"), as(mounds, "Spatial"), funct = "t",outp = "r", time = "m", export = TRUE) # Tobler on path hiking in minutes, started at 15.57, errored out at 16.07

# Check the Least Cost path length summary (in hours) - not straight lines (ca 3% increase in length)
summary(MndToKazTobler$LCPs$length)
(MndToKazTobler$accumulated.cost.raster)
hist(MndToKazTobler$accumulated.cost.raster)
hist(MndToKazTobler$dest.loc.w.cost$cost)

## This output is in ToblerOutputMinutes

MndToKazPandolf <- movecost(Kaz_sm, as(Kazanlak, "Spatial"), as(mounds, "Spatial"), funct = "p",outp = "r", time = "m", export = TRUE) #

summary(MndToKazPandolf$LCPs$length)
(MndToKazPandolf$accumulated.cost.raster)
hist(MndToKazPandolf$accumulated.cost.raster)
summary(MndToKazPandolf$dest.loc.w.cost$cost)

############### Distance to Seuthopolis
# If we consider Seuthopolis as a regional capital, how easily can you get from it to the major sacred sites of the kings and their retinue? Let's compare this with Kazanlak/
# Imagine the processions the movement in the landscape 

Seuthopolis <- data.frame(name="Seuthopolis", Lat = 42.6175092,Long =25.2799577)
Seuthopolis <- Seuthopolis %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(crs = 32635)


MndToSeuthopolisPandolf <- movecost(Kaz_sm, as(Seuthopolis, "Spatial"), as(mounds, "Spatial"), funct = "p",outp = "r", time = "m", export = TRUE) #

MndToSeuthopolisTobler <- movecost(Kaz_sm, as(Seuthopolis, "Spatial"), as(mounds, "Spatial"), funct = "t",outp = "r", time = "m", export = TRUE) # 
