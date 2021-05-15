#########Focal Denmark

# get raster data
library(raster)
library(mapview)
dk1 <- getData('SRTM', lon = 9, lat= 56)
plot(dk1)
dk2 <- getData('SRTM', lon = 11, lat= 56)
plot(dk2)
dk3 <- getData('SRTM', lon = 9, lat= 51)
plot(dk3)
dk4 <- getData('SRTM', lon = 11, lat= 51)
plot(dk4)


dk <- mosaic(dk1, dk2, dk3, dk4, fun = mean)


mapview(dk)


plot(dk)
e <- drawExtent(show = TRUE, col = "red")
dk_crop <- crop(dk, e)

mapview(dk_crop) + mapview(dk_borders)

# get elevation data
dk_el <- getData("alt", country = "DK", mask = TRUE)
plot(dk_el)

# get admin data
dk_borders <- getData("GADM", country = "DK", level = 0)
dk_regions <- getData("GADM", country = "DK", level = 1)


# Calculate prominence
# run f.prom function first
f.prom <- function(x) perc(x,x[length(x)/2],"lt")

# Apply it in focal environment of cells by cells
f1 <- focal(Kaz_sm, w=matrix(1,nrow=21,ncol=21)  , fun=f.prom) #21*30 = 630 wrapped in focal function, the f.prom performs much faster!
plot(f1)
