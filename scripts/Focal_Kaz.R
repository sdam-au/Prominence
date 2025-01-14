## Here instead of making my own, I feed the prominence function into the focal 
## function of the raster package. It is faster, but the result is basically the same.

library(easypackages)

install.packages(c("raster", "FSA"))
libraries("raster", "FSA") #easypackages

# read in text DEM from Blue Mountains (2m res, 1000 cells a side)
Kat <- raster("landscape_prominence/data/Katoomba201804-LID2-AHD_2486260_56_0002_0002_2m.asc")

# read in DEM from Kazanlak (30m res, 2000 cells a side)
Kaz <- raster("landscape_prominence/data/Aster.tif")

res(Kaz)
ncell(Kaz)

res(Kat)
ncell(Kat)
plot(Kaz)
Kat

################ APPLY MY FUNCTION
# run the function
prom_radius(Kaz, 21) #21 m radius 

# wrong number of cells

## Crop by rows and columns
e <- extent(Kaz, 500, 1700, 900, 2100)
plot(Kaz); plot(e, add=TRUE, col = "red")
Kaz_sm <- crop(Kaz, extent(Kaz,  500, 1700, 900, 2100))
Kaz_sm
# run again

prom_radius(Kaz_sm, 500) # 500m radius 


############ APPLY FOCAL
f1 <- focal(Kaz_sm, w=matrix(1,nrow=21,ncol=21)  , fun=f.prom) #21*30 = 630 it is much faster!
plot(f1)
writeRaster(f1, "/work/landscape_prominence/output/prominence600m.tif", format="GTiff" , overwrite= TRUE)

f630 <- raster("/work/landscape_prominence/output/prominence600m.tif") 
plot(f1, main = "This is the ASTER in Kaz, prminence within 630m radius of any point")

plot(Kaz); plot(f1, add = TRUE)

hist(f1, xlab = "% visible cells from location")

plot(f1)


## Let's try large radii
f1500 <- focal(Kaz_sm, w=matrix(1,nrow=51,ncol=51)  , fun=f.prom) #51*30 = 1530, it is much faster!
plot(f1500, main = "Prominence within 1530m radius of any point")
writeRaster(f1500, "/work/landscape_prominence/output/prominence1500m.tif", format="GTiff" , overwrite= TRUE)
hist(f1500, xlab = "% visible cells from location")

