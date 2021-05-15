# test
library(FSA)
# define the function to be run in the neighborhood
f.prom <- function(x) perc(x,x[length(x)/2],"lt")    # percentage of cells lower than the central one 
f.noprom <- function(x) perc(x,x[length(x)/2],"gt") 
f.eqprom <- function(x) perc(x,x[length(x)/2],"leq")
f.noeqprom<- function(x) perc(x,x[length(x)/2],"geq")
#lt, gt, leq, geq are the arguments for lower, greater, lower or equal, and greater or equal 


r_prom <- raster(as.matrix(as.integer(focal(Kat, matrix(1,11,11), f.prom, pad = T, padValue = 0))))
r_noprom <- raster(as.matrix(as.integer(focal(Kat, matrix(1,11,11), f.noprom, pad = T, padValue = 0))))

# plot
par(mfrow=c(2,2))
plot(r_prom);plot(r_noprom)
hist(r_prom);hist(r_noprom)

r_eqprom <- raster(as.matrix(as.integer(focal(Kat, matrix(1,11,11), f.eqprom, pad = T, padValue = 0))))
r_noeqprom <- raster(as.matrix(as.integer(focal(Kat, matrix(1,11,11), f.noeqprom, pad = T, padValue = 0))))

par(mfrow=c(2,2))
plot(r_eqprom);plot(r_noeqprom)
hist(r_eqprom);hist(r_noeqprom)


#############  Testing raster reclassification and aggregation
library(raster)
plot(Kat)
m <- c(0,400,1,
       400,600,2,
       600,800,3,
       800,1000,4,
       1000,2000,NA)
rclmat <- matrix(m, nrow = 5, byrow = TRUE)
Katrc <- reclassify(Kat, rcl = rclmat)
plot(Katrc)


hist(Kat)
res(Kat)
ncell(Kat)
file.size("data/Katoomba201804-LID2-AHD_2486260_56_0002_0002_2m.asc")
Kat_sm <- aggregate(Kat,fact = 20, fun = mean)
ncell(Kat_sm)
res(Kat)
plot(Kat)
plot(Kat_sm)
writeRaster(Kat_sm, "data/Kat_sm_mean.tif")
file.size("data/Kat_sm.tif")
file.size("data/Kat_sm_mean.tif")
