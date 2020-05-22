##########################################################################################
#
# PROMINENCE FUNCTION RADIUS
#
###########################################################################################

# This script shows how we can calculate landscape  prominence in a projected DEM from Blue Mountains, NSW, 
# which has sides specified at 2000m each and resolution of 2 m, and is projected in GRS80 UTM 56S

# this function is useful when the user seeks to interrogate prominence at a specific distance from central point
# knowing the raster resolution is not necessary, it is automatically extracted from the raster metadata.


# read in raster from GeospatialExperiment folder
input <- raster("./data/Katoomba201804-LID2-AHD_2486260_56_0002_0002_2m.asc")


prom_radius <- function(input, radius) {  #length of neighborhood is defined by desired radius in meters
  #e.g. radius in m will determine the size of passing window / neighborhood 
  
  # check for required packages
  if(!require(raster)){
    install.packages("raster")
    library(raster)
  } 
  if(!require(FSA)){
    install.packages("FSA")
    library(FSA)
  }
  
  # define the function to be run in the neighborhood
  f.prom <- function(x) perc(x,x[length(x)/2],"lt")    # perc is a function from FSA package, 
  # in this case the perc() function calculates percentage of cells lower than the central one 
  # lt, gt, leg, get are the arguments for lower, greater, lower or equal, and greater or equal 
  
  # check input raster has even sides ## WHY IS THAT NEEDED? CAN WE RELAX THIS REQUIREMENT?
        #I thought for moving window statistic a raster much have odd number of pixels to a row and that was it?
  if(res(input)[1]*dim(input)[1]!=res(input)[2]*dim(input)[2]) {print("Raster has uneven sides")
  } else { 
    print(paste(res(input)[1]*dim(input)[1],"is the length (in m) of both raster sides, all is copacetic"))
    
    # define size of neighborhood
    img_res <- res(input)[1]  # how many meters are represented by a raster cell
    window_side <- radius*2   # length of neighborhood side in  meters 
    cells <- as.integer(window_side/img_res)  # number of cells that make the length of neighborhood
    if ((cells %% 2) == 0) {
      print(paste(cells,"is an even number. Neighborhood size must be uneven to calculate. Revise radius"))   # toggle radius in light of image res
    } else {
      r_prom <- raster(as.matrix(as.integer(focal(input, matrix(1,cells,cells), f.prom, pad = T, padValue = 0))))
      print("Working on the plot now")
      
      par(mfrow=c(1,2))
      plot(input, main = "This is the original raster \n Ruined Castle 2 m DEM")
      plot(r_prom, main = "Prominence within input radius \n of any spot within the DEM") # HOW CAN I PRINT THE INPUT RADIUS?
      }
  } 
}

prom_radius(Kat, 21)

### WRITE TO FILE
# this file will not render in FIle Explorer, but will render in ArcMAP or back in R
writeRaster(r_prom, filename=file.path("outputs/", "your_name"), format = "GTiff",
            datatype= "INT2S", overwrite = TRUE, NAflag = 9999)

### NEXT TASK : PARALELLIZE

# https://gis.stackexchange.com/questions/213225/processing-vector-to-raster-faster-with-r/213376



system.time(prom_radius(Kat, 21))  
# takes forever, or 48 secs on a 2m res raster!



