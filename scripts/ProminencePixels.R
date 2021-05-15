#####################################################################

# PROMINENCE FUNCTION CELLS/PIXELS

#####################################################################

# serves to calculate prominence in unprojected rasters that
# lack the translation of pixels into distance units
# this function is suitable for arbitrary rasters, 
# and requires the user to specify moving window size only.


prom_pixels <- function(input, output, cells) {  #length of neighborhood is defined in pixel cells
  
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
  f.prom <- function(x) perc(x,x[length(x)/2],"lt")    # percentage of cells lower than the central one 
  f.noprom <- function(x) perc(x,x[length(x)/2],"gt") 
  #lt, gt, leq, geq are the arguments for lower, greater, lower or equal, and greater or equal 
  
  # check input raster has even sides 
  if(res(input)[1]*dim(input)[1]!=res(input)[2]*dim(input)[2]) {print("Raster has uneven sides")
  } else { 
    print(paste(res(input)[1]*dim(input)[1],
                "(m)is the length of both raster sides, all is copacetic"))
    
    # define size of neighborhood
    if ((cells %% 2) == 0) {
      print(paste(cells,"is an even number. Neighborhood size must be uneven to calculate. Revise your input (add 1 to your cells input)"))   # toggle radius in light of image res
    } else {
      r_prom <- raster(as.matrix(as.integer(focal(input, matrix(1,cells,cells), f.prom, pad = T, padValue = 0))))
      # r_prom is a recalculated raster, product of a focal function
      # focal function calculates moving window values for the neighborhood of focal cells 
      # using a matrix of weights, in combination with f.prom - prominence function, which 
      # calculates the percentage of cells higher/lower than the focal value
      print("Working on the plot now")
      par(mfrow=c(2,2))
      plot(input, main = "This is the original raster \n Ruined Castle 2 m DEM")
      plot(r_prom, main = paste0("Prominence within ",res(input)[1]*dim(input)[1] ,"m window \n of any spot within the DEM")) # HOW CAN I PRINT THE INPUT RADIUS? use paste()
      hist(input, xlab= "Elevation in meters" ,
           main = "Elevation distribution in the DEM")
      hist(r_prom, xlab = "%",
          main = paste0("Prominence within the window of  \n ",cells, " cell or ",res(input)[1]*dim(input)[1] ,"m"))
      # Trying to print the raster, but it is not working yet.
      if(!require(rgdal)){
        print("Not yet done, installing rgdal to create output")
        install.packages("rgdal")
        library(rgdal)
        print("writing resulting raster to file")
        writeRaster(r_prom, filename = output, format = "GTiff",  datatype= "INT2S", 
                    overwrite = TRUE, NAflag = 9999) 
      } # raster is only viewable back in R, or with a stretch in ArcMAP 
      print("All done")
    }
  } 
}

hist(Kat, xlab = "%")
?hist()



# EXAMPLE OF THE FUNCTION AT WORK

# First we need some libraries. I used the libraries rgdal, FSA, and raster. 
# If they’re not present, use the install.packages() function. By the way, I’ve just discovered the package, easypackages.
# This package will allow you to load multiple packages with just one line. I recommend it:

install.packages("easypackages")  # this library lets 
library(easypackages)

install.packages(c("raster", "FSA"))
libraries("raster", "FSA") #easypackages

# read in text DEM from Blue Mountains (2m res, 1000 cells a side)
Kat <- raster("./data/Katoomba201804-LID2-AHD_2486260_56_0002_0002_2m.asc")
Kat
# run the function, remember to specify output file name
prom_pixels(Kat, "Kattest.tif", 11)
# prom_pixels(raster(x), "test.tif", 3) # run this line if you played with creaing a  raster
prom_pixels(input, "RC_50mProm.tif", 101)

### Print the raster

if(!require(rgdal)){
  print("Not yet done, installing rgdal to create output")
  install.packages("rgdal")
  library(rgdal)
  print("writing resulting raster to file")
  writeRaster(r_prom, filename=paste0("outputs/", output), format = "GTiff",  datatype= "INT2S", 
              overwrite = TRUE, NAflag = 9999) 
} # raster is only viewable back in R, or with a stretch in ArcMAP 


### NEXT TASK : PARALELLIZE

# https://gis.stackexchange.com/questions/213225/processing-vector-to-raster-faster-with-r/213376



system.time(prom_pixels(Kat, "Kattest.tif", 51))  
# takes forever, or 48 secs on a 2m res raster!

