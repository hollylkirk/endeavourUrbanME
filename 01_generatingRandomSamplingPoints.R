rm(list = ls())
# generating random sampling points across landscape
# within 1.5km of the parks. Two parks, Royal Park and Westgate park
# Original CRS = GDA94/MGA zone 55 EPSG:28355

# load relevant packages
library(ggmap)
library(spatstat)
library(rgdal)
library(rgeos)
library(raster)
library(maptools)

# optional 
# devtools::install_github("jcheng5/rasterfaster")
# to find the details of an EPSG code
# CRS("+init=epsg:28355")

# set output folder in the data folder



#function to generate random points within a shape
randSampPoint <- function(distance, nPoints, sampPoly, ...){
  # distance is the minimum distance for 
  # nPoints = number of points, if left blank will generate until samparea"full"
  # sampPoly = shape of the sample area
  
  # use rSSI function (from stat package) to generate the points
  # rSSI converts distance into whatever the "+unit" is for the polygon shape you give it
  p <- rSSI(distance, nPoints, sampPoly)
  # extract the coordinates from this point object
  c <- cbind(p$x, p$y)
  # transform to a spatial points file in the same CRS as sampPoly
  sp <- SpatialPoints(c, crs(sampPoly))
  
  return(sp)
  
}


# define the CRS for the project
# crsDef <- CRS("+proj=utm +zone=55 +south +ellps=GRS80 +units=m +no_defs")
# to find the details of an EPSG code
# CRS("+init=epsg:28355")


#--------------------------------------

# load Royal park buffer shape
# find the shape files in dsn
dsn <- "../endeavourUrbanME/data"
# read shapefile
RP_poly <- readOGR(dsn = dsn,
                   layer = "RoyalPark1500Buff_split",
                   verbose = TRUE)


# Generate 60 random points across RoyalPark shape, at least 200m apart from each other
RP_points <- randSampPoint(200, 60, RP_poly)

# plot to make sure it works
plot(RP_poly)
points(RP_points)

# make into a spatial data frame
df <- data.frame(RP_points@coords[,1], RP_points@coords[,2])
RP_df <- SpatialPointsDataFrame(RP_points, df)

#convert to lat long for projection in google maps
RP_sample <- spTransform(RP_df, CRS("+proj=longlat +datum=WGS84"))
RP_sample <- data.frame(RP_sample@coords[,1], RP_sample@coords[,2])

# export to GPX/CSV/KML file
cnames <- c("long", "lat")
write.csv(RP_sample, "output/RP_SamplePoints.csv", col.names = cnames, row.names = FALSE)




#-------------------------------------

# find the shape files in dsn
dsn <- "../endeavourUrbanME/data"

# load Westgate park buffer shape
# read shapefile
WGP_poly <- readOGR(dsn = dsn,
                   layer = "WestGatePark1500Buff_split",
                   verbose = TRUE)

# Generate 40 random points across RoyalPark shape, at least 200m apart from each other
WGP_points <- randSampPoint(200, 40, WGP_poly)

# plot to make sure it works
plot(WGP_poly)
points(WGP_points)

# make into a spatial data frame
df <- data.frame(WGP_points@coords[,1], WGP_points@coords[,2])
WGP_df <- SpatialPointsDataFrame(WGP_points, df)

#convert to lat long for projection in google maps
WGP_sample <- spTransform(WGP_df, CRS("+proj=longlat +datum=WGS84"))
WGP_sample <- data.frame(WGP_sample@coords[,1], WGP_sample@coords[,2])

# export to GPX/CSV/KML file
cnames <- c("long", "lat")
write.csv(WGP_sample, "output/WGP_SamplePoints.csv", col.names = cnames, row.names = FALSE)








