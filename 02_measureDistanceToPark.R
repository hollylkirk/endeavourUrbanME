# measure distance to the edge of RP or WP for each survey location
rm(list = ls())

library(here)
library(rgdal)
library(sp)
library(rgeos)
library(geosphere)


#load polygon shape files
# data is here
dsn <- "../endeavourUrbanME/data"

# royal park shape file
RP_poly <- readOGR(dsn = dsn,
                   layer = "RoyalPark_Outline",
                   verbose = TRUE)
# convert to lat-long projection to match the coordinates
RP_poly <- spTransform(RP_poly, CRS("+proj=longlat +datum=WGS84"))

# westgate park shape file
WGP_poly <- readOGR(dsn = dsn,
                    layer = "WestgatePark_Outline",
                    verbose = TRUE)
# convert to lat-long projection to match coordinates
WGP_poly <- spTransform(WGP_poly, CRS("+proj=longlat +datum=WGS84"))




# load coordinate lists for sample locations in each park
# Royal park sample points
RP_points <- read.csv(here("data", "RP_SamplePoints.csv"))

#extract coordinates, ID and create a spatial points data frame, make sure to state CRS
pts <- cbind(RP_points$long, RP_points$lat)
df <- data.frame(RP_points$ID)
RP_pts <- SpatialPointsDataFrame(pts, df, 
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
#NB Proj4string tells R what format the data is ALREADY in, it doesn't transform the data. 
# to coerce the data into a new projection you have to use SP transform function.

#check that points and polygons in same place
# plot(RP_pts)
# plot(RP_poly, add = TRUE)

# use the dist2line function to calculate great circle distance of each sample survey location from 
# the edge of royal park
d <- dist2Line(RP_pts, RP_poly)
d <- cbind(d, pt_id = RP_pts$RP_points.ID)

#plot to check the 
plot(RP_pts, pch = 21, cex = d[, 1] / 500)
plot(RP_poly, add = TRUE)
# plot(RP_pts, add = TRUE, pch = 21, cex = d[, 1] / 100)

#export the distances and point IDs to a CSV file
write.csv(d, file = "output/RPdistances.csv", row.names = FALSE)



# do the same for the WGP points
# westgate park points
WGP_points <- read.csv(here("data", "WGP_SamplePoints.csv"))
pts <- cbind(WGP_points$long, WGP_points$lat)
df <- data.frame(WGP_points$ID)
WGP_pts <- SpatialPointsDataFrame(pts, df, 
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))


# calculate distances
d2 <- dist2Line(WGP_pts, WGP_poly)
d2 <- cbind(d2, pt_id = WGP_pts$WGP_points.ID)

#plot to check things worked
plot(WGP_pts, pch = 21, cex = d2[, 1] / 500)
plot(WGP_poly, add = TRUE)
# plot(RP_pts, add = TRUE, pch = 21, cex = d[, 1] / 100)

#export the distances and point IDs to a CSV file
write.csv(d2, file = "output/WGPdistances.csv", row.names = FALSE)






