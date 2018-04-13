
# Testing out the basic movement model, ready to run on real world raster of CoM
# simulate a movement-based abundance distribution map

set.seed(1)
library (raster)


# make a fake raster of habitat type (park, housing matrix and industrial matrix)
# three habitats: parks, housing, and industrial
# stipulate number of columns and rows (e.g. 30)
nrow <- ncol <- 30
# make a matrix of this size (populated by 0s)
mat <- matrix(0, nrow, ncol)
# first column is the border of the park
mat[, 1] <- 1
# second set of columns and rows is urban matrix type housing
mat[seq_len(nrow / 2), -1] <- 2  
# second set of columns and rows is urban matrix type industrial
mat[ceiling(nrow / 2) + seq_len(nrow / 2), -1] <- 3 
# make this into a raster object class 1000 by 1000 cells
ras <- raster::raster(mat, 0, 1000, 0, 1000)


# calculate raster of distance from the park
# duplicate the raster from above
mask <- ras
# get rid of cells that are classed as park
mask[mask != 1] <- NA
# use distance function from raster package (in raster units or pixels)
dist <- raster::distance(mask)





