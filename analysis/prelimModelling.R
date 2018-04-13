
#  
# simulate a movement-based abundance distribution map

set.seed(1)
library (raster)


# make a fake raster of habitat type
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

