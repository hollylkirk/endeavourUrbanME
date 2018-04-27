
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


# movement ability differs depending on class of landscape
# park can move anywhere,
# housing harder to move, industrial even more difficult
movement_ability <- list(park = Inf,
                         housing = 500,
                         industrial = 100)



# define function to calculate relative probability of moving to a pixel from
# the park. This is the basic movement model definition
p_move <- function (distance, habitat, ...) {
  # if habitat is type 2, movement ability is 500
  # if habitat is something other than 2, movement ability is 100
  mean_movement <- ifelse(habitat == 2,
                          movement_ability$housing,
                          movement_ability$industrial)
  # is habitat is type 0, movement ability is infinite
  mean_movement <- ifelse(habitat == 0,
                          movement_ability$park,
                          mean_movement)
  # define movement model 
  lambda <- 1 / mean_movement
  exp(-lambda * distance)
}




# plot the curves
# x-axis is distance from park
# make a bunch of distances from park
dists <- seq(0, 1000, length.out = 100)
# apply p_move to range of distances, where habitat = 2
Pmove_housing <- vapply(dists, p_move, habitat = 2, FUN.VALUE = 1)
# apply p_move to range of distances, where habitat = 3
Pmove_industrial <- vapply(dists, p_move, habitat = 3, FUN.VALUE = 1)
# plot the two probability curves
plot(Pmove_housing ~ dists, type = "l", ylim = c(0, 1))
lines(Pmove_industrial ~ dists, lty = 2)

# function to make a raster heatmap of relative dispersal probabiltiy
p_move_raster <- function (x, ...) {
  p_move(x[, 1], x[, 2])
}
# create raster heatmap of movement probability
prob_raster <- raster::calc(stack(dist, ras),
                     p_move_raster)



# ----------------------------------

# turn into expected number of individuals
expected_count <- prob * rpois(1, 10)

# put down some random sampling locations
n_sites <- 300
expected <- raster::sampleRandom(expected_count, n_sites, cells = TRUE)
locs <- raster::xyFromCell(expected_count, expected[, "cell"])

# randomly draw observed counts
counts <- rpois(nrow(expected), expected[, 2])

# plot the landscape
plot(expected_count, col = colorRampPalette(c("light green", "seagreen"))(100))
points(locs, cex = log(counts + 1.2), pch = 21, bg = "light blue")

# how to fit the model and estimate the parameters from count data

# extract the data
df <- data.frame(count = counts)
df$distance <- extract(dist, locs)
df$land_cover <- extract(ras, locs)
df$land_cover <- names(movement_ability)[df$land_cover]

# fit models to estimate movement abilities in different habitat types
m_housing <- glm(count ~ distance,
                 family = poisson,
                 data = df[df$land_cover == "housing", ])
beta_housing <- coef(m_housing)[2]
mean_movement_housing <- 1 / -beta_housing

m_industrial <- glm(count ~ distance,
                    family = poisson,
                    data = df[df$land_cover == "industrial", ])
beta_industrial <- coef(m_industrial)[2]
mean_movement_industrial <- 1 / -beta_industrial

# comapre them to the true values
unlist(movement_ability[-1])
c(mean_movement_housing, mean_movement_industrial)




