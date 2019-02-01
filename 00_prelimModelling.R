rm(list = ls())
# Testing out the basic movement model, ready to run on real world raster of CoM
# simulate a movement-based abundance distribution map.
# Thoughts on how to test this hypothesis

set.seed(1)
library (raster)
library(devtools)
# library(ochRe)


# make a fake raster of habitat type (park, housing matrix and industrial matrix)
# three habitats: parks, housing, and industrial
# stipulate number of columns and rows (e.g. 30)
nrow <- ncol <- 30
# make a matrix of this size (populated by 0s)
mat <- matrix(0, nrow, ncol)
# first column is the border of the park
mat[, 1] <- 1
# rest of columns are non-park
mat[ , -1] <- 2  
# make this into a raster object class 1000 by 1000 cells
ras <- raster::raster(mat, 0, 1000, 0, 1000)



# calculate raster of distance from the park
# duplicate the raster from above
mask <- ras
# get rid of cells that are classed as park
mask[mask != 1] <- NA
# use distance function from raster package (in raster units or pixels)
dist <- raster::distance(mask)


# movement ability differs depending on bird species
# park can move anywhere,
movement_ability <- 300



# define function to calculate relative probability of moving to a pixel from
# the park. This is the basic movement model definition
p_move <- function (distance, movement_ability) {
  lambda <- 1 / movement_ability
  exp(-lambda * distance)
}



# plot the curves
# x-axis is distance from park
# make a bunch of distances from park
dists <- seq(0, 1000, length.out = 100)
# apply p_move to range of distances, where habitat = 2
Pmove <- p_move(dists, movement_ability)
# plot the probability curves
plot(Pmove ~ dists, type = "l", ylim = c(0, 1))

# function to make a raster heatmap of relative dispersal probabiltiy
p_move_raster <- function (x, ...) {
  p_move(x, movement_ability)
}
# create raster heatmap of movement probability
prob <- raster::calc(dist,
                     p_move_raster)

plot(prob)

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
pal <- colorRampPalette(ochre_palettes[["olsen_seq"]])
plot(expected_count, col = pal(100))
points(locs, cex = log(counts + 1.2), pch = 21, bg = "light blue")




# how to fit the model and estimate the parameters from count data?

# extract the data
# create dataframe of observations of bird numbers
df <- data.frame(count = counts)
# add column of "distance from park" for each sample location
df$distance <- extract(dist, locs)

# fit models to estimate movement abilities in different habitat types
model_basic <- glm(count ~ distance,
                    family = poisson,
                    data = df)

# get second coefficient (slope of line)
beta <- coef(model_basic)[2]
# convert the coefficient to the mean movement ability (flight distance)
mean_movement <- 1 / -beta



# comapre them to the true values
movement_ability

# print different model outcomes into a vector
c(mean_movement)


# statistically test whether the null hypothesis model (based on rural movement distaance estimates)
# is different to the alternative hypothesis model, based on observed data.

# create a log offset (fixed slope) based on rural literature values
# convert the coefficient to the mean movement ability (flight distance)
# mean_movement <- 1 / -beta
# 1 / mean_movement <- -beta

# set the mean move distance for this species
mean_movement_null <- 200
# calculate the beta (slope on the line) for this species
beta_null <- -1 / mean_movement_null
# calculate the log of the expected relative abundance for each location for this species
null_logExpected <- beta_null * df$distance


# null hypothesis model
# offset term in model because we are using a known coefficent rather than an estimated one
model_null <- glm(count ~ offset(null_logExpected),
                  family = poisson,
                  data = df)


# likelihood ratio test to compare the two different models (null and observed_basic)
# just use ANOVA as this is the same thing when used on GLMs
t <- anova(model_null, model_basic, test = "Chisq")
t

# LLR test compares how well each model fits the data. And also how complex the model is

