#### Maps are fun! ####
#### Created by Katherine T. Charton
#### Updated 16 February 2021

## Load some useful libraries
library(NLMR)
library(landscapetools)
library(sp)
library(raster)
library(landscapemetrics)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rasterVis) 
library(plotrix)

## Create some cool random maps
create_random_nlm <- function(ncol = 100, nrow = 100, res = 100, p = 0.1, n = 1) { # 1 replicate
  
  for(i in 1:n) { # loop through each replicate
    if (i == 1) {
      random <- nlm_percolation(ncol = ncol, nrow = nrow, resolution = res, prob = p)
      names(random) <- "layer.1"
    } else if (i > 1) {
      random.new <- nlm_percolation(ncol = ncol, nrow = nrow, resolution = res, prob = p)
      names(random.new) <- paste("layer", i, sep = ".")
      random <- stack(random, random.new)
    }
  }
  return(random) # return raster stack
}

random10 <- create_random_nlm(p = 0.1) # vary proportion of landscape occupied by cover type
random20 <- create_random_nlm(p = 0.2)
random30 <- create_random_nlm(p = 0.3)
random40 <- create_random_nlm(p = 0.4)
random50 <- create_random_nlm(p = 0.5)
random60 <- create_random_nlm(p = 0.6)
random70 <- create_random_nlm(p = 0.7)
random80 <- create_random_nlm(p = 0.8)
random90 <- create_random_nlm(p = 0.9)

plots <- stack(random10[[1]], random20[[1]], random30[[1]], random40[[1]],
               random50[[1]], random60[[1]], random70[[1]], random80[[1]],
               random90[[1]])

lattice.options(layout.heights = list(xlab.axis.padding = list(x = 1), strip = list(x = 0.5)),
                layout.widths = list(ylab.axis.padding = list(x = 1), between = list(x = 7.5)))

levelplot(plots, cuts = 1, col.regions = c("gray80", "black"), colorkey = FALSE,
          scales = list(draw = FALSE), layout = c(3, 3), names.attr = rep("", 9), margin = TRUE)

## Create some cool non-random (fractal) maps

create_random_frac <- function(ncol = 100, nrow = 100, fract_dim = 0.8, res = 100, p = 0.1, n = 1) { # moderate contagion

  for(i in 1:n) {
    if (i == 1) {
      random <- util_binarize(nlm_fbm(ncol = ncol, nrow = nrow, fract_dim = fract_dim, resolution = res), p)
      names(random) <- "layer.1"
    } else if (i > 1) {
      random.new <- util_binarize(nlm_fbm(ncol = ncol, nrow = nrow, fract_dim = fract_dim, resolution = res), p)
      names(random.new) <- paste("layer", i, sep = ".")
      random <- stack(random, random.new)
    }
  }
  return(random)
}

random10_frac0.8 <- create_random_frac(p = 0.1)
random20_frac0.8 <- create_random_frac(p = 0.2)
random30_frac0.8 <- create_random_frac(p = 0.3)
random40_frac0.8 <- create_random_frac(p = 0.4)
random50_frac0.8 <- create_random_frac(p = 0.5)
random60_frac0.8 <- create_random_frac(p = 0.6)
random70_frac0.8 <- create_random_frac(p = 0.7)
random80_frac0.8 <- create_random_frac(p = 0.8)
random90_frac0.8 <- create_random_frac(p = 0.9)

plots_frac0.8 <- stack(random10_frac0.8[[1]], random20_frac0.8[[1]], random30_frac0.8[[1]], random40_frac0.8[[1]],
                       random50_frac0.8[[1]], random60_frac0.8[[1]], random70_frac0.8[[1]], random80_frac0.8[[1]],
                       random90_frac0.8[[1]])

lattice.options(layout.heights = list(xlab.axis.padding = list(x = 1), strip = list(x = 0.5)),
                layout.widths = list(ylab.axis.padding = list(x = 1), between = list(x = 7.5)))

levelplot(plots_frac0.8, cuts = 1, col.regions = c("gray80", "black"), colorkey = FALSE,
          scales = list(draw = FALSE), layout = c(3, 3), names.attr = rep("", 9), margin = TRUE)

