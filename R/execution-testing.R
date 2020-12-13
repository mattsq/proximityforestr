source("R/example_recursion.R")
source("R/helper-functions.R")

### Greedy Regular Tree

seq <- rbinom(1000, 1, prob = .5) + 1
undebug(grow_greedy_tree)
l <- grow_greedy_tree(seq = seq, num = rnorm(10), mtry = 50)
unlist(l)


### Greedy Distance Tree


n <- 250

mat <- tibble(x1 = rnorm(n), x2 = rnorm(n))
seq <- rbinom(n, 1, prob = if_else(mat$x1 > 0 & mat$x2 < 0, .95, .05)) + 1
#num <- dplyr::if_else(seq == 1, rnorm(n, 2), rnorm(n,0))
mat <- as.matrix(mat)
undebug(grow_greedy_distance_tree)
l <- grow_greedy_distance_tree(seq = seq, mat = mat, mtry = 100, verbose = FALSE)
unlist(l)


##

data <- tsforest::FreezerRegularTrain_TRAIN
mtry <- 10
