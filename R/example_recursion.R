
grow_greedy_tree <- function(seq, num, mtry) {
  # this stops the growth of the tree at a pure leaf
  if(length(seq) == 1 | var(seq) == 0) return(c(seq))
  # calculate the parent imbalance
  parent_gini <- DescTools::Gini(seq, unbiased = FALSE)
  # initalize some vectors for the loop (for speed)
  weighted_child_gini <- numeric(mtry)
  gain <- numeric(mtry)
  split_points <- numeric(mtry)
  # iterate mtry times through random split points
  # and calculate the Gini gain (parent gini - child gini, weighted by length of children)
  for (k in 1:mtry) {
    split_point <- purrr::rdunif(n = 1, a = min(num) + .0001, b = max(num) + .0001)
    idx <- which(num <= split_point)
    seq_l <- seq[idx]
    seq_r <- seq[-idx]
    weighted_child_gini[k] <- DescTools::Gini(seq_l, unbiased = FALSE) * length(seq_l) + DescTools::Gini(seq_l, unbiased = FALSE) * length(seq_l)
    gain[k] <- parent_gini - weighted_child_gini[k]
    split_points[k] <- split_point
  }
  # pick the best split point with the maximum gini gain
  best_split <- split_points[which.max(gain)]
  # pick the index that splits using that point
  idx <- which(num <= best_split)
  # split into two trees
  seq_l <- seq[idx]
  num_l <- num[idx]
  seq_r <- seq[-idx]
  num_r <- num[-idx]
  # some verbose printing to help me keep an eye on what's going on
  cat(seq_l, "<->", seq_r, "\n")
  cat(num_l, "<->", num_r, "\n")
  # sometimes one of the children is empty - this stops it trying to grow an empty branch
  if (is.null(seq_l)) {
    grow_greedy_tree(seq_r, num_r, mtry)
  } else if (is.null(seq_r)) {
    grow_greedy_tree(seq_l, num_l, mtry)
  } else {
    # otherwise, keep on growing branches using the splits
    list(grow_greedy_tree(seq_l, num_l, mtry), grow_greedy_tree(seq_r, num_r, mtry))
  }
}

seq <- rbinom(1000, 1, prob = .5) + 1
undebug(grow_greedy_tree)
l <- grow_greedy_tree(seq = seq, num = rnorm(10), mtry = 50)
unlist(l)

sample_with_one <- function(x, size, ...) {
  if (length(x) == 1) return(rep(x, size))
  else sample(x, size, ...)
}



grow_greedy_distance_tree <- function(seq, mat, mtry) {

  if(length(seq) == 1 | var(seq) == 0) return(c(seq))

  parent_gini <- DescTools::Gini(seq, unbiased = FALSE)

  weighted_child_gini <- numeric(mtry)
  gain <- numeric(mtry)
  examplars_1 <- numeric(mtry)
  examplars_2 <- numeric(mtry)
  dist_mat <- as.matrix(dist(mat))

  for (k in seq_along(1:mtry)) {
    exemplar_1_idx <- sample_with_one(which(seq == 1),1)
    exemplar_2_idx <- sample_with_one(which(seq == 2),1)
    examplar_1_dist <- dist_mat[exemplar_1_idx,]
    examplar_2_dist <- dist_mat[exemplar_2_idx,]
    idx <- which(examplar_1_dist < examplar_2_dist)
    seq_l <- seq[idx]
    seq_r <- seq[-idx]
    weighted_child_gini[k] <- (DescTools::Gini(seq_l, unbiased = FALSE) * length(seq_l) +
      DescTools::Gini(seq_r, unbiased = FALSE) * length(seq_r))/length(seq)
    gain[k] <- parent_gini - weighted_child_gini[k]
    examplars_1[k] <- exemplar_1_idx
    examplars_2[k] <- exemplar_2_idx
  }

  best_examplar_1 <- examplars_1[which.max(gain)]
  best_examplar_2 <- examplars_2[which.max(gain)]
  examplar_1_dist <- dist_mat[best_examplar_1,]
  examplar_2_dist <- dist_mat[best_examplar_2,]

  idx <- which(examplar_1_dist < examplar_2_dist)

  seq_l <- seq[idx]
  mat_l <- mat[idx,]
  seq_r <- seq[-idx]
  mat_r <- mat[-idx,]

#  cat("Examplar 1:", num[best_examplar_1], "Examplar 2:", num[best_examplar_2],"\n")
#  cat("Class split:", seq_l, "<->", seq_r, "\n")
#  cat("Value Split:", num_l, "<->", num_r, "\n")
#  cat("----------------------------------------\n")

  if (is.null(seq_l)) {
    grow_greedy_distance_tree(seq_r, mat_l, mtry)
  } else if (is.null(seq_r)) {
    grow_greedy_distance_tree(seq_l, mat_r, mtry)
  } else {

    list(grow_greedy_distance_tree(seq_l, mat_l, mtry), grow_greedy_distance_tree(seq_r, mat_r, mtry))
  }
}

n <- 100

mat <- matrix(data = rnorm(n*2), nrow = n, ncol = 2)
seq <- rbinom(n, 1, prob = .5) + 1
#num <- dplyr::if_else(seq == 1, rnorm(n, 2), rnorm(n,0))
debug(grow_greedy_distance_tree)
l <- grow_greedy_distance_tree(seq = seq, mat = mat, mtry = 3)
unlist(l)
