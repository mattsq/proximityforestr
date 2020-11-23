
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

seq <- rbinom(10, 1, prob = .5) + 1
undebug(grow_greedy_tree)
l <- grow_greedy_tree(seq = seq, num = rnorm(10), mtry = 50)
unlist(l)

