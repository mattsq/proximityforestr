
grow_greedy_tree <- function(seq) {
  if(length(seq) == 1) return(seq)
  n <- length(seq) - 1
  split_point <- sample(seq[1:n], 1)
  idx <- seq[1]:split_point
  seq_l <- seq[idx]
  seq_r <- seq[-idx]
  cat(seq_l, "-", seq_r, "\n")
  list(grow_greedy_tree(seq_l), grow_greedy_tree(seq_r))
}

l <- grow_greedy_tree(1:10)
