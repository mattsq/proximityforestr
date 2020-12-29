
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

grow_greedy_distance_tree <- function(seq, mat, mtry,
                                      dist_measures = distance_measures,
                                      verbose = TRUE) {

  if(length(seq) == 1 | var(seq) == 0) return(c(seq))

  parent_gini <- DescTools::Gini(seq, unbiased = FALSE)

  weighted_child_gini <- numeric(mtry)
  gain <- numeric(mtry)
  examplars_1 <- numeric(mtry)
  examplars_2 <- numeric(mtry)
  measure_used <- character(mtry)

  for (k in seq_along(1:mtry)) {
    used_distance_measure <- sample_with_one(dist_measures, 1)
    dist_mat <- as.matrix(dist(mat, method = used_distance_measure))
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
    measure_used[k] <- used_distance_measure
  }

  best_examplar_1 <- examplars_1[which.max(gain)]
  best_examplar_2 <- examplars_2[which.max(gain)]
  best_dist_mat <- as.matrix(dist(mat, method = measure_used[which.max(gain)]))

  examplar_1_dist <- best_dist_mat[best_examplar_1,]
  examplar_2_dist <- best_dist_mat[best_examplar_2,]

  idx <- which(examplar_1_dist < examplar_2_dist)

  seq_l <- seq[idx]
  mat_l <- mat[idx,]
  seq_r <- seq[-idx]
  mat_r <- mat[-idx,]

  if (verbose) {
    cat("Examplar 1:", mat[best_examplar_1,], "Examplar 2:", mat[best_examplar_2,],"\n")
    cat("Measure used:", measure_used[which.max(gain)], "\n")
    cat("Class split:", seq_l, "<->", seq_r, "\n")
  #  cat("Value Split:", num_l, "<->", num_r, "\n")
    cat("----------------------------------------\n")
  }
  if (is.null(seq_l)) {
    grow_greedy_distance_tree(seq_r, mat_l, mtry)
  } else if (is.null(seq_r)) {
    grow_greedy_distance_tree(seq_l, mat_r, mtry)
  } else {

    list(grow_greedy_distance_tree(seq_l, mat_l, mtry), grow_greedy_distance_tree(seq_r, mat_r, mtry))
  }
}

# Theory:
#  Wrap the recursive function in a higher level function.
#  Initialise a parent node list object
#  The recursive function takes a node object along with arguments.
#  During the call of the function, use `<<-` to assign the current state of the node
#  To a higher level store that then knows what the tree looks like
#  Something like:
#   Parent-ID ; L-child-ID; R-child-ID; dist_used; parent-idx; L-idx;  R-idx ; L-examplar-idx; R-exemplar-idx;
#   "dfgdhhfh"; "4tey8725"; "et982896"; "maximum"; c(1,5,6,4); c(1,5); c(6,4); 1             ; 6
#

parent_id <- generate_random_id(20)
recusss <- function(parent_id, seq) {
  if (length(seq) == 1) {
    return(dplyr::tibble(
      parent_id = parent_id,
      l_child_id = NA_character_,
      r_child_id = NA_character_,
      seq = list(seq),
      left = NA,
      right = NA
        ))
  }
  random_splitpoint <- sample(1:length(seq), 1)
  l_idx <- 1:random_splitpoint
  r_idx <- (random_splitpoint+1):length(seq)
  l_child_id <- generate_random_id(20)
  r_child_id <- generate_random_id(20)
  out_table <- dplyr::tibble(
    parent_id = parent_id,
    l_child_id = l_child_id,
    r_child_id = r_child_id,
    seq = list(seq),
    left = list(NA),
    right = list(NA)
  )

  out_table$left[[1]] <- recusss(l_child_id, seq[l_idx])
  out_table$right[[1]] <- recusss(r_child_id, seq[r_idx])

  return(
    out_table
  )
}

out <- recusss(parent_id, seq)
debug(recusss)

q <- bind_rows(out, out$left, out$right)

reducer <- function(df) {
  bind_rows(df, df$left, df$right)
}

while (min(map_dbl(q$seq, length)) > 1) {
  q <- bind_rows(q, q$left, q$right)
}



