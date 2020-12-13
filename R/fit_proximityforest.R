
#
# Remaining questions:
# - How do you store the splits (as a tree of indexes?) - DONE
# - need to be able recursively calculate many different distance measures and exemplars - DONE
# - how do you apply it recursively
# - the tree itself reaches purity (disturbingly) quickly - but I can't immediately
#   see how to grow it in a recursive way. Need to go look at some examples
# - how do you split the data on a new data set (store the exemplars and distance measures at each split?) - DONE
# - build a proximity forest (not just a tree) and the voting mechanism

create_proxforest_node <- function(indexes,
                                   dist_measure,
                                   class_0_exemplar,
                                   class_1_exemplar,
                                   weighted_child_impurity,
                                   L_idx, R_idx) {
  structure(
    list(
      node_data_indexes = indexes,
      dist_measure = dist_measure,
      class_0_exemplar = class_0_exemplar,
      class_1_exemplar = class_1_exemplar,
      weighted_child_impurity = weighted_child_impurity,
      left = list(
        indexes = L_idx,
        child_node = NA,
        leaf = 0
        ),
      right = list(
        indexes = R_idx,
        child_node = NA,
        leaf = 0
      )
    ),
    class = "proxforest_node"
  )
}


calculate_node_split <- function(data, X_0_exemplar_idx, X_1_exemplar_idx, random_measure) {
  X_df <- data[,!colnames(data) == "target"]
  distance_matrix <- as.matrix(dist(X_df, method = random_measure))
  X_df_1_dist <- distance_matrix[,X_0_exemplar_idx]
  X_df_0_dist <- distance_matrix[,X_1_exemplar_idx]
  combined_distances <- t(rbind(X_df_1_dist, X_df_0_dist))
  split_idx <- which(combined_distances[,1] > combined_distances[,2])
  L <- data$target[split_idx]
  R <- data$target[-split_idx]
  parent_impurity <- DescTools::Gini(data$target, unbiased = FALSE)
  L_impurity <- DescTools::Gini(L, unbiased = FALSE)
  R_impurity <- DescTools::Gini(R, unbiased = FALSE)
  weighted_child_impurity <- (L_impurity * length(L) + R_impurity * length(R))/nrow(X_df)
  L_idx <- split_idx
  R_idx <- which(!1:nrow(X_df) %in% split_idx)

  return(
    tibble::tibble(
      L_idx = list(L_idx),
      R_idx = list(R_idx),
      parent_impurity = parent_impurity,
      weighted_child_impurity = weighted_child_impurity
    )
  )
}


select_proximityforest_nodesplit <- function(data, mtry, ...) {
  force(data)
  X_df_1_idx <- which(data$target == 1)
  X_df_0_idx <- which(data$target == 2)
  distance_measures <- c("euclidean", "maximum", "manhattan", "canberra", "minkowski")
  random_measure <- sample(distance_measures, mtry, replace = TRUE)
  random_ts_1_idx <- sample(X_df_1_idx, mtry, replace = TRUE)
  random_ts_0_idx <- sample(X_df_0_idx, mtry, replace = TRUE)

  split_attempts <- purrr::pmap_dfr(list(X_0_exemplar_idx = random_ts_0_idx, X_1_exemplar_idx = random_ts_1_idx, random_measure = random_measure),
                  calculate_node_split,
                  data = data)

  split_attempts$measure <- random_measure
  split_attempts$X_0_exemplar_idx <- random_ts_0_idx
  split_attempts$X_1_exemplar_idx <- random_ts_1_idx
  split_attempts <- dplyr::arrange(split_attempts, weighted_child_impurity)

  create_proxforest_node(indexes = 1:nrow(data),
                         dist_measure = split_attempts$measure[1],
                         class_0_exemplar = split_attempts$X_0_exemplar_idx[1],
                         class_1_exemplar = split_attempts$X_1_exemplar_idx[1],
                         weighted_child_impurity = split_attempts$weighted_child_impurity[1],
                         L_idx = split_attempts$L_idx[[1]], R_idx = split_attempts$R_idx[[1]])

}

fit_proximityforest <- function(data, mtry, node_depth) {
  parent_node <- select_proximityforest_nodesplit(data = data, mtry = mtry)
  if (DescTools::Gini(data[parent_node$left$indexes,]$target) == 0) {
    parent_node$left$leaf <- 1
  }
  if (DescTools::Gini(data[parent_node$right$indexes,]$target) == 0) {
    parent_node$right$leaf <- 1
  }
  left <- select_proximityforest_nodesplit(data = data[parent_node$left$indexes,], mtry = mtry)
  DescTools::Gini(data[left$left$indexes,]$target)
  DescTools::Gini(data[left$right$indexes,]$target)
  right <- select_proximityforest_nodesplit(data = data[parent_node$right$indexes,], mtry = mtry)
  DescTools::Gini(data[right$left$indexes,]$target)
  DescTools::Gini(data[right$right$indexes,]$target)
  right_left <- select_proximityforest_nodesplit(data = data[right$left$indexes,], mtry = mtry)
  DescTools::Gini(data[right_left$left$indexes,]$target)
  DescTools::Gini(data[right_left$right$indexes,]$target)
  right_right <- select_proximityforest_nodesplit(data = data[right$right$indexes,], mtry = mtry)
  DescTools::Gini(data[right_right$left$indexes,]$target)
  DescTools::Gini(data[right_right$right$indexes,]$target)


}


