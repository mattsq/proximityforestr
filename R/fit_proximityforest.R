data <- tsforest::FreezerRegularTrain_TRAIN
#
# Remaining questions:
# - How do you store the splits (as a tree of indexes?)
# - need to be able recursively calculate many different distance measures and exemplars
# - how do you apply it recursively
# - how do you split the data on a new data set (store the exemplars and distance measures at each split?)
# - build a proximity forest (not just a tree) and the voting mechanism


fit_proximityforest <- function(data, ...) {
  X_df <- data[,!colnames(data) == "target"]

  X_df_1_idx <- which(data$target == 1)
  X_df_0_idx <- which(data$target == 2)
  distance_measures <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  random_measure <- sample(distance_measures, 1)
  random_ts_1_idx <- sample(X_df_1_idx, 1)
  random_ts_0_idx <- sample(X_df_0_idx, 1)
  distance_matrix <- as.matrix(dist(X_df, method = random_measure))

  X_df_1_dist <- distance_matrix[,random_ts_1_idx]
  X_df_0_dist <- distance_matrix[,random_ts_0_idx]
  combined_distances <- t(rbind(X_df_1_dist, X_df_0_dist))
  split_idx <- which(combined_distances[,1] > combined_distances[,2])

  L <- data[split_idx,]
  R <- data[-split_idx,]
  parent_impurity <- DescTools::Gini(data$target)
  weighted_child_impurity <- (DescTools::Gini(L$target) * nrow(L) +
    DescTools::Gini(R$target) * nrow(R))/nrow(data)
}

create_proxforest_node <- function(indexes,
                                   dist_measure,
                                   class_0_exemplar,
                                   class_1_exemplar) {
  structure(
    list(
      node_data_indexes = indexes,
      dist_measure = dist_measure,
      class_0_exemplar = class_0_exemplar,
      class_1_exemplar = class_1_exemplar,
      left_child_node = NA,
      right_child_node = NA
    ),
    class = "proxforest_node"
  )
}
