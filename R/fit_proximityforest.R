data <- FreezerRegularTrain_TRAIN

fit_proximityforest <- function(data, ...) {
  X_df <- data[,!colnames(data) == "target"]

  X_df_1 <- X_df[data$target == 1,]
  X_df_0 <- X_df[data$target == 2,]
  dist_1 <- as.matrix(dist(X_df_1, method = "euclidean"))
  dist_0 <- as.matrix(dist(X_df_0, method = "euclidean"))
  exemplar_1_idx <- which.min(rowMeans(dist_1))
  examplar_0_idx <- which.min(rowMeans(dist_0))
  examplar_1_ts <- X_df_1[exemplar_1_idx,]
  examplar_0_ts <- X_df_0[exemplar_1_idx,]

}
