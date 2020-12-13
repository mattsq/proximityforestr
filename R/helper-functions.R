sample_with_one <- function(x, size, ...) {
  if (length(x) == 1) return(rep(x, size))
  else sample(x, size, ...)
}

generate_id <- function(n) {
  paste0(sample(c(letters, 1:9), n, TRUE), collapse = "")
}


distance_measures <- c("euclidean", "maximum", "manhattan", "canberra", "minkowski")

simple_node <- function(level) {
  list(
    level = level,
    id = generate_id(20),
    dist_used = NA,
    L_idx = NA,
    R_idx = NA
  )
}

tabular_node <- function() {
  tibble::tibble(
    level = numeric(1),
    parent_id = character(1),
    child_id = character(1),
    idx = list(),
    dist_used = character(1)
  )
}
