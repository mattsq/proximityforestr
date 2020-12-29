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
  return(dplyr::tibble(
    parent_id = NA_character_,
    l_child_id = NA_character_,
    r_child_id = NA_character_,
    dist_used = NA_character_,
    parent_idx = list(NA_real_),
    l_idx = list(NA_real_),
    r_idx = list(NA_real_),
    l_examplar_idx = NA_integer_,
    r_examplar_idx = NA_integer_
  ))
}

generate_random_id <- function(n) {
  return(
    paste(sample(c(1:9,letters,LETTERS), size = n, replace = TRUE), collapse = "")
  )
}


# dplyr::tibble(
#   parent_id = replicate(5, paste(sample(c(letters, 1:9), 10), collapse = ""), TRUE),
#   l_child_id = replicate(5, paste(sample(c(letters, 1:9), 10), collapse = ""), TRUE),
#   r_child_id = replicate(5, paste(sample(c(letters, 1:9), 10), collapse = ""), TRUE),
#   dist_used = sample(distance_measures, 5, TRUE),
#   l_idx = purrr::map(1:5, ~ sample(1:100, 10)),
#   r_idx = purrr::map(1:5, ~ sample(1:100, 10)),
#   l_examplar_idx = sample(1:100, 5),
#   r_examplar_idx = sample(1:100, 5)
# )
