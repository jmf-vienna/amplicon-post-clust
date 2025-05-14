filter_features <- function(features, eepm_max) {
  features |>
    dplyr::filter(quality_min_eepm <= eepm_max)
}

filter_counts <- function(counts, filtered_features) {
  counts |>
    dplyr::filter(new_feature_id %in% dplyr::pull(filtered_features, new_feature_id))
}
