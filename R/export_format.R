trim_counts <- function(counts, feature_id_var, sample_id_var, count_var) {
  counts |>
    select(new_feature_id, sample, count) |>
    arrange(new_feature_id, sample) |>
    dplyr::rename("{feature_id_var}" := new_feature_id, "{sample_id_var}" := sample)
}

trim_features <- function(features, feature_id_var) {
  features |>
    mutate(quality_min_eepm = round(quality_min_eepm, 3L)) |>
    distinct(new_feature_id, quality_min_eepm, sequence_length, sequence, sha1, sha1base36) |>
    arrange(new_feature_id) |>
    dplyr::rename("{feature_id_var}" := new_feature_id)
}

trim_sample_metrics <- function(sample_metrics, tool, sample_id_var, sample_plural_name) {
  sample_metrics |>
    mutate(
      tool = tool,
      resolution = sample_plural_name,
      state = "crude",
      .before = 1L
    ) |>
    arrange(sample) |>
    dplyr::rename("{sample_id_var}" := sample)
}
