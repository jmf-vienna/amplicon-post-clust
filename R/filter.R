get_abundant_features <- function(counts, count_sum_min) {
  counts |>
    group_by(feature) |>
    summarise(count = sum(count), .groups = "drop") |>
    dplyr::filter(count >= count_sum_min) |>
    pull(feature)
}

get_quality_features <- function(feature_quality, eepm_max) {
  feature_quality |>
    dplyr::filter(quality_min_eepm <= eepm_max) |>
    pull(feature)
}

filter_counts <- function(counts, keep_features) {
  res <-
    counts |>
    dplyr::filter(feature %in% keep_features)

  before <-
    counts |>
    pull(feature) |>
    vec_unique_count()
  after <-
    res |>
    pull(feature) |>
    vec_unique_count()
  cli_alert_success("filtered {.val {before}} features down to {.val {after}} features")

  res
}

make_final_features <- function(sequences_table, feature_ids, feature_quality, keep_features) {
  feature_ids |>
    dplyr::filter(sha1 %in% keep_features) |>
    dplyr::left_join(sequences_table, by = join_by(sha1)) |>
    dplyr::left_join(feature_quality, by = join_by(sha1 == feature))
}

make_sample_metrics <- function(raw_counts, quality_counts, final_counts) {
  bind_rows(
    raw_counts |> add_column(phase = "clustering final", .before = 1L),
    quality_counts |> add_column(phase = "expected errors filtered", .before = 1L),
    final_counts |> add_column(phase = "sum(count) filtered", .before = 1L)
  ) |>
    group_by(phase, sample) |>
    summarise(count = sum(count), features = dplyr::n(), .groups = "drop")
}
