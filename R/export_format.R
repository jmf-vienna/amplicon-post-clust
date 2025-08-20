make_feature_ids <- function(sequences_table, feature_id_prefix = "OTU") {
  # check for duplicate sequences
  assert_unique(sequences_table, sequence)

  feature_ids <-
    sequences_table |>
    mutate(
      # sha1 = sequence |> openssl::sha1(), # handled by vsearch for now
      sha1base36 = sha1 |> Rmpfr::mpfr(base = 16L) |> Rmpfr::formatMpfr(base = 36L, drop0trailing = TRUE),
      new_feature_id = str_c(
        feature_id_prefix, "_",
        str_sub(sha1base36, 1L, 3L), "_",
        str_sub(sha1base36, 4L, 6L), "_",
        str_sub(sha1base36, 7L, 9L)
      ),
      .keep = "used"
    )

  # Assert that the new feature IDs are unique
  assert_unique(feature_ids, new_feature_id)

  feature_ids
}

format_counts <- function(counts, feature_ids, feature_id_var, sample_id_var, count_var) {
  counts |>
    left_join(feature_ids, by = join_by(feature == sha1)) |>
    select(new_feature_id, sample, count) |>
    arrange(new_feature_id, sample) |>
    dplyr::rename("{feature_id_var}" := new_feature_id, "{sample_id_var}" := sample)
}

format_features <- function(features, feature_id_var) {
  features |>
    mutate(quality_min_eepm = round(quality_min_eepm, 3L)) |>
    distinct(new_feature_id, quality_min_eepm, sequence_length, sequence, sha1, sha1base36) |>
    arrange(new_feature_id) |>
    dplyr::rename("{feature_id_var}" := new_feature_id)
}

format_sample_metrics <- function(sample_metrics, sample_id_var, sample_plural_name) {
  sample_metrics |>
    mutate(
      resolution = sample_plural_name,
      state = "crude",
      .after = "tool"
    ) |>
    arrange(sample) |>
    dplyr::rename("{sample_id_var}" := sample)
}
