tidy_counts_table <- function(counts_table) {
  counts_raw <-
    counts_table |>
    dplyr::select(feature, sample, count) |>
    mutate(count = as.integer(count)) |>
    arrange(feature, sample)

  observations <-
    counts_raw |>
    count(feature, sample) |>
    dplyr::filter(n != 1L)

  if (nrow(observations) > 0L) {
    cli_abort(
      "Found duplicate observations in the counts matrix. Please make sure each combination of sample and feature is unique!"
    )
  }

  counts_raw
}

tidy_counts <- function(counts_raw, features) {
  counts_raw |>
    left_join(features |> select(feature, new_feature_id), by = join_by(feature))
}

trim_counts <- function(counts, feature_id_var, sample_id_var, count_var) {
  counts |>
    select(new_feature_id, sample, count) |>
    arrange(new_feature_id, sample) |>
    dplyr::rename("{feature_id_var}" := new_feature_id, "{sample_id_var}" := sample)
}

tidy_features <- function(features_sequences, counts_raw, feature_id_prefix = "OTU") {
  loadNamespace("Biostrings")

  features <-
    tibble(
      feature = features_sequences |> names() |> str_remove(";.+"),
      Sequence_length = BiocGenerics::width(features_sequences),
      Sequence = features_sequences |> as.character(),
      sha1 = Sequence |> openssl::sha1(),
      sha1base36 = sha1 |> Rmpfr::mpfr(base = 16) %>% Rmpfr::formatMpfr(base = 36, drop0trailing = TRUE),
      new_feature_id = str_c(
        feature_id_prefix, "_",
        str_sub(sha1base36, 1, 3), "_",
        str_sub(sha1base36, 4, 6), "_",
        str_sub(sha1base36, 7, 9)
      )
    )

  # Assert that the new feature IDs are unique
  features |> pull(new_feature_id) |> anyDuplicated() |> identical(0L) |> stopifnot()

  features
}

trim_features <- function(features, feature_id_var) {
  features |>
    distinct(new_feature_id, Sequence_length, Sequence, sha1, sha1base36) |>
    arrange(new_feature_id) |>
    dplyr::rename("{feature_id_var}" := new_feature_id)
}

tidy_sample_metrics <- function(sample_metrics_raw, counts) {
  base_data <-
    sample_metrics_raw |>
    select(!c(phase, count)) |>
    distinct()

  final <-
    counts |>
    group_by(sample) |>
    summarise(count = sum(count)) |>
    add_column(phase = "final") |>
    left_join(base_data, by = "sample")

  sample_metrics_raw |>
    bind_rows(final)
}

trim_sample_metrics <- function(sample_metrics, sample_id_var, sample_plural_name) {
  sample_metrics |>
    mutate(
      resolution = sample_plural_name,
      state = "crude",
      .after = "tool"
    ) |>
    arrange(sample) |>
    dplyr::rename("{sample_id_var}" := sample)
}
