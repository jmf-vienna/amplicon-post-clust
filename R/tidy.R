tidy_counts_matrix <- function(counts_matrix) {
  counts <-
    counts_matrix |>
    rename(feature := 1L) |>
    pivot_longer(!feature, names_to = "sample", values_to = "count") |>
    arrange(feature, sample)

  observations <-
    counts |>
    count(feature, sample) |>
    filter(n != 1L)

  if (nrow(observations) > 0L) {
    cli_abort(
      "Found duplicate observations in the counts matrix. Please make sure each combination of sample and feature is unique!"
    )
  }

  counts
}

trim_counts <- function(counts, feature_id_var, sample_id_var, count_var) {
  counts |>
    filter(count > 0L) |>
    rename("{feature_id_var}" := feature, "{sample_id_var}" := sample, "{count_var}" := count)
}

tidy_features <- function(features_sequences) {
  loadNamespace("Biostrings")

  tibble(
    .feature_id = features_sequences |> names() |> str_remove(";.+"),
    Sequence_length = BiocGenerics::width(features_sequences),
    Sequence = as.character(features_sequences)
  )
}

trim_features <- function(features, feature_id_var) {
  features
}
