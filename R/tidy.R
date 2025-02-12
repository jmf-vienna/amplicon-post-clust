tidy_counts_matrix <- function(counts_matrix) {
  counts <-
    counts_matrix |>
    rename(feature := 1L) |>
    pivot_longer(!feature, names_to = "sample", values_to = "count") |>
    filter(count > 0L) |>
    mutate(
      count = count |> as.integer(),
      orientation = if_else(str_ends(sample, fixed(".R")), "reverse", "forward")
    ) |>
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
    rename("{feature_id_var}" := feature, "{sample_id_var}" := sample, "{count_var}" := count)
}

tidy_features <- function(features_sequences, counts) {
  loadNamespace("Biostrings")

  features <- tibble(
    feature = features_sequences |> names() |> str_remove(";.+"),
    Sequence_length = BiocGenerics::width(features_sequences),
    seq = as.character(features_sequences),
    seq_revcomp = features_sequences |> Biostrings::reverseComplement() |> as.character()
  ) |>
    arrange(feature)

  orientation <- counts |> distinct(feature, orientation)

  stopifnot(identical(features[["feature"]], orientation[["feature"]]))

  features |>
    left_join(orientation, by = "feature") |>
    mutate(Sequence = if_else(orientation == "reverse", seq_revcomp, seq))
}

trim_features <- function(features, feature_id_var) {
  features |>
    select(feature, Sequence_length, Sequence, orientation)
}
