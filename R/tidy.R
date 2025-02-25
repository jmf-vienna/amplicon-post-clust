tidy_counts_matrix <- function(counts_matrix) {
  counts_raw <-
    counts_matrix |>
    dplyr::rename(feature := 1L) |>
    pivot_longer(!feature, names_to = "sample", values_to = "count") |>
    filter(count > 0L) |>
    mutate(
      count = count |> as.integer(),
      orientation = if_else(str_ends(sample, fixed(".R")), "reverse", "forward")
    ) |>
    arrange(feature, sample)

  observations <-
    counts_raw |>
    count(feature, sample) |>
    filter(n != 1L)

  if (nrow(observations) > 0L) {
    cli_abort(
      "Found duplicate observations in the counts matrix. Please make sure each combination of sample and feature is unique!"
    )
  }

  counts_raw
}

tidy_counts <- function(counts_raw, features) {
  counts_raw |>
    left_join(features |> select(feature, orientation, new_feature_id), by = join_by(feature, orientation))
}

trim_counts <- function(counts, feature_id_var, sample_id_var, count_var) {
  counts |>
    select(new_feature_id, sample, count) |>
    arrange(new_feature_id, sample) |>
    dplyr::rename("{feature_id_var}" := new_feature_id, "{sample_id_var}" := sample)
}

tidy_features <- function(features_sequences, counts_raw) {
  loadNamespace("Biostrings")

  features <- tibble(
    feature = features_sequences |> names() |> str_remove(";.+"),
    Sequence_length = BiocGenerics::width(features_sequences),
    seq = features_sequences |> as.character(),
    seq_revcomp = features_sequences |> Biostrings::reverseComplement() |> as.character()
  ) |>
    arrange(feature)

  orientation <- counts_raw |> distinct(feature, orientation)

  orientations_by_feature <- orientation |>
    count(feature) |>
    dplyr::filter(n > 1L)
  if (!vec_is_empty(orientations_by_feature)) {
    cli_abort("Found both orientations for the feature{?s} {.val {orientations_by_feature$feature}}. Please make sure each feature has only one orientation!")
  }

  stopifnot(identical(features[["feature"]], orientation[["feature"]]))

  features |>
    left_join(orientation, by = "feature") |>
    mutate(
      Sequence = if_else(orientation == "reverse", seq_revcomp, seq),
      new_feature_id = Sequence |> openssl::sha1()
    )
}

trim_features <- function(features, feature_id_var) {
  features |>
    distinct(new_feature_id, Sequence_length, Sequence) |>
    arrange(new_feature_id) |>
    dplyr::rename("{feature_id_var}" := new_feature_id)
}

tidy_sample_metrics <- function(sample_metrics_raw, counts) {
  final <-
    counts |>
    group_by(sample) |>
    summarise(count = sum(count)) |>
    add_column(phase = "final")

  sample_metrics_raw |>
    dplyr::rename(sample := 1L) |>
    pivot_longer(!sample, names_to = "phase", values_to = "count") |>
    mutate(phase = phase |> str_to_lower() |> str_replace_all("[^a-z]", " ") |> str_remove("reads?") |> str_squish()) |>
    bind_rows(final) |>
    tibble::add_column(
      state = "crude",
    ) |>
    relocate(phase, sample, count, .after = last_col())
}

trim_sample_metrics <- function(sample_metrics, sample_id_var, tool) {
  sample_metrics |>
    mutate(
      tool = tool,
      resolution = sample_id_var |> str_extract("[a-z]+") |> str_replace("y$", "ies"),
      .before = 1L
    ) |>
    arrange(sample) |>
    dplyr::rename("{sample_id_var}" := sample)
}
