assert_unique <- function(data, var) {
  data |>
    pull({{ var }}) |>
    anyDuplicated() |>
    identical(0L) |>
    stopifnot()
}

tidy_sequences <- function(sequences_table) {
  sequences_table |>
    mutate(
      sequence_length = str_length(sequence)
    )
}

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

tidy_counts <- function(counts_raw, feature_ids) {
  counts_raw |>
    left_join(feature_ids |> select(feature = sha1, new_feature_id), by = join_by(feature))
}

tidy_expected_errors <- function(expected_errors_table) {
  expected_errors_table |>
    rename(feature = sha1)
}

summarise_expected_errors <- function(expected_errors_table) {
  expected_errors_table |>
    group_by(feature) |>
    summarise(quality_min_eepm = min(expected_errors / length * 1e6L))
}
