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

tidy_expected_errors <- function(expected_errors_table) {
  expected_errors_table |>
    rename(feature = sha1)
}

tidy_cluster_members <- function(cluster_members_table) {
  bind_rows(
    cluster_members_table |>
      mutate(member = seed) |>
      select(-members),
    cluster_members_table |>
      separate_longer_delim(members, " ") |>
      dplyr::filter(!is.na(members)) |>
      rename(member = members)
  )
}

make_solitary_counts <- function(x) {
  x
}

make_pooled_counts <- function(pooled_cluster_members, reads_table) {
  reads_table |>
    dplyr::inner_join(pooled_cluster_members, by = join_by(sha1 == member)) |>
    count(seed, sample) |>
    select(feature = seed, sample, count = n)
}

summarise_expected_errors <- function(expected_errors_table) {
  expected_errors_table |>
    group_by(feature) |>
    summarise(quality_min_eepm = min(expected_errors / length * 1e6L))
}
