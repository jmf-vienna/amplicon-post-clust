make_count_histogram <- function(counts_list) {
  counts_list |>
    imap(\(x, idx) dplyr::rename(x, "{idx}" := n)) |>
    purrr::reduce(\(x, y) dplyr::full_join(x, y, by = "count")) |>
    arrange(count)
}

.make_count_histogram <- function(counts) {
  counts |>
    group_by(feature) |>
    summarise(count = sum(count), .groups = "drop") |>
    dplyr::count(count)
}

make_seqlen_histogram <- function(sequences_table) {
  sequences_table |>
    dplyr::count(sequence_length) |>
    rename(raw = n)
}
