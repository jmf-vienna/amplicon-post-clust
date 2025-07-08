make_count_histogram <- function(solitary_raw_counts, solitary_final_counts, pooled_raw_counts, pooled_final_counts) {
  list(
    solitary_raw = .make_count_histogram(solitary_raw_counts),
    solitary_final = .make_count_histogram(solitary_final_counts),
    pooled_raw = .make_count_histogram(pooled_raw_counts),
    pooled_final = .make_count_histogram(pooled_final_counts)
  ) |>
    imap(\(x, idx) dplyr::rename(x, "{idx}" := n)) |>
    purrr::reduce(\(x, y) dplyr::full_join(x, y, by = "count"))
}

.make_count_histogram <- function(data) {
  data |>
    group_by(feature) |>
    summarise(count = sum(count), .groups = "drop") |>
    dplyr::count(count)
}
