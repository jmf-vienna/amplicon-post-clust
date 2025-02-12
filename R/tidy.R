tidy_counts_matrix <- function(counts_matrix, sample_id_var, feature_id_var, count_var) {
  counts <-
    counts_matrix |>
    rename("{feature_id_var}" := 1L) |>
    pivot_longer(where(is.numeric), names_to = sample_id_var, values_to = count_var) |>
    arrange(.data[[feature_id_var]], .data[[sample_id_var]])

  observations <-
    counts |>
    count(.data[[feature_id_var]], .data[[sample_id_var]]) |>
    filter(n != 1L)

  if (nrow(observations) > 0L) {
    cli_abort(
      "Found duplicate observations in the counts matrix. Please make sure each combination of {.var {sample_id_var}} and {.var {feature_id_var}} is unique!"
    )
  }

  counts
}

trim_counts <- function(counts, count_var) {
  counts |>
    filter(.data[[count_var]] > 0L)
}
