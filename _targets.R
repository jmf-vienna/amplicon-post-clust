library(targets)

jmf::quiet()
options(warn = 2L)
tar_option_set(
  packages = c("cli", "dplyr", "fs", "purrr", "readr", "rlang", "stringr", "tibble", "tidyr", "vctrs"),
  format = "qs"
)

tar_config_get("script") |>
  fs::path_dir() |>
  fs::path("R") |>
  tar_source()

list(
  # config ----
  tar_target(config_file, Sys.getenv("R_CONFIG_FILE", "config.yaml"), format = "file"),
  tar_target(config, config::get(config = Sys.getenv("TAR_PROJECT", "default"), file = config_file)),
  tar_target(sample_id_var, config |> pluck("annotation", "sample id", "variable name", .default = "Sample_ID")),
  tar_target(feature_id_var, config |> pluck("annotation", "feature id", "variable name", .default = "Feature_ID")),
  tar_target(count_var, config |> pluck("annotation", "feature id", "variable name", .default = "Count")),

  ## paths ----
  tar_target(input_path, config |> pluck("path", "clusters", .default = "data")),
  tar_target(output_path, config |> pluck("path", "data", .default = "data")),

  # counts ----
  tar_target(counts_matrix_file, find_one_file(input_path, "*.tsv"), format = "file"),
  tar_target(counts_matrix, read_tsv(counts_matrix_file)),
  tar_target(counts, counts_matrix |> tidy_counts_matrix(sample_id_var, feature_id_var, count_var)),
  tar_target(
    counts_file,
    counts |>
      trim_counts(count_var) |>
      write_tsv(path(output_path, "counts", ext = "tsv"))
  )
)
