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
  tar_target(auto_reverse_complement, config |> pluck("auto reverse complement", .default = FALSE)),

  # paths ----
  tar_target(input_path, config |> pluck("path", "clusters", "data", .default = "data")),
  tar_target(metrics_path, config |> pluck("path", "clusters", "metrics", .default = "data")),
  tar_target(output_path, config |> pluck("path", "data", .default = "data")),

  # counts ----
  tar_target(counts_matrix_file, find_one_file(input_path, "*.tsv"), format = "file"),
  tar_target(counts_matrix, read_tsv(counts_matrix_file)),
  tar_target(counts_raw, counts_matrix |> tidy_counts_matrix(auto_reverse_complement)),
  tar_target(counts, counts_raw |> tidy_counts(features)),

  # features ----
  tar_target(features_sequences_file, find_one_file(input_path, "*.fna"), format = "file"),
  tar_target(features_sequences, Biostrings::readDNAStringSet(features_sequences_file)),
  tar_target(features, tidy_features(features_sequences, counts_raw)),

  # samples ----
  tar_target(sample_metrics_file, find_one_file(metrics_path, "*.tsv"), format = "file"),
  tar_target(sample_metrics_raw, read_tsv(sample_metrics_file)),
  tar_target(sample_metrics, tidy_sample_metrics(sample_metrics_raw, counts)),

  # export ----
  tar_target(feature_id_var, config |> pluck("annotation", "feature id", "variable name", .default = "Feature_ID")),
  tar_target(feature_plural_name, feature_id_var |> str_remove("_?ID$") |> str_c("s")),
  tar_target(sample_id_var, config |> pluck("annotation", "sample id", "variable name", .default = "Sample_ID")),
  tar_target(sample_plural_name, sample_id_var |> str_extract("[a-z]+") |> str_replace("y$", "ies")),
  tar_target(tool_name, config |> pluck("annotation", "meta", "clustering tool", .default = "SOME TOOL")),
  tar_target(output_prefix, tool_name |> str_extract("[A-Za-z0-9]+")),
  tar_target(
    counts_file,
    counts |>
      trim_counts(feature_id_var, sample_id_var) |>
      write_tsv(path(output_path, str_c(output_prefix, "_counts"), ext = "tsv")),
    format = "file"
  ),
  tar_target(
    features_file,
    features |>
      trim_features(feature_id_var) |>
      write_tsv(path(output_path, str_c(output_prefix, "_", feature_plural_name), ext = "tsv")),
    format = "file"
  ),
  tar_target(
    metrics_file,
    sample_metrics |>
      trim_sample_metrics(sample_id_var, sample_plural_name, tool_name) |>
      write_tsv(path(output_path, str_c(output_prefix, "_", sample_plural_name), ext = "tsv")),
    format = "file"
  )
)
