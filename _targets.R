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

  # paths ----
  tar_target(input_path, config |> pluck("path", "clusters", "data", .default = "data")),
  tar_target(output_path, config |> pluck("path", "data", .default = "data")),
  tar_target(path_glob, config |> pluck("path", "clusters", "filter", .default = "*")),

  # settings ----
  tar_target(eepm_max, config |> pluck("filter", "eepm", .default = Inf)),

  ## reads ----
  tar_target(reads_file, find_one_file(input_path, str_c(path_glob, "reads.tsv")), format = "file"),
  tar_target(reads_table, read_tsv(reads_file)),
  tar_target(feature_quality, reads_table |> tidy_expected_errors() |> summarise_expected_errors()),

  ## sequences ----
  tar_target(sequences_file, find_one_file(input_path, str_c(path_glob, "sequences.tsv")), format = "file"),
  tar_target(sequences_table, sequences_file |> read_tsv() |> tidy_sequences()),
  tar_target(feature_ids, make_feature_ids(sequences_table)),

  # counts ----
  tar_target(solitary_stats_file, find_one_file(input_path, str_c(path_glob, "solitary*_stats.tsv")), format = "file"),
  tar_target(solitary_stats, solitary_stats_file |> read_tsv() |> tidy_counts_table()),
  tar_target(raw_counts, tidy_counts(solitary_stats, feature_ids)),

  # filters
  tar_target(keep_features, filter_features(feature_quality, eepm_max)),
  tar_target(final_counts, filter_counts(raw_counts, keep_features)),
  tar_target(final_features, make_final_features(sequences_table, feature_ids, feature_quality, final_counts |> pull(feature))),

  # metrics ----
  tar_target(sample_metrics, make_sample_metrics(raw_counts, final_counts)),

  # export ----
  tar_target(feature_id_var, config |> pluck("annotation", "feature id", "variable name", .default = "Feature_ID")),
  tar_target(feature_plural_name, feature_id_var |> str_remove("_?ID$") |> str_c("s")),
  tar_target(sample_id_var, config |> pluck("annotation", "sample id", "variable name", .default = "Sample_ID")),
  tar_target(sample_plural_name, sample_id_var |> str_extract("[a-z]+") |> str_replace("y$", "ies")),
  tar_target(solitary_output_prefix, solitary_stats_file |> str_remove("_stats[.]tsv$") |> str_extract("[A-Za-z0-9_]+$")),
  tar_target(generic_output_prefix, solitary_output_prefix |> str_extract("[A-Za-z0-9]+$")),
  tar_target(tool, solitary_output_prefix |> str_replace_all(fixed("_"), " ")),
  tar_target(
    counts_file,
    final_counts |>
      trim_counts(feature_id_var, sample_id_var) |>
      write_tsv(path(output_path, str_c(solitary_output_prefix, "_counts"), ext = "tsv")),
    format = "file"
  ),
  tar_target(
    features_file,
    final_features |>
      trim_features(feature_id_var) |>
      write_tsv(path(output_path, str_c(generic_output_prefix, "_", feature_plural_name), ext = "tsv")),
    format = "file"
  ),
  tar_target(
    metrics_file,
    sample_metrics |>
      trim_sample_metrics(tool, sample_id_var, sample_plural_name) |>
      write_tsv(path(output_path, str_c(solitary_output_prefix, "_", sample_plural_name), ext = "tsv")),
    format = "file"
  )
)
