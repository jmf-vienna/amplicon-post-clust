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
  tar_target(count_sum_min, config |> pluck("filter", "count sum", .default = 2L)),

  ## reads ----
  tar_target(reads_file, find_one_file(input_path, str_c(path_glob, "reads.tsv")), format = "file"),
  tar_target(reads_table, read_tsv(reads_file)),
  tar_target(feature_quality, reads_table |> tidy_expected_errors() |> summarise_expected_errors()),

  ## sequences ----
  tar_target(sequences_file, find_one_file(input_path, str_c(path_glob, "sequences.tsv")), format = "file"),
  tar_target(sequences_table, sequences_file |> read_tsv() |> tidy_sequences()),
  tar_target(feature_ids, make_feature_ids(sequences_table)),

  # metrics from previous steps ----
  tar_target(prior_metrics_file, find_one_file(input_path, str_c(path_glob, "metrics.tsv")), format = "file"),
  tar_target(prior_metrics, read_tsv(prior_metrics_file)),

  # counts ----

  ## solitary ----
  tar_target(solitary_stats_file, find_one_file(input_path, str_c(path_glob, "solitary*_stats.tsv")), format = "file"),
  tar_target(solitary_stats, solitary_stats_file |> read_tsv() |> tidy_counts_table()),
  tar_target(solitary_raw_counts, tidy_counts(solitary_stats, feature_ids)),

  ## pooled ----
  tar_target(pooled_cluster_members_file, find_one_file(input_path, str_c(path_glob, "pooled*_cluster_members.tsv")), format = "file"),
  tar_target(pooled_cluster_members, pooled_cluster_members_file |> read_tsv() |> tidy_cluster_members()),
  tar_target(pooled_raw_counts, make_pooled_counts(pooled_cluster_members, reads_table, feature_ids)),

  ## filters ----
  ### by eepm ----
  tar_target(quality_features, get_quality_features(feature_quality, eepm_max)),
  tar_target(solitary_quality_counts, filter_counts(solitary_raw_counts, quality_features)),
  tar_target(pooled_quality_counts, filter_counts(pooled_raw_counts, quality_features)),
  ### by sum(count) ----
  tar_target(solitary_abundant_features, get_abundant_features(solitary_raw_counts, count_sum_min)),
  tar_target(solitary_final_counts, filter_counts(solitary_quality_counts, solitary_abundant_features)),
  tar_target(pooled_abundant_features, get_abundant_features(pooled_raw_counts, count_sum_min)),
  tar_target(pooled_final_counts, filter_counts(pooled_quality_counts, pooled_abundant_features)),
  ### final feature list ----
  tar_target(final_features_id, union(pull(solitary_final_counts, feature), pull(pooled_final_counts, feature))),
  tar_target(final_features, make_final_features(sequences_table, feature_ids, feature_quality, final_features_id)),

  # histograms ----
  tar_target(count_histogram, make_count_histogram(list(
    solitary_raw = .make_count_histogram(solitary_raw_counts),
    solitary_quality = .make_count_histogram(solitary_quality_counts),
    solitary_final = .make_count_histogram(solitary_final_counts),
    pooled_raw = .make_count_histogram(pooled_raw_counts),
    pooled_quality = .make_count_histogram(pooled_quality_counts),
    pooled_final = .make_count_histogram(pooled_final_counts)
  ))),
  tar_target(seqlen_histogram, make_seqlen_histogram(sequences_table)),

  # metrics ----
  tar_target(solitary_sample_metrics, make_sample_metrics(
    prior_metrics, str_replace_all(solitary_output_prefix, fixed("_"), " "),
    solitary_raw_counts, solitary_quality_counts, solitary_final_counts
  )),
  tar_target(pooled_sample_metrics, make_sample_metrics(
    prior_metrics, str_replace_all(pooled_output_prefix, fixed("_"), " "),
    pooled_raw_counts, pooled_quality_counts, pooled_final_counts
  )),

  # export ----
  tar_target(feature_id_var, config |> pluck("annotation", "feature id", "variable name", .default = "Feature_ID")),
  tar_target(feature_plural_name, feature_id_var |> str_remove("_?ID$") |> str_c("s")),
  tar_target(sample_id_var, config |> pluck("annotation", "sample id", "variable name", .default = "Sample_ID")),
  tar_target(sample_plural_name, sample_id_var |> str_extract("[a-z]+") |> str_replace("y$", "ies")),
  tar_target(solitary_output_prefix, solitary_stats_file |> str_remove("_stats[.]tsv$") |> str_extract("[A-Za-z0-9_]+$")),
  tar_target(pooled_output_prefix, pooled_cluster_members_file |> str_remove("_cluster_members[.]tsv$") |> str_extract("[A-Za-z0-9_]+$")),
  tar_target(generic_output_prefix, solitary_output_prefix |> str_extract("[A-Za-z0-9]+$")),
  tar_target(
    solitary_counts_file,
    solitary_final_counts |>
      trim_counts(feature_id_var, sample_id_var) |>
      write_tsv(path(output_path, str_c(solitary_output_prefix, "_counts"), ext = "tsv")),
    format = "file"
  ),
  tar_target(
    pooled_counts_file,
    pooled_final_counts |>
      trim_counts(feature_id_var, sample_id_var) |>
      write_tsv(path(output_path, str_c(pooled_output_prefix, "_counts"), ext = "tsv")),
    format = "file"
  ),
  tar_target(
    solitary_metrics_file,
    solitary_sample_metrics |>
      trim_sample_metrics(sample_id_var, sample_plural_name) |>
      write_tsv(path(output_path, str_c(solitary_output_prefix, "_", sample_plural_name), ext = "tsv")),
    format = "file"
  ),
  tar_target(
    pooled_metrics_file,
    pooled_sample_metrics |>
      trim_sample_metrics(sample_id_var, sample_plural_name) |>
      write_tsv(path(output_path, str_c(pooled_output_prefix, "_", sample_plural_name), ext = "tsv")),
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
    count_histogram_file,
    count_histogram |>
      write_tsv(path(output_path, str_c(generic_output_prefix, "_", feature_plural_name, "_count_histogram"), ext = "tsv")),
    format = "file"
  ),
  tar_target(
    seqlen_histogram_file,
    seqlen_histogram |>
      write_tsv(path(output_path, str_c(generic_output_prefix, "_", feature_plural_name, "_sequence_length_histogram"), ext = "tsv")),
    format = "file"
  )
)
