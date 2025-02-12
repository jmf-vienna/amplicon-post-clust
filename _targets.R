library(targets)

tar_option_set(
  packages = c("cli", "fs", "purrr", "readr", "rlang", "stringr", "vctrs"),
  format = "qs",
)

tar_config_get("script") |>
  fs::path_dir() |>
  fs::path("R") |>
  tar_source()

list(
  # config ----
  tar_target(config_file, Sys.getenv("R_CONFIG_FILE", "config.yaml"), format = "file"),
  tar_target(config, config::get(config = Sys.getenv("TAR_PROJECT", "default"), file = config_file)),

  ## io ----
  tar_target(data_path, config |> pluck("path", "clusters", .default = "data")),

  # counts ----
  tar_target(counts_matrix_file, find_one_file(data_path, "*.tsv"), format = "file"),
  tar_target(counts_matrix, read_tsv(counts_matrix_file) |> print())
)
