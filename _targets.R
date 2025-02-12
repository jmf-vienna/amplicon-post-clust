library(targets)

tar_option_set(
  packages = c("cli", "fs", "purrr", "readr", "rlang"),
  format = "qs",
)

find_one_file <- function(path, glob) {
  file <- dir_ls(path, type = "file", glob = glob)

  if (!is_string(file)) {
    cli_abort("Expected exactly one file matching the pattern {.arg {glob}} in {.path {path}}, but found {?none/}{.file {file}} instead!")
  }

  cli_alert("found {.file {file}}")
  file
}

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
