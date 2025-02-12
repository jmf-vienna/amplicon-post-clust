find_one_file <- function(path, glob) {
  file <- dir_ls(path, type = "file", glob = glob)

  if (!is_string(file)) {
    cli_abort("Expected exactly one file matching the pattern {.arg {glob}} in {.path {path}}, but found {?none/}{.file {file}} instead!")
  }

  cli_alert("found {.file {file}}")
  file
}
