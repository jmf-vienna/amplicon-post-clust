find_one_file <- function(path, glob) {
  res <- dir_ls(path, type = "file", glob = glob)

  if (!is_string(res)) {
    cli_abort("Expected exactly one file matching the pattern {.arg {glob}} in {.path {path}}, but found {?none/}{.file {res}} instead!")
  }

  cli_alert("found {.file {res}}")
  res
}
