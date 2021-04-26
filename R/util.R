system_open = function(path) {
  path = fs::path_real(path)
  stopifnot(file.exists(path))

  if (.Platform['OS.type'] == "windows"){
    shell.exec(paste0("file://", path))
  } else {
    system(paste(Sys.getenv("R_BROWSER"), path))
  }
}

`%||%` = function (x, y) {
  if (is.null(x)) y else x
}


pat_to_regex = function(pattern, is_regex) {
  if (is_regex) pattern else utils::glob2rx(pattern)
}

rs_icon = function(name, h = 16, w = NULL, class = "rs_icon") {
  rsicons::icon(
    name, h, w,
    output = "tag",
    resize = TRUE,
    method = "at_least",
    class = "rs_icon"
  )
}


check_outdated = function(source, output, default = FALSE) {
  purrr::map2_lgl(
    source, output,
    function(x, y) {
      if (is.na(x) || is.na(y))
        default
      else
        file.mtime(y) > file.mtime(x)
    }
  )
}


bs_spinner = function(text) {
  shiny::span(
    class = "spinner",
    shiny::span(
      class = "spinner-border spinner-border-sm", role="status",
      shiny::span(class = "sr-only", text)
    )
  )
}
