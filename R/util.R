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

rs_icon = function(name, h = NULL, w = NULL, class = "rs_icon") {
  rsicons::icon(
    name, h, w,
    output = "tag",
    resize = TRUE,
    method = "at_least",
    class = "rs_icon"
  )
}
