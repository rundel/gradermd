system_open = function(dir = getwd()) {
  dir = sub("file://", "", dir)
  dir = fs::path_real(dir)

  if (.Platform['OS.type'] == "windows"){
    shell.exec(paste0("file://", dir))
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}


copy_env = function(from, to, names = ls(from, all.names=TRUE)) {
  mapply(
    assign, names, mget(names, from), list(to),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)

  invisible()
}


`%||%` = function (x, y) {
  if (is.null(x)) y
  else            x
}


pat_to_regex = function(pattern, is_regex) {
  if (is_regex)
    pattern
  else
    utils::glob2rx(pattern)
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
