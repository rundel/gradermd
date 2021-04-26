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

as_status = function(x) {
  factor(x, levels = c("ok", "outdated", "missing", "error", "rendering"))
}


# Based on shinyjs::runjs
runjs = function(code, sessioon) {
  fxn <- "runjs"
  params <- list(code = code)

  fxn <- paste0("shinyjs-", fxn)
  if (inherits(session, "session_proxy")) {
    if ("id" %in% names(params) && !is.null(params[["id"]])) {
      if (!"asis" %in% names(params) || !params[["asis"]]) {
        params[["id"]] <- session$ns(params[["id"]])
      }
    }
  }
  session$sendCustomMessage(type = fxn, message = params)
  invisible(NULL)
}
