dashboard_state = R6::R6Class(
  private = list(
    settings = list(
      doc_pat = "*.Rmd",
      doc_regex = FALSE,
      output = "html",
      docker_image = "rocker/rstudio:latest"
    ),
    d = NULL,
    proj_dir = NULL,
    proj_sub_pat= NULL,
    proj_had_settings = NULL,

    sub_path = NULL,
    sub_name = NULL,
    sub_rproj_path = NULL,

    assign_path = NULL,
    assign_name = NULL,

    output_path = NULL,
    output_name = NULL,

    rendering = FALSE
  ),

  public = list(
    initialize = function(path, pattern = NULL, regexp = FALSE) {
      private$proj_dir = fs::path_expand(path)
      private$proj_sub_pat = pat_to_regex(pattern, regexp)

      settings_path = fs::path(private$proj_dir, "gradermd_settings.rds")
      if (fs::file_exists(settings_path)) {
        private$proj_had_settings = TRUE
        do.call(self$set_setting, c(readRDS(settings_path), warn = FALSE))
      }

      self$update_submissions()
      self$update_assignments()
      self$update_output()

      invisible(self)
    },

    set_cell = function(i, col, value) {
      stopifnot(col %in% names(private$d))
      stopifnot(all(i %in% seq_len(nrow(private$d))))

      private$d[[col]][i] = value

      invisible(self)
    },

    set_status = function(i, col, status) {
      stopifnot(col %in% names(private$d))
      stopifnot(all(i %in% seq_len(nrow(private$d))))

      attr(private$d[[col]], "status")[i] = status

      invisible(self)
    },

    render = function(i) {
      stopifnot(TRUE)

      Sys.sleep(2)

      invisible(self)
    },

    start_rendering = function() {
      private$rendering = TRUE
      invisible(self)
    },

    stop_rendering = function() {
      private$rendering = FALSE
      invisible(self)
    },

    is_rendering = function() {
      private$rendering
    },


    had_settings = function() {
      private$proj_had_settings
    },

    print_settings = function() {
      width = purrr::map_int(names(private$settings), nchar) %>%
        max()

      purrr::walk2(
        private$settings, names(private$settings),
        ~ cat(.y, rep(" ", width-nchar(.y)), " : ", .x, "\n", sep="")
      )

      invisible(self)
    },

    save_settings = function(dir = private$proj_dir) {
      saveRDS(private$settings, file = file.path(dir, "gradermd_settings.rds"))

      invisible(self)
    },

    set_setting = function(..., warn = TRUE) {
      l = list(...)

      purrr::walk2(
        names(l), l,
        function(name, value) {
          if (warn && is.null(private$settings[[name]])) {
            warning("Setting `", name, "` does not exist, and likely does nothing.", call. = FALSE)
          }

          private$settings[[name]] = value
        }
      )

      invisible(self)
    },

    get_setting = function(name) {
      s = private$settings[[name]]
      if (is.null(s))
        stop("Setting `", name, "` does not exist.", call. = FALSE)

      s
    },

    get_table = function() {
      private$d
    },

    update_submissions = function() {

      private$sub_path = fs::dir_ls(
        private$proj_dir, recurse = FALSE, type = "directory",
        regexp = self$proj_sub_pat
      )

      private$sub_name = fs::path_file(private$sub_path)

      private$sub_rproj_path = purrr::map(
        private$sub_path,
        fs::dir_ls,
        type = "file",
        regexp = "\\.rproj$",
        ignore.case = TRUE
      ) %>%
        purrr::map_chr(1, .default = NA)

      #browser()

      self$update_table(
        Submission = private$sub_name,
        dir = TRUE,
        proj = !is.na(private$sub_rproj_path)
      )

      invisible(self)
    },

    update_assignments = function() {

      private$assign_path = purrr::map(
        private$sub_path,
        fs::dir_ls,
        type = "file",
        regexp = pat_to_regex(private$settings$doc_pat, private$settings$doc_regex)
      ) %>%
        purrr::map_chr(1, .default = NA) %>%
        unname()

      status = rep("ok", length(private$assign_path)) %>%
        purrr::modify_if(
          is.na(private$assign_path),
          ~ "missing"
        ) %>%
        as_status()

      self$update_table(
        Assignment = structure(
          fs::path_file(private$assign_path),
          status = status
        )
      )

      invisible(self)
    },

    update_output = function() {
      stopifnot(!is.null(private$assign_path))

      private$output_path = fs::path_ext_set(private$assign_path, private$settings$output) %>%
        purrr::modify_if(~ !file.exists(.x) %||% {print(.x);TRUE}, ~ NA_character_)

      private$output_name = fs::path_file(private$output_path)

      status = rep("ok", length(private$output_path)) %>%
        purrr::modify_if(
          is.na(private$output_path),
          ~ "missing"
        ) %>%
        purrr::modify_if(
          check_outdated(private$assign_path, private$output_path),
          ~ "outdated"
        ) %>%
        as_status()

      self$update_table(
        Output = structure(
          private$output_name,
          status = status
        )
      )

      invisible(self)
    },

    update_table = function(...) {
      args = list(...)

      if (is.null(private$d)) {
        private$d = tibble::as_tibble(args)
      } else {
        purrr::walk2(
          args, names(args),
          ~ {private$d[[.y]] = .x}
        )
      }

      invisible(self)
    },

    click = function(cell, col, row) {

      #cat(cell, col, row, "\n")

      if (is.null(cell) || is.na(cell) || cell == "" || cell == FALSE)
        return()

      dir = if (col == "dir") {
        private$sub_path[ row ]
      } else if (col == "proj") {
        private$sub_rproj_path[ row ]
      } else if (col == "Assignment") {
        private$assign_path[ row ]
      } else if (col == "Output") {
        private$output_path[ row ]
      } else {
        return()
      }

      stopifnot(length(dir) <= 1)
      stopifnot(fs::file_exists(dir))

      system_open(dir)
    }



  )
)
