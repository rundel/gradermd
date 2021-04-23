menubar = function(proj_dir) {
  shiny::tags$nav(
    class = "navbar navbar-light bg-light justify-content-between",

    shiny::tags$div(
      class = "navbar-brand",
      "gradermd Dashboard"
    ),

    shiny::tags$span(
      class="navbar-text",
      paste0("Project: ", proj_dir)
    ),

    shiny::tags$div(
      class="nav-item dropdown",

      shiny::tags$button(
        `class` = "btn btn-primary dropdown-toggle",
        `type` = "button",
        `id` = "action_dropdown",
        `data-toggle` = "dropdown",
        `aria-haspopup` = "true",
        `aria-expanded` = "false",

        "Actions"
      ),

      shiny::tags$div(
        `class` = "dropdown-menu dropdown-menu-right",
        `aria-labelledby` = "navbarDropdownMenuLink",

        shiny::actionLink(
          "action1", "Add Column",
          class = "dropdown-item"
        ),

        shiny::actionLink(
          "action2", "Edit Column",
          class = "dropdown-item"
        ),

        shiny::actionLink(
          "action2", "Delete Column",
          class = "dropdown-item"
        ),

        shiny::actionLink(
          "action3", "Import Roster",
          class = "dropdown-item"
        ),

        shiny::actionLink(
          "menu_settings", "Settings",
          class = "dropdown-item"
        ),
      )
    )


  )
}

save_state = function(state) {

  save = new.env()
  settings = c("setting_doc_pat",
               "setting_doc_regex",
               "setting_output",
               "setting_roster_path",
               "setting_roster_column")

  copy_env(state, save, names = settings)

  saveRDS(save, file.path(state$proj_dir, "gradermd_settings.rds"))

  rm(save)
}

settings_dialog_ui = function(input, output, session, state) {

  observeEvent(
    input$settings_cancel,
    shiny::removeModal()
  )

  observeEvent(
    input$settings_save,
    {
      state$setting_doc_pat       = input$setting_doc_pat
      state$setting_doc_regex     = input$setting_doc_regex
      state$setting_output        = input$setting_output
      state$setting_roster_path   = input$setting_roster_path
      state$setting_roster_column = input$setting_roster_column

      save_state(state)

      update_submissions(state)
      update_assignments(state)
      update_output(state)

      reactable::updateReactable(
        "table", data = state$d
      )

      shiny::removeModal()
    }
  )


  observeEvent(
    input$setting_roster_file,
    {
      shiny::req(input$setting_roster_file)

      d = readr::read_csv(input$setting_roster_file$datapath)
      output$setting_roster = shiny::reactive({
        d
      })

      output$setting_roster_conditional = shiny::renderUI({
        shiny::selectInput(
          "setting_roster_join_column", "Submission names column:",
          choices = names(d)
        )
      })
    }
  )

  shiny::modalDialog(
    title = "Project Settings",

    # Help javascipt for closing the modal dialog
    shiny::tags$script(shiny::HTML(
      '$(document).keyup(function(e) {
        if (document.activeElement.id == "shiny-modal") {
          if (e.key == "Enter") {
            $("#settings_save").click();
          } else if (e.key == "Escape") {
            $("#settings_cancel").click();
          }
        }
      });'
    )),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::textInput("setting_doc_pat", label = "Document pattern:", value = state$setting_doc_pat),
        shiny::checkboxInput("setting_doc_regex", label = "Use regex", value = state$setting_doc_regex)
      )
    ),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::selectInput(
          "setting_output", label = "Output type:",
          choices = c("html", "pdf"),
          selected = state$setting_output
        ),
      )
    ),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::fileInput("setting_roster_file", "Roster file:", accept = ".csv"),
        shiny::uiOutput(
          "setting_roster_conditional"
        )
      )
    ),

    easyClose = FALSE,
    size = "m",
    footer = list(
      shiny::actionButton("settings_cancel", "Cancel"),
      shiny::actionButton("settings_save", "Save", class = "btn-success")
    )
  )
}


set_defaults = function(state) {
  state$setting_doc_pat   = state$setting_doc_pat   %||% "*.Rmd"
  state$setting_doc_regex = state$setting_doc_regex %||% FALSE
  state$setting_output    = state$setting_output    %||% "html"
}

update_submissions = function(state) {

  path = fs::dir_ls(state$proj_dir, recurse = FALSE, type = "directory", regexp = state$proj_sub_pat)

  state$sub_path = path
  state$sub_name = fs::path_file(path)

  state$proj_path = purrr::map(
    path,
    fs::dir_ls,
    type = "file",
    regexp = "\\.rproj$",
    ignore.case = TRUE
  ) %>%
    purrr::map_chr(1, .default = NA)

  update_table(
    state,
    Submission = state$sub_name,
    dir = TRUE,
    proj = !is.na(state$proj_path)
  )
}

update_assignments = function(state) {
  assignment = purrr::map(
    state$sub_path,
    fs::dir_ls,
    type = "file",
    regexp = pat_to_regex(state$setting_doc_pat, state$setting_doc_regex)
  ) %>%
    purrr::map_chr(
      ~ {if(length(.x) == 1) .x else NA}
    )

  names(assignment) = NULL

  state$assign_path = assignment
  state$assign_name = fs::path_file(assignment)

  update_table(state, Assignment = state$assign_name)
}

update_output = function(state) {
  #browser()
  state$output_path = fs::path_ext_set(state$assign_path, state$setting_output)
  state$output_name = purrr::map_chr(
    state$output_path,
    ~ {if (fs::file_exists(.x)) fs::path_file(.x) else NA}
  )

  update_table(state, Output = state$output_name)
}


update_table = function(state, ...) {
  args = list(...)

  if (is.null(state$d)) {
    state$d = tibble::as_tibble(args)
  } else {
    purrr::walk2(
      args, names(args),
      ~ {state$d[[.y]] = .x}
    )
  }
}


init_proj = function(input, output, session, state) {
  prev_state_rds = fs::path(state$proj_dir, "gradermd_settings.rds")

  if (fs::file_exists(prev_state_rds)) {
    copy_env(readRDS(prev_state_rds), state)
  }

  set_defaults(state)

  update_submissions(state)
  update_assignments(state)
  update_output(state)
}



#' @export
dashboard = function(dir = "~/Desktop/StatProg-s1-2020/Marking/hw1/repos/", pattern = NULL, regexp = FALSE) {

  dir = fs::path_expand(dir)

  if (!regexp & !is.null(pattern))
    pattern = glob2rx(pattern)


  app = shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::includeCSS(system.file("www/gradermd.css", package="gradermd")),
      menubar(dir),
      shiny::tags$div(
        style = "padding-left: 1%; padding-right: 1%; padding-top: 0.5em;",
        reactable::reactableOutput("table"),
        shiny::tags$br()
      ),

      theme = bslib::bs_theme(version = 4)
    ),
    server = function(input, output, session) {
      test_test = 1

      state = new.env()
      state$proj_dir = dir
      state$proj_sub_pat = pattern

      init_proj(input, output, session, state)

      shiny::showModal(
        settings_dialog_ui(input, output, session, state)
      )

      observeEvent(
        input$menu_settings,
        shiny::showModal(
          settings_dialog_ui(input, output, session, state)
        )
      )

      # Handle clicks on the submissions column
      observeEvent(
        input$submission_click,
        {
          cell = input$submission_click$cell
          col  = input$submission_click$col
          row  = input$submission_click$row

          if (is.null(cell) || is.na(cell))
            return()

          if (col == "Submission") {
            dir = state$sub_path[ row ]
          } else if (col == "Assignment") {
            dir = state$assign_path[ row ]
          } else if (col == "Output") {
            dir = state$output_path[ row ]
          } else {
            return()
          }

          stopifnot(length(dir) <= 1)
          stopifnot(fs::file_exists(dir))

          system_open(dir)
        }
      )

      output$table = reactable::renderReactable({
        reactable::reactable(
          state$d,
          defaultPageSize = 100,
          fullWidth = FALSE,
          searchable = TRUE,
          striped = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          theme = reactable::reactableTheme(
            borderColor = "#dfe2e5",
            stripedColor = "#f6f8fa",
            highlightColor = "#f0f5f9",
            cellPadding = "8px 12px",
            style = list(fontFamily = "Fira Code, monospace, monospace"),
            searchInputStyle = list(width = "100%")
          ),
          defaultColDef = reactable::colDef(
            minWidth = 150,
            #style = "user-select: none",
            cell = function(value, row, col) {
              if (is.na(value)) {
                htmltools::div(
                  class = "tag status-missing",
                  "Missing"
                )
              } else if (col == "Assignment") {
                shiny::tags$div(
                  rs_icon("newRMarkdownDoc", 24),
                  value
                )
              } else if (col == "Output") {
                shiny::tags$div(
                  if (state$setting_output == "html") {
                    rs_icon("newHtmlDoc", 24)
                  } else if (state$setting_output == "pdf") {
                    rs_icon("compilePDF", 24)
                  } else if (state$setting_output == "R") {
                    rs_icon("newSourceDoc", 24)
                  } else {
                    rs_icon("newTextDoc", 24)
                  },
                  value
                )
              } else if (col == "dir") {
                shiny::tags$div(
                  if (value) rs_icon("folder", 24)
                  else       ""
                )
              } else if (col == "proj") {
                shiny::tags$div(
                  if (value) rs_icon("application-x-r-project", 24)
                  else       ""
                )
              } else {
                value
              }
            }
          ),
          columns = list(
            Submission = reactable::colDef(
              minWidth = 250
            ),
            dir  = icon_col(),
            proj = icon_col()
          ),
          onClick = reactable::JS(
            "function(rowInfo, colInfo) {
              if (window.Shiny) {
                Shiny.setInputValue(
                  'submission_click',
                  {col: colInfo.id, cell: rowInfo.row[colInfo.id], row: rowInfo.index+1, debug: rowInfo},
                  { priority: 'event' }
                )
              }
            }"
          )
        )
      })
    }
  )

  shiny::runApp(app, launch.browser = TRUE)
}

icon_col = function(name = "") {
  reactable::colDef(
    minWidth = 40,
    name = name,
    align = "center",
    style = list(
      paddingLeft = "0",
      paddingRight = "0",
      borderLeft = "0"
    ),
    headerStyle = list(
      borderLeft = "0"
    )
  )
}

