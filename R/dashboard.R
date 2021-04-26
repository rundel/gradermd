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

        shiny::tags$div(class = "dropdown-divider"),

        shiny::actionLink(
          "menu_render_all", "Render all ...",
          class = "dropdown-item"
        ),

        shiny::actionLink(
          "menu_render_outdated", "Render outdated ...",
          class = "dropdown-item"
        ),

        shiny::actionLink(
          "menu_render_missing", "Render missing ...",
          class = "dropdown-item"
        ),


        shiny::tags$div(class = "dropdown-divider"),

        shiny::actionLink(
          "menu_reload", "Reload",
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


settings_dialog_ui = function(input, output, session, state) {

  observeEvent(
    input$settings_cancel,
    shiny::removeModal()
  )

  observeEvent(
    input$settings_save,
    {
      state$set_setting(
        doc_pat       = input$setting_doc_pat,
        doc_regex     = input$setting_doc_regex,
        output        = input$setting_output
        #roster_path   = input$setting_roster_path,
        #roster_column = input$setting_roster_column
      )$
        save_settings()$
        update_assignments()$
        update_output()

      shiny::removeModal()

      #reactable::updateReactable(
      #  "table", data = state$get_table(), session = session
      #)

      # Rerender the whole thing because update is broken atm.
      output$table = render_table(state)
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
        shiny::textInput("setting_doc_pat", label = "Document pattern:", value = state$get_setting("doc_pat")),
        shiny::checkboxInput("setting_doc_regex", label = "Use regex", value = state$get_setting("doc_regex"))
      )
    ),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::selectInput(
          "setting_output", label = "Output type:",
          choices = c("html", "pdf", "md"),
          selected = state$get_setting("output")
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

#' @export
dashboard = function(dir = "~/Desktop/StatProg-s1-2020/Marking/hw1/repos/", pattern = NULL, regexp = FALSE) {

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

      state = dashboard_state$new(dir, pattern, regexp)

      if (!state$had_settings()) { # This is a new project so show settings pane
        settings_dialog_ui(input, output, session, state) %>%
          shiny::showModal()
      }

      observeEvent(
        input$menu_settings,
        settings_dialog_ui(input, output, session, state) %>%
          shiny::showModal()
      )

      observeEvent(
        input$menu_render_missing,
        {
          if (state$is_rendering()) {
            print("already rendering")
            return()
          }


          missing = which(is.na(state$get_table()$Output))[1:5]
          print(missing)

          state$start_rendering()$
            set_status(i = missing, col = "Output", status = "rendering")

          output$table = render_table(state)

          x = promises::promise_map(
            c(0, missing),
            function(i) {
              if (i == 0) # Seems needed to quickly return to unblock output
                return(i)

              promises::future_promise({
                cat("Started", i,"\n")
                state$render(i)$
                  set_cell(i = i, col = "Output", value = "Blah")$
                  set_status(i = i, col = "Output", status = "ok")

                cat("Finished", i,"\n")
                i
              })
            }
          ) %...>%
            (function(x) {
              state$stop_rendering()
            })

          # future::future({
          #   for(i in missing) {
          #     cat("Started", i,"\n")

          #     state$render(i)$
          #       set_cell(i = i, col = "Output", value = "Blah")$
          #       set_status(i = i, col = "Output", status = "ok")

          #     cat("Finished", i,"\n")
          #   }
          # }) %...>%
          #   (function(x) {
          #     state$stop_rendering()
          #   })

          NULL
        }
      )


      # Handle clicks on the submissions column
      observeEvent(
        input$submission_click,
        {
          state$click(
            cell = input$submission_click$cell,
            col  = input$submission_click$col,
            row  = input$submission_click$row
          )
        }
      )

      observeEvent(
        input$refresh_table,
        {
          output$table = render_table(state)
        }
      )


      output$table = render_table(state)
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

render_table = function(state) {

  reactable::renderReactable({

    if (state$is_rendering())
      shiny::invalidateLater(1000)

    cat("here!", state$is_rendering(), "\n")

    reactable::reactable(
      state$get_table(),
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
        #style = list(fontFamily = "Fira Code, monospace, monospace"),
        searchInputStyle = list(width = "100%")
      ),
      defaultColDef = reactable::colDef(
        minWidth = 150,
        style = "user-select: none",
        cell = function(value, row, col) {
          status = attr(state$get_table()[[col]], "status")[row] %||% "ok"

          l = list(class = paste0("tag status-",status))

          if (status == "missing") {
            l = c(l, "Missing")
          } else if (status == "rendering") {
            l = c(l, list(bs_spinner("Rendering")), "Rendering")
          } else {
            l = c(l, add_icons(value, col, state))
          }


          do.call(htmltools::div, l)
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

add_icons = function(value, col, state) {

  col_icons = list(
    dir  = function() {
      if (value) list(rs_icon("folder", 24))
    },

    proj = function() {
      if (value) list(rs_icon("application-x-r-project", 24))
    },

    Assignment = function() {
      list(rs_icon("newRMarkdownDoc"), value)
    },

    Output = function() {
      if (state$get_setting("output") == "html") {
        list(rs_icon("newHtmlDoc"), value)
      } else if (state$get_setting("output") == "pdf") {
        list(rs_icon("compilePDF"), value)
      } else if (state$get_setting("output") == "md") {
        list(rs_icon("newMarkdownDoc"), value)
      }

    },

    default = function() {
      value
    }
  )

  (col_icons[[col]] %||% col_icons$default)()
}



