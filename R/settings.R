settings_ui = function(id, state) {
  ns = shiny::NS(id)

  shiny::tagList(
    shiny::h4("Project Settings"),
    shiny::br(),

    shiny::textInput(
      ns("doc_pat"), label = "Assignment pattern:"#, value = state$get_setting("doc_pat")
    ),
    shiny::checkboxInput(
      ns("doc_regex"), label = "Use regex"#, value = state$get_setting("doc_regex")
    ),

    shiny::hr(),

    shiny::selectInput(
      ns("output"), label = "Output type:",
      choices = c("html", "pdf", "md")#, selected = state$get_setting("output")

    ),

    shiny::hr(),


    shiny::fileInput(
      ns("roster_file"), "Roster file:", accept = ".csv"
    ),
    shiny::uiOutput(
      ns("roster_conditional")
    ),

    shiny::hr(),

    shiny::selectInput(
      ns("render_env"),
      "Render environment:",
      choices = c("Docker" = "docker", "local R" = "local")
    ),
    shiny::uiOutput(
      ns("render_extras")
    ),

    shiny::br(),
    shiny::br(),

    shiny::fluidRow(
      shiny::column(width=4),
      shiny::column(
        width = 4,
        shiny::actionButton(ns("reset"), "Reset", width="75px"),
      ),
      shiny::column(
        width = 4,
        shiny::actionButton(ns("save"), "Save", class = "btn-success", width="75px")
      )
    )
  )
}



settings_server = function(id, state) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns = shiny::NS(id)

      settings_from_state = function() {
        shiny::updateTextInput(
          session, "doc_pat", value = state$get_setting("doc_pat")
        )
        shiny::updateCheckboxInput(
          session, "doc_regex", value = state$get_setting("doc_regex")
        )
        shiny::updateSelectInput(
          session, "output", selected = state$get_setting("output")
        )
      }

      settings_from_state()

      observeEvent(
        input$reset,
        settings_from_state()
      )

      event = shiny::reactiveVal()

      observeEvent(
        input$save,
        {
          state$set_setting(
            doc_pat       = input$doc_pat,
            doc_regex     = input$doc_regex,
            output        = input$output
            #roster_path   = input$roster_path,
            #roster_column = input$roster_column
          )$
            save_settings()$
            update_assignments()$
            update_output()

          # Value needs to change to trigger events downstream
          event("")
          event("save")
        }
      )


      observeEvent(
        input$roster_file,
        {
          shiny::req(input$roster_file)

          d = readr::read_csv(input$roster_file$datapath)
          output$roster = shiny::reactive({
            d
          })

          output$roster_conditional = shiny::renderUI({
            shiny::selectInput(
              ns("roster_join_column"), "Submission names column:",
              choices = names(d)
            )
          })
        }
      )

      cmd_modal(
        "docker_test", shiny::reactive(input$docker_test),
        "docker", c("run", "--rm", "hello-world"),
        title = "Docker test"
      )

      cmd_modal(
        "docker_pull", shiny::reactive(input$docker_pull),
        "docker", c("pull", input$docker_image),
        title = "Docker pull (update) image"
      )

      observeEvent(
        input$render_env,
        {
          ui = if (input$render_env == "local") {
            shiny::div(
              style = "font-size: 75%; padding-left: 0.75em; padding-right: 0.75em;",
              shiny::strong("Warning"),
              " - using your local R environment to run student code is not",
              "recommend as any accidental or intential side effects will affect",
              "this machine (e.g. packages installed / upgrades, files deleted, etc.)"
            )
          } else if (input$render_env == "docker") {
            shiny::div(
              class = "docker_details",
              shiny::textInput("docker_image", "Docker image:", state$get_setting("docker_image")),
              shiny::div(
                class = "docker_links",
                shiny::actionLink(ns("docker_test"), "Test Docker"),
                shiny::actionLink(ns("docker_pull"), "Pull Docker image")
              )
            )
          }

          output$render_extras = shiny::renderUI(ui)
        }
      )

      return(event)
    }
  )
}
