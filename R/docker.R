cmd_modal = function(id, event, cmd, args = character(), title = "", interval = 100) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = shiny::NS(id)

      m = shiny::modalDialog(
        title = title,

        shiny::div(
          style = "width:100%; height:400px; margin:0 auto; padding: 5px 10px; border: 1px; overflow-y: auto",
          shiny::verbatimTextOutput(
            ns("cmd_output")
          )
        ),

        easyClose = FALSE,
        size = "l",
        footer = shinyjs::disabled(
          bs_action_button(ns("cmd_modal_dismiss"), text = "Running...")
        )
      )

      observeEvent(event(), {

        output$cmd_output = shiny::renderText({
          paste(cmd_output_txt(), collapse="\n")
        })

        observeEvent(
          input$cmd_modal_dismiss,
          shiny::removeModal()
        )

        shiny::showModal(m)

        p = processx::process$new(
          cmd, args,
          stdout = "|", stderr = "|"
        )
        cmd_output_txt = shiny::reactiveVal(
          paste(c(">", cmd, args), collapse=" ")
        )

        shiny::observe({
          alive = p$is_alive()
          if (alive)
            shiny::invalidateLater(interval)

          new = p$read_output_lines()

          #print(new)

          if (!alive) {
            new = c(
              new, "",
              paste0("Process completed with exit code ", p$get_exit_status())
            )

            shiny::updateActionButton(
              inputId = "cmd_modal_dismiss", label = "Finished",
              icon = shiny::icon("check")
            )

            shinyjs::enable("cmd_modal_dismiss")
          }

          #print(new)
          if (length(new) != 0 && any(new != "")) {
            #print("here")
            shiny::isolate(
              cmd_output_txt( c(cmd_output_txt(), new) )
            )
          }
        })

      })

      NULL
    }
  )
}




# ## Usage example
#
# shiny::shinyApp(
#   ui = shiny::fluidPage(
#     shinyjs::useShinyjs(),
#     shiny::actionButton("show", "Show modal dialog"),
#     theme = bslib::bs_theme(version = 4)
#   ),
#   server = function(input, output, session) {
#     cmd_modal(
#       "docker_pull", shiny::reactive(input$show),
#       "docker", c("run", "--rm", "hello-world"),
#       title = "hello world!",
#       interval = 500
#     )
#   }
# )

