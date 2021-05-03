cmd_modal_ui = function(id, label, title = "", type = c("link", "button")) {
  ns = shiny::NS(id)

  type = match.arg(type)

  action = if (type == "link") shiny::actionLink else  shiny::actionButton

  shiny::tagList(
    modal_dialog(
      id = ns("modal_dialog"),
      title = title,

      htmltools::tagAppendAttributes(
        shiny::verbatimTextOutput(ns("modal_output")),
        style = 'width:100%; height: auto; max-height: 90%; overflow-y: auto;
                   margin:0 auto; margin-bottom: 0; padding: 5px 10px; border: 1px;'
      ),

      easy_close = FALSE,
      dialog_class = "modal-lg",
      footer = shinyjs::disabled(
        modal_dismiss_button(
          id = ns("modal_dismiss"),
          shiny::tagList(
            shiny::span(class="spinner-border spinner-border-sm", role="status", `aria-hidden`="true"),
            "Running..."
          )
        )
      )
    ),

    action(ns("modal_trigger"), label),
  )
}



cmd_modal_server = function(id, cmd, args = character(), title = "", interval = 100) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = shiny::NS(id)

      observeEvent(input$modal_trigger, {
        output_txt = shiny::reactiveVal(
          paste(c(">", cmd, args), collapse=" ")
        )

        output$modal_output = shiny::renderText({
          paste(output_txt(), collapse="\n")
        })

        modal_show("modal_dialog")

        p = processx::process$new(
          cmd, args,
          stdout = "|", stderr = "|"
        )

        shiny::observe({
          alive = p$is_alive()
          if (alive)
            shiny::invalidateLater(interval)

          new = p$read_output_lines()


          if (!alive) {
            new = c(
              new, "",
              paste0("Process completed with exit code ", p$get_exit_status())
            )

            shinyjs::enable("modal_dismiss")

            shinyjs::runjs( paste0(
              "document.getElementById('", ns("modal_dismiss"), "').innerHTML = 'Dismiss'"
            ) )
          }

          print(new)
          if (length(new) != 0 && any(new != "")) {
            shiny::isolate(
              output_txt( c(output_txt(), new) )
            )
          }
        })

      })

      NULL
    }
  )
}



test_docker_modal = function() {

  shiny::addResourcePath("www", system.file("www/", package="gradermd"))

  # ## Usage example
  #
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shinyjs::useShinyjs(),
      shiny::tags$script(type="text/javascript", src="www/modal.js"),
      cmd_modal_ui("test", label = "Run ...", type = "button"),
      theme = bslib::bs_theme(version = 4)
    ),
    server = function(input, output, session) {
      cmd_modal_server(
        "test",
        "docker", c("run", "--rm", "hello-world"),
        title = "hello world!",
        interval = 500
      )
    }
  )
}
