docker_pull_ui = function(input, output, session, image) {

  shiny::modalDialog(
    title = paste0("Updating Docker image: ", image),

    shiny::div(
      style = "width:100%; height:400px; margin:0 auto; padding: 5px 10px; border: 1px; overflow-y: auto",
      shiny::verbatimTextOutput(
        "pull_output"
      )
    ),

    easyClose = FALSE,
    size = "m",
  )

 # print("hello")
}

image = "rocker/rstudio"

shinyApp(
  ui = basicPage(
    actionButton("show", "Show modal dialog")
  ),
  server = function(input, output, session) {
    observeEvent(input$show, {
      showModal(docker_pull_ui(input, output, session, image))

      pull_output_txt = reactiveVal(NULL)

      p = processx::process$new(
        "docker", c("pull", image),
        stdout = "|", stderr = "|",
        echo_cmd = TRUE
      )


      shiny::observe({
        alive = p$is_alive()
        if (alive)
          shiny::invalidateLater(500)

        new = p$read_output_lines()

        if (!alive)
          new = c(
            new, "",
            paste0("Process completed with exit code ", p$get_exit_status())
          )

        if (length(new) != 0 && new != "") {
          print(new)
          pull_output_txt(
            c(pull_output_txt(), new)
          )
        }

      })

      output$pull_output = shiny::renderText({
        paste(pull_output_txt(), collapse="\n")
      })

    })
  }
)
