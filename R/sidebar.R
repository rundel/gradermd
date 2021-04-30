sidebar_ui_button = function(id, type = c("button", "link"), label, ...) {
  ns = shiny::NS(id)
  type = match.arg(type)

  if (type == "button")    shiny::actionButton(ns("sidebar_toggle"), label, ...)
  else if (type == "link") shiny::actionLink(ns("sidebar_toggle"), label, ...)
}

sidebar_ui_div = function(...) {
  shiny::tagList(
    shiny::tags$style( shiny::HTML("
        .sidebar {
          height: 100%;
          width: 0;
          position: fixed;
          z-index: 100;
          top: 0;
          right: 0;
          background-color: #ccc;
          overflow-x: hidden;
          transition: width 0.25s;
          transition-timing-function: ease;
          padding-top: 60px;
        }

        #sidebar-content {
          padding: 0 1em;
          min-width: 250px;
        }
    ")),
    shiny::div(
      id = "right-sidebar", class = "sidebar",
      shiny::div(
        id = "sidebar-content",
        ...
      )
    )
  )
}

sidebar_server = function(id, body_id, width = 250) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      collapsed = shiny::reactiveVal(TRUE)

      shinyjs::runjs(glue::glue(
        'document.getElementById("sidebar-content").style.width = "{width}px";',
      ) )

      shiny::observeEvent(input$sidebar_toggle, {
        x = if (collapsed()) width else 0

        shinyjs::runjs(glue::glue(
          'document.getElementById("right-sidebar").style.width = "{x}px";',
          'document.getElementById("{body_id}").style.marginRight = "{x}px";'
        ))

        collapsed(!collapsed())
      })
    }
  )
}

