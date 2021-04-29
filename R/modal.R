test_modal_button = function(label = "Close") {
  shiny::tags$button(
    type = "button",
    class = "btn btn-default",
    `data-dismiss` = "modal",
    label
  )
}

test_modal = function (id, title, trigger, ..., size = NULL, footer = test_modal_button()) {
  if (!is.null(size)) {
    size = if (size == "large") "modal-lg"
    else if (size == "small") "modal-sm"
  }
  size = paste(c("modal-dialog", size), collapse = " ")

  tag = shiny::tags$div(
    class = "modal sbs-modal fade",
    id = id, tabindex = "-1", `data-sbs-trigger` = trigger,
    shiny::tags$div(
      class = size,
      shiny::tags$div(
        class = "modal-content",
        shiny::tags$div(
          class = "modal-header",
          shiny::tags$h4(class = "modal-title", title)
        ),
        shiny::tags$div(
          class = "modal-body",
          list(...)
        ),
        shiny::tags$div(
          class = "modal-footer",
          footer
        )
      )
    )
  )
  #htmltools::attachDependencies(tag, shinyBS:::shinyBSDep)
}


library(shiny)
shinyApp(
  basicPage(
    shiny::includeScript(system.file("www/modal.js", package="gradermd")),
    actionButton("showModal1","Show Modal 1"),
    test_modal("modal1", "this is modal 1", "showModal1", actionButton("showModal2", "Show Modal 2")),
    test_modal("modal2", "this is modal 2", "showModal2", p("After dismissing me, this should return to the first modal"))
  ),
  function(input, output, session) {
  }
)
