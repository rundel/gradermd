modal_show = function(modal_id, session = shiny::getDefaultReactiveDomain()) {
  shiny:::validate_session_object(session)
  session$sendInputMessage(modal_id, list(state = "show"))
}

modal_hide = function(modal_id, session = shiny::getDefaultReactiveDomain()) {
  shiny:::validate_session_object(session)
  session$sendInputMessage(modal_id, list(state = "hide"))
}

modal_toggle = function(modal_id, session = shiny::getDefaultReactiveDomain()) {
  shiny:::validate_session_object(session)
  session$sendInputMessage(modal_id, list(state = "toggle"))
}

modal_update = function(modal_id, session = shiny::getDefaultReactiveDomain()) {
  shiny:::validate_session_object(session)
  session$sendInputMessage(modal_id, list(update = TRUE))
}

modal_set_options = function(
  modal_id,
  backdrop = NULL,
  keyboard = NULL,
  focus = NULL,
  show = NULL,
  session = shiny::getDefaultReactiveDomain()
) {
  msg = list(
    backdrop = backdrop,
    keyboard = keyboard,
    focus = focus,
    show = show
  )

  shiny:::validate_session_object(session)
  session$sendInputMessage(modal_id, msg)
}


modal_dismiss_button = function(label = "Close", class = "btn btn-default", ...) {
  shiny::tags$button(
    type = "button",
    class = class,
    `data-dismiss` = "modal",
    label,
    ...
  )
}

modal_dismiss_x = function() {
  shiny::tags$button(
    type = "button",
    class = "close",
    `data-dismiss` = "modal",
    shiny::span(
      shiny::HTML("&times;")
    )
  )
}


#' @title Shiny Modal Dialog UI Element
#'
#' @description
#'
#' @param id A unique identifier for the modal dialog
#' @param title The modal dialog's title
#' @param ... UI elements to be contained in the body of the modal dialog
#' @param footer UI elements for footer
#' @param dialog_class Character vector of modifier classes, see Details for
#' commonly used values
#' @param easy_close If `TRUE` the modal dialog can be dismissed by clicking outside
#' the dialog or by pressing Escape.
#' @param fade If `FALSE` do not use a fade in animation.
#' @param close_x If `TRUE` include a `x` button in the header that can dismiss the dialog.
#'
#' @details
#' # Dialog class modifiers:
#'
#' - Vertical centering - `modal-dialog-centered`
#'
#' - Scrolling content - `modal-dialog-scrollable`
#'
#' - Optional sizes - See possible classes / sizes below.
#'
#' | Size        | Class          | Modal max-width |
#' |-------------|----------------|-----------------|
#' | Small       | `modal-sm`     | 300 px          |
#' | Default     | None           | 500 px          |
#' | Large       | `modal-lg`     | 800 px          |
#' | Extra Large | `modal-xl`     | 1140 px         |
#'
modal_dialog = function (
  id, title, ..., footer = modal_dismiss_button(),
  dialog_class = character(),
  easy_close = FALSE, fade = TRUE, close_x = FALSE
) {
  modal_dialog_class = paste(
    c("modal-dialog", dialog_class),
    collapse = " "
  )

  tag = shiny::div(
    id = id,
    class = if (fade) "modal shiny_modal fade"
            else      "modal shiny_modal",
    tabindex = "-1",
    `data-backdrop` = if (!easy_close) "static",
    `data-keyboard` = if (!easy_close) "false",
    shiny::div(
      class = modal_dialog_class,
      shiny::div(
        class = "modal-content",
        shiny::div(
          class = "modal-header",
          if (close_x) modal_dismiss_x(),
          shiny::h4(class = "modal-title", title)
        ),
        shiny::div(
          class = "modal-body",
          list(...)
        ),
        shiny::div(
          class = "modal-footer",
          footer
        )
      )
    )
  )
}


# library(shiny)
# shinyApp(
#   basicPage(
#     shiny::includeScript(system.file("www/modal.js", package="gradermd")),
#     actionButton("showModal1","Show Modal 1"),
#     modal_dialog(
#       "test_modal1", "This is modal 1",
#       footer = shiny::tagList(
#         modal_dismiss_button(),
#         actionButton("showModal2", "Show Modal 2")
#       )
#     ),
#     modal_dialog(
#       "modal2", "This is modal 2",
#       p("After dismissing me, this should return to the first modal"),
#       close_x = TRUE,
#       easy_close = TRUE
#     )
#   ),
#   function(input, output, session) {
#
#     shiny::observeEvent(input$showModal1, {
#       modal_set_options("modal1", backdrop = FALSE, FALSE)
#       modal_show("test_modal1")
#     })
#
#     shiny::observeEvent(input$showModal2, {
#       modal_show("modal2")
#     })
#
#   }
# )
