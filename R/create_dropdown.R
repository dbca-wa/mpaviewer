#Shiny normally resets dropdown to first choice when dropdown is rebuilt in renderUI (e.g. coral site list which changes with park)
#This function keeps whatever the user selected if it is still a valid choice (only relevant for coral at present)
#Used to be defined as a helper function inside app_server, now works within a module

#' Dropdown that keeps its current selection when choices change
#'
#' @param input,ns The module's `input` and `ns` from (`module_Server`).
#' @param input_name Un-namespaced input id.

create_dropdown <- function (input, ns, input_name, choices, label, multiple) {
  selected <- if (!is.null(input[[input_name]]) && input[[input_name]] %in% choices) {
    input[[input_name]]
  } else {
    choices[1]
  }

  shiny::selectInput(
    inputID = ns(input_name),
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple, width = "100%"
  )
}
