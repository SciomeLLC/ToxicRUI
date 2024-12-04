#' Shiny module for the results - UI side
#'
#' @param id character, id of module
#' @return UI object
#'
#' @import shiny
#' @import shinyBS
#' @import shinycssloaders
#' @importFrom shinytoastr useToastr
#' @importFrom shinytoastr toastr_success toastr_error
#' @importFrom shinyjs useShinyjs extendShinyjs enable disable
#' @export
resultsUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Results",
    fluidRow(
      column(
        12,
        h3("Analysis Results"),
        withSpinner(
          uiOutput(ns("resultsDisplay")),
          type = 6,
          color = "blue",
          size = 2
        )
      )
    ),
    value = "results"
  )
}