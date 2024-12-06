#' Shiny module for model fitting - UI side
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
fitUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Fit Models",
    fluidRow(
      column(
        6,
        h3("Data Variables"),
        wellPanel(
          selectInput(NS(id, "average_or_fit"), "Modelling Type:", choices = averageOrFit),
          conditionalPanel(
            condition = "input['fit-average_or_fit'] == 'fit'",
            ns = ns,
            selectInput(NS(id, "model_type"), "Model Type:", choices = modelTypes),
            selectInput(NS(id, "distribution"), "Distribution:", choices = distributions)
          ),
          selectInput(NS(id, "fit_type"), "Fit Type:", choices = fitTypes),
          selectInput(NS(id, "bmr_type"), "BMR Type:", choices = bmrTypes)
        ),
        actionButton(ns("submitDoseResponse"), "Run Dose-Response Analysis", disabled = FALSE, class = "btn-primary")
      ),
      column(
        6,
        h3("Analysis Parameters"),
        wellPanel(
          numericInput(NS(id, "bmr"), label = "benchmark response BMR", value = 0.1),
          numericInput(
            ns("alpha"),
            label = "Alpha - the specified nominal coverage rate for computation of the lower bound on the BMDL and BMDU",
            value = 0.05
          ),
          numericInput(NS(id, "seed"), label = "GSL seed", value = 12331),
          conditionalPanel(
            condition = "input['fit-fit_type'] == 'mcmc'",
            ns = ns,
            numericInput(
              ns("samples"),
              label = "Number of samples",
              value = 25000
            ),
            numericInput(
              ns("burnin"),
              label = "Number of burnin samples",
              value = 1000
            )
          ),
          conditionalPanel(
            condition = "input['fit-average_or_fit'] == 'fit'",
            checkboxInput(
              ns("ewald"),
              label = "perform Wald CI computation",
              FALSE
            )
          )
        )
      )
    ),
    value = "fit-models"
  )
}