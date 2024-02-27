library(shinycssloaders)
# Define the options for each parameter
modelTypes <- c(
  "exp-aerts", "invexp-aerts", "hill-aerts", "gamma-aerts",
  "invgamma-aerts", "lomax-aerts", "invlomax-aerts", "lognormal-aerts",
  "logskew-aerts", "invlogskew-aerts", "logistic-aerts", "probit-aerts",
  "gamma-efsa", "LMS"
)

fitTypes <- c("laplace", "mle", "mcmc")

bmrTypes <- c("sd", "rel", "hybrid", "abs")

distributions <- c("normal", "normal-ncv", "lognormal")

outcomeTypes <- c("continuous", "dichotomous")

#' Shiny module for model fitting - UI side
#'
#' @param id character, id of module
#' @return UI object
#'
#' @import shiny
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinycssloaders withSpinner
#' @export
fitUI <- function(id) {
  tabPanel(
    "Fit Models",
    # For refreshing div(id = NS(id, "fitPage"),
    fluidRow(
      column(
        6,
        h3("Data Variables"),
        wellPanel(
          selectInput(NS(id, "outcome_type"), "Outcome Type:", choices = outcomeTypes),
          selectInput(NS(id, "model_type"), "Model Type:", choices = modelTypes),
          selectInput(NS(id, "fit_type"), "Fit Type:", choices = fitTypes),
          selectInput(NS(id, "bmr_type"), "BMR Type:", choices = bmrTypes),
          selectInput(NS(id, "distribution"), "Distribution:", choices = distributions)
        ),
        wellPanel(
          uiOutput(NS(id, "submitDoseResponseButton")),
        ),
      ),
      column(
        6,
        h3("Analysis"),
        wellPanel(
          numericInput(NS(id, "bmr"), label = "benchmark response BMR", value = 0.1),
          numericInput(
            NS(id, "alpha"),
            label = "Alpha - the specified nominal coverage rate for computation of the lower bound on the BMDL and BMDU",
            value = 0.05
          ),
          numericInput(NS(id, "seed"), label = "GSL seed", value = 12331),
          conditionalPanel(
            condition = "input['fit-fit_type'] == 'mcmc'",
            numericInput(
              NS(id, "samples"),
              label = "Number of samples",
              value = 25000
            ),
            numericInput(
              NS(id, "burnin"),
              label = "Number of burnin samples",
              value = 1000
            )
          ),
          checkboxInput(
            NS(id, "ewald"),
            label = "perform Wald CI computation",
            FALSE
          )
        )
      )
    )
  )
}


#' Shiny module for loading data - UI side
#'
#' @param id character, id of module
#'
#' @return UI object
#'
#' @import shiny
#' @export
dataUI <- function(id) {
  tabPanel(
    "Data",
    fluidRow(
      column(
        6,
        conditionalPanel("input.helpBMD % 2 == 1",
          ns = NS(id),
          fluidRow(
            column(
              6,
              radioButtons(NS(id, "sep"), "Data separator",
                c(
                  "Tab (.txt file)" = "\t",
                  "Space (.txt file)" = " ",
                  "Comma (.csv file)" = ",",
                  "Semicolon (.csv file)" = ";"
                ),
                selected = " "
              )
            ),
            column(
              6,
              radioButtons(NS(id, "dec"), "Decimal separator", c("Point" = ".", "Comma" = ","))
            )
          )
        ),
        fileInput(
          NS(id, "dataLoaded"),
          label = "",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".rda")
        ),

        # For refreshing the data ui div(id = NS(id, "dataPage"),
        selectInput(
          NS(id, "selectedResponseDosage"),
          label = "Select the dosage column",
          choices = NULL,
          multiple = FALSE
        ),
        selectInput(
          NS(id, "selectedResponse"),
          label = "Which response(s) do you want to consider?",
          choices = NULL,
          multiple = TRUE
        )
      ),
      column(
        6,
        wellPanel(
          DT::DTOutput(NS(id, "dataOverview")),
          uiOutput(NS(id, "dataMessage")),
          uiOutput(NS(id, "dataOutlier"))
        )
      )
    )
  )
}

#' Shiny module for displaying results - UI side
#'
#' @param id character, id of module
#'
#' @return UI object
#'
#' @import shiny
#' @export
resultsUI <- function(id) {
  tabPanel(
    "Results",
    fluidRow(
      column(
        6,
        h3("Command Output:"),
        verbatimTextOutput(NS("fit", "commandText")),
        h3("Model Summary:"),
        verbatimTextOutput(NS("fit", "results")) %>% withSpinner()
      ),
      column(
        6,
        plotOutput(NS("fit", "plotData")) %>% withSpinner(),
        plotOutput(NS("fit", "tracePlot"))
      )
    )
  )
}