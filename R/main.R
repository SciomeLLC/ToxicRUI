library(shinycssloaders)
# Define the options for each parameter
modelTypes <- c(
  "Exponential Aerts" = "exp-aerts",
  "Inverse Exponential Aerts" = "invexp-aerts",
  "Hill Aerts" = "hill-aerts",
  "Gamma Aerts" = "gamma-aerts",
  "Inverse Gamma Aerts" = "invgamma-aerts",
  "Lomax Aerts" = "lomax-aerts",
  "Inverse Lomax Aerts" = "invlomax-aerts",
  "Log-Normal Aerts" = "lognormal-aerts",
  "Log-Skew-Normal Aerts" = "logskew-aerts",
  "Inverse Log-Skew-Normal Aerts" = "invlogskew-aerts",
  "Logistic Aerts" = "logistic-aerts",
  "Probit Aerts" = "probit-aerts",
  "Gamma EFSA" = "gamma-efsa",
  "Least Mean Squares" = "LMS"
)

fitTypes <- c("laplace", "mle", "mcmc")

bmrTypes <- c(
  "Standard Deviation" = "sd",
  "Relative" = "rel",
  "Hybrid" = "hybrid",
  "Absolute" = "abs"
)

distributions <- c(
  "Normal" = "normal",
  "Normal with normalized coefficient of variation" = "normal-ncv",
  "Lognormal" = "lognormal"
)

outcomeTypes <- c(
  "Continuous" = "continuous",
  "Dichotomous" = "dichotomous"
)

averageOrFit <- c(
  "Model Averaging" = "average",
  "Individual Model" = "fit"
)

averagingUI <- function(id) {
  tabPanel(
    "Averaging Models",
    fluidRow(
      column(
        6,
        h3("Data Variables"),
        wellPanel(

        ),
        wellPanel(
          uiOutput(NS(id, "submitAverageResponeButton"))
        )
      ),
      column(
        6,
        h3("Analysis"),
        wellPanel(
        )
      )
    )
  )
}
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
          selectInput(NS(id, "average_or_fit"), "Modelling Type:", choices = averageOrFit),
          selectInput(NS(id, "outcome_type"), "Outcome Type:", choices = outcomeTypes),
          conditionalPanel(
            condition = "input['fit-average_or_fit'] == 'fit'",
            selectInput(NS(id, "model_type"), "Model Type:", choices = modelTypes),
            selectInput(NS(id, "distribution"), "Distribution:", choices = distributions)
          ),
          selectInput(NS(id, "fit_type"), "Fit Type:", choices = fitTypes),
          selectInput(NS(id, "bmr_type"), "BMR Type:", choices = bmrTypes)
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
          conditionalPanel(
            condition = "input['fit-average_or_fit'] == 'fit'",
            checkboxInput(
              NS(id, "ewald"),
              label = "perform Wald CI computation",
              FALSE
            )
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
        plotOutput(NS("fit", "tracePlot")) %>% withSpinner()
      )
    )
  )
}