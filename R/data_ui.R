
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

#' Shiny module for loading data - UI side
#'
#' @param id character, id of module
#'
#' @return UI object
#'
#' @import shiny
#' @import shinycssloaders
#' @import shinyBS
#' @importFrom shinytoastr useToastr
#' @importFrom shinytoastr toastr_success toastr_error
#' @importFrom shinyjs useShinyjs extendShinyjs enable disable
#' @export
dataUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Data",
    fluidRow(
      column(
        6,
        conditionalPanel(
          condition = "input['data-helpBMD'] % 2 == 1",
          ns = ns, # Ensure correct namespacing
          fluidRow(
            column(
              6,
              radioButtons(ns("sep"), "Data separator",
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
              radioButtons(ns("dec"), "Decimal separator", c("Point" = ".", "Comma" = ","))
            )
          )
        ),
        fileInput(
          ns("dataLoaded"),
          label = "",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".rda")
        ),
        selectInput(
          ns("dataType"),
          label = "Type of data",
          choices = outcomeTypes,
          multiple = FALSE
        ),
        selectInput(
          ns("selectedResponseDosage"),
          label = "Select the dosage column",
          choices = NULL,
          multiple = FALSE
        ),
        fluidRow(
          uiOutput(ns("dichotomous_inputs")),
          uiOutput(ns("continuous_summary_inputs")),
          uiOutput(ns("continuous_full_inputs"))
        ),
        actionButton(ns("proceedToFit"), "Proceed to Fit Models", class = "btn-primary", disabled = TRUE)
      ),
      column(
        6,
        wellPanel(
          DT::DTOutput(ns("dataOverview")),
          uiOutput(ns("dataMessage")),
          uiOutput(ns("dataOutlier"))
        )
      )
    )
  )
}