
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
        ),
        actionButton(ns("proceedToFit"), "Proceed to Fit Models", class = "btn-primary", disabled = TRUE)
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