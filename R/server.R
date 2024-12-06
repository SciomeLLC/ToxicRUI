#' Main server function
#'
#' @param input Shiny object
#' @param output Shiny object
#' @param session Shiny object
#'
#' @return no return value
#'
#' @import shiny
#' @import bayesplot
#' @import ToxicR
#' @import shinycssloaders
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_col hot_to_r
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom magrittr %>%
#' @importFrom utils packageVersion
#' @importFrom ggplotify as.ggplot
#' @importFrom mailR send.mail
#' @importFrom stats na.omit
#' @importFrom shinyjs enable disable
#' @importFrom shinytoastr toastr_error
#'
#' @export
serverFunction <- function(input, output, session) {
  session$userData$fitReady <- FALSE
  session$userData$resultsReady <- FALSE
  res <- reactiveValues()
  # For debugging
  observeEvent(input$debug_console, browser())
  output$debug_print <- renderPrint(resultData)

  # Load code for all tabpages
  resultData <- dataServer("data")

  ## 1. Observe the "Proceed to Fit Models" Click Event ##
  observeEvent(resultData$proceedToFitClicked(), {
    session$userData$fitReady <- TRUE
    updateTabsetPanel(session, "tabs", selected = "fit-models")
  })

  fitResults <- fitServer(
    id = "fit",
    loadedData = resultData$loadedData,
    message = resultData$message,
    selectedResponse = resultData$selectedResponse,
    sample_col = resultData$samples,
    std_dev = resultData$std_dev,
    outcome = resultData$outcome,
    selectedResponseDosage = resultData$selectedResponseDosage,
    inFile = resultData$inFile
  )

  observeEvent(fitResults$proceedToResultsClicked(), {
    session$userData$resultsReady <- TRUE
    updateTabsetPanel(session, "tabs", selected = "results")
  })

  resultsServer(
    id = "results",
    analysis_result = fitResults$analysis_result
  )

  observeEvent(input$dataLoaded, {
    if (!is.null(input$responses)) {
      shinyjs::reset("data-dataPage")
      shinyjs::reset("fit-fitPage")
    }
  })
  output$downloadConsole <- downloadHandler(
    filename = "consoleOutput.txt",
    content = function(file) {
      file.copy(file.path(tempdir(), "consoleOutput.txt"), file)
    }
  )
}

#' Find best variable name to match with given input field
#' @param patterns character vector, contains all possible patterns, in order
#' of importance, that will be checked for
#' @param choices vector, variable names in which to search for pattern
#' @return one element of \code{choices} that matches best with \code{patterns}
#' @export
bestChoice <- function(patterns, choices) {

  allCandidates <- do.call(c, lapply(patterns, function(iPattern) {
    grep(pattern = iPattern, x = choices, ignore.case = TRUE)
  }))

  if (length(allCandidates) > 0) {
    return(choices[allCandidates[1]])
  } else {
    return("none")
  }
}