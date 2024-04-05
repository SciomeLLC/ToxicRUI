library(shiny)
library(bayesplot)
library(ToxicR)
library(shinycssloaders)

#' Find best variable name to match with given input field
#' @param patterns character vector, contains all possible patterns, in order
#' of importance, that will be checked for
#' @param choices vector, variable names in which to search for pattern
#' @return one element of \code{choices} that matches best with \code{patterns
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

#' Read BMD data for analysis
#'
#' Updated because of decimal separator, extra argument compared to f.scan()
#' @param fileName character, name of the file (without full path)
#' @param filePath path name, where is the .txt or .csv file located
#' @param separator delimiter for the dataset
#' @param decimal character, used for decimal points
#' @return dataframe
#' @importFrom utils read.table
#' @export
readDataBMD <- function(fileName, filePath, separator, decimal) {
  extension <- substring(
    fileName,
    first = (nchar(fileName) - 2),
    last = nchar(fileName)
  )

  tryCatch(
    {
      if (extension == "rda") {
        load(file.path(filePath))
        tmpData <- get(substring(fileName, first = 1, last = (nchar(fileName) - 4)))$data
      } else {
        tit <- scan(filePath, "", nlines = 1, sep = separator)
        if (sum(sapply(tit, function(x) nchar(x) > 0)) == 1) {
          tmpData <- read.table(
            filePath,
            skip = 4,
            sep = separator,
            dec = decimal,
            row.names = NULL
          )
        } else {
          tmpData <- read.table(filePath, header = T, sep = separator, dec = decimal)
        }
      }

      return(tmpData)
    },
    error = function(err) {
      return(err)
    }
  )
}

#' Main server function
#'
#' @param input Shiny object
#' @param output Shiny object
#' @param session Shiny object
#'
#' @return no return value
#'
#'
#' @import shiny
#' @import ToxicR
#' @import bayesplot
#' @importFrom rmarkdown render
#'
#' @export
serverFunction <- function(input, output, session) {
  
  # For debugging
  observeEvent(input$debug_console, browser())
  output$debug_print <- renderPrint(resultData)

  # Load code for all tabpages

  resultData <- dataServer("data")

  fitResults <- fitServer(
    id = "fit",
    loadedData = resultData$loadedData,
    message = resultData$message,
    selectedResponse = resultData$selectedResponse,
    selectedResponseDosage = resultData$selectedResponseDosage,
    inFile = resultData$inFile
  )


  # Refresh all input fields upon loading (new) data
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
#' Shiny module for loading data - server side
#'
#' @param id character, id of module
#'
#' @return reactive object, loaded data
#'
#' @import shiny
#' @importFrom shinyjs toggle hidden
#' @importFrom utils read.csv
#' @export
dataServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      initialData <- reactiveVal(NULL)
      subsetData <- reactiveVal(NULL)
      inputData <- reactiveVal(NULL) # data ready to be used as input for further analysis

      message <- reactiveValues(
        subset = NULL,
        outlier = NULL,
        missing = NULL
      )

      ## 1. Load Data ##
      observeEvent(input$dataLoaded, {
        # Detect extension. If Control Data is not openend, automatically, otherwise takes inputs as
        # defined in conditional panel
        inFile <- input$dataLoaded
        if (is.null(inFile) || input$dataLoaded$size == 0) {
          print("file is null\n")
          return()
        }
        newName <- inFile$name
        decimal <- "."

        extension <- substring(newName, first = (nchar(newName) - 2), last = nchar(newName))

        if (extension == "csv") {
          updateRadioButtons(session, "sep", selected = ",")
          separator <- ","
        } else if (extension == "txt") {
          updateRadioButtons(session, "sep", selected = "\t")
          separator <- "\t"
        }

        # Read out data
        tmpData <- readDataBMD(
          fileName = input$dataLoaded$name,
          filePath = input$dataLoaded$datapath,
          separator = separator,
          decimal = decimal
        )

        initialData(tmpData)
      })

      observe({
        subsetData(initialData())
      })
      # Show all data (including outliers)
      dataLoadedOut <- reactive({
        validate(need(subsetData(), "No data loaded"))
        # Remove samples containing NA from data RatStudy.csv
        stats::na.omit(subsetData())
      })


      ## 3. Select response(s)
      observe(
        {
          req(initialData())

          updateSelectInput(
            session = session, inputId = "selectedResponse",
            choices = c("", colnames(initialData())),
            selected = "" # bestChoice(patterns = c("response"), choices = colnames(initialData()))
          )
          updateSelectInput(
            session = session, inputId = "selectedResponseDosage",
            choices = c("", colnames(initialData())),
            selected = "" # bestChoice(patterns = c("response"), choices = colnames(initialData()))
          )
        },
        priority = 1000
      )

      ## 4. Show data
      output$dataOverview <- DT::renderDT({
        req(dataLoadedOut())

        # Avoid rounding non-numeric variables
        tmpData <- dataLoadedOut()
        for (i in colnames(tmpData)) {
          tmpData[[i]] <- if (is.numeric(tmpData[[i]])) {
            round(tmpData[[i]], 3)
          } else {
            tmpData[[i]]
          }
        }

        # Build datatable
        DT::datatable(
          data = tmpData,
          options = list(
            scrollX = TRUE,
            lengthMenu = list(c(15, 100, -1), c("15", "100", "All")),
            pageLength = 15
          ),
          rownames = FALSE,
          selection = "multiple"
        )
      })

      output$dataMessage <- renderUI({
        req(dataLoadedOut())
        nMissing <- nrow(subsetData()) - nrow(dataLoadedOut())
        tagList(
          # Note if there were incomplete cases removed from the data
          if (nMissing > 0) {
            tags$em(paste("There were", nMissing, "records excluded from the data due to missing values."))
          },
          br(),
          tags$em("You can select rows in the table that should be excluded from the analysis (outliers).")
        )
      })

      ## 5. Exclude outliers if necessary
      observe({
        selectedRows <- input$dataOverview_rows_selected
        if (is.null(selectedRows)) {
          inputData <- inputData(dataLoadedOut())
        } else {
          inputData <- inputData(dataLoadedOut()[-selectedRows, ])

          message$outlier <- paste0(
            "Warning: You selected", length(selectedRows),
            "rows to be excluded from the analysis.\n",
            knitr::kable(dataLoadedOut()[selectedRows, ], digits = 3)
          )
        }
      })

      output$dataOutlier <- renderUI({
        req(input$dataOverview_rows_selected)

        tags$div(
          style = "margin-top:60px; margin-bottom:20px;",
          tags$em(paste("You have selected", length(input$dataOverview_rows_selected), "outlier(s).")),
        )
      })

      ## 6. Output of dataServer
      list(
        loadedData = reactive(inputData()),
        selectedResponse = reactive(input$selectedResponse),
        selectedResponseDosage = reactive(input$selectedResponseDosage),
        message = reactive(message),
        inFile = reactive(input$dataLoaded)
      )
    }
  )
}



#' Shiny module for model fitting - server side
#'
#' @param id character. id of module
#' @param loadedData data frame, data after subsetting
#' @param message list, messages from data loading tab for report
#' @param selectedResponse character, selected responses
#' @return list with fitted models and analysis data
#'
#' @import shiny
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_col hot_to_r
#' @importFrom shinyFeedback feedbackWarning
#' @importFrom magrittr %>%
#' @importFrom utils packageVersion
#' @importFrom ggplotify as.ggplot
#' @importFrom mailR send.mail
#' @importFrom stats na.omit
#'
#' @export
fitServer <- function(id, loadedData, message, selectedResponse, selectedResponseDosage, inFile) {
  moduleServer(
    id,
    function(input, output, session) {
      command_str_reactive <- reactiveVal("")
      fit_results_reactive <- reactiveVal(NULL)
      average_results_reactive <- reactiveVal(NULL)
      mcmc_results_reactive <- reactiveVal(NULL)
      observeEvent(input$submitDoseResponse, {
        modelling_type <- input$average_or_fit
        D <- loadedData()[[selectedResponseDosage()]]
        Y <- loadedData()[[selectedResponse()]]
        dosage_column <- selectedResponseDosage()
        data_columns <- selectedResponse()
        fit_type <- input$fit_type
        bmr_type <- input$bmr_type
        bmr <- input$bmr
        alpha <- input$alpha
        samples <- input$samples
        burnin <- input$burnin
        outcome_type <- input$outcome_type
        seed <- input$seed
        fileInfo <- inFile()
        if (modelling_type == "fit") {
          model_type <- input$model_type
          ewald <- input$ewald
          distribution <- input$distribution
          # transform <- input$transform
          fit_results <- tryCatch({
            if (outcome_type == "continuous") {
              single_continuous_fit(
                D = D,
                Y = Y,
                model_type = model_type,
                fit_type = fit_type,
                BMR_TYPE = bmr_type,
                BMR = bmr,
                distribution = distribution,
                alpha = alpha,
                burnin = burnin,
                samples = samples,
                ewald = ewald,
                # transform = transform,
                seed = seed
              )
            } else {
              single_dichotomous_fit(
                D = D,
                Y = Y,
                model_type = model_type,
                fit_type = fit_type,
                BMR = bmr,
                alpha = alpha,
                burnin = burnin,
                samples = samples,
                seed = seed
              )
            }
          }, error = function(e) {
            print(e)
            return(list(error = toString(e)))
          })

          if (outcome_type == "continuous") {
            command_str <- paste0("library(ToxicR)
library(ToxicRUI)
fileData <- readDataBMD(
  fileName='", fileInfo$name, "', 
  filePath='", fileInfo$datapath, "', 
  separator=',', 
  decimal='.'
)
D <- fileData[['", dosage_column, "']]
Y <- fileData[['", data_columns, "']]
fitResults <- single_continuous_fit(
  D = D, 
  Y = Y, 
  model_type = '", model_type, "', 
  fit_type = '", fit_type, "', 
  BMR_TYPE = '", bmr_type, "', 
  BMR = ", bmr, ", 
  distribution = '", distribution, "', 
  alpha = ", alpha, ", 
  burnin = ", burnin, ", 
  samples = ", samples, ", 
  ewald = ", ewald, ", 
  transform = ", transform, ", 
  seed = ", seed,
")
plot(fitResults)
summary(fitResults)
"
            )
          } else {
            command_str <- paste0("library(ToxicR)
library(ToxicRUI)
fileData <- readDataBMD(fileName='", fileInfo$name, "', filePath='", fileInfo$datapath, "', separator=',', decimal='.')
D <- fileData[['", dosage_column, "']]
Y <- fileData[['", data_columns, "']]
fitResults <- single_dichotomous_fit(
  D = D, 
  Y = Y, 
  model_type = '", model_type, "', 
  fit_type = '", fit_type, "', 
  BMR = ", bmr, ", 
  alpha = ", alpha, ", 
  burnin = ", burnin, ", 
  samples = ", samples, ", 
  transform = ", transform, ", 
  seed = ", seed, "
)
plot(fitResults)
summary(fitResults)"
            )
          }

          fit_results_reactive(fit_results)
          if (fit_type == "mcmc") {
            temp <- as.matrix(fit_results$mcmc_result$PARM_samples)

            if (distribution == "normal-ncv") {
              colnames(temp) <- c(letters[1:(ncol(temp)-2)], "Non-constant var", "log(sig2)")
            } else {
              colnames(temp) <- c(letters[1:(ncol(temp)-1)], "log(sig2)")
            }
            mcmc_results_reactive(temp)
          }
        }

        if (modelling_type == "average") {
          average_results <- tryCatch({
            if (outcome_type == "continuous") {
              ma_continuous_fit(
                D = D,
                Y = Y,
                fit_type = fit_type,
                BMR_TYPE = bmr_type,
                BMR = bmr,
                alpha = alpha,
                burnin = burnin,
                samples = samples,
                seed = seed
              )
            } else {
              ma_dichotomous_fit(
                D = D,
                Y = Y,
                fit_type = fit_type,
                BMR_TYPE = bmr_type,
                BMR = bmr,
                alpha = alpha,
                burnin = burnin,
                samples = samples,
                seed = seed
              )
            }
          }, error = function(e) {
            print(e)
            return(list(error = toString(e)))
          })

          if (outcome_type == "continuous") {
            command_str <- paste0("library(ToxicR)
library(ToxicRUI)
fileData <- readDataBMD(fileName='", fileInfo$name, "', filePath='", fileInfo$datapath, "', separator=',', decimal='.')
D <- fileData[['", dosage_column, "']]
Y <- fileData[['", data_columns, "']]
fitResults <- ma_continuous_fit(
  D = D, 
  Y = Y, 
  fit_type = '", fit_type, "', 
  BMR_TYPE = '", bmr_type, "', 
  BMR = ", bmr, "',
  alpha = ", alpha, ", 
  burnin = ", burnin, ", 
  samples = ", samples, ", 
  seed = ", seed, "
)
plot(fitResults)
summary(fitResults)"
            )
          } else {
            command_str <- paste0("library(ToxicR)
library(ToxicRUI)
fileData <- readDataBMD(fileName='", fileInfo$name, "', filePath='", fileInfo$datapath, "', separator=',', decimal='.')
D <- fileData[['", dosage_column, "']]
Y <- fileData[['", data_columns, "']]
fitResults <- ma_dichotomous_fit(
  D = D, 
  Y = Y, 
  model_type = '", model_type, "', 
  fit_type = '", fit_type, "', 
  BMR_TYPE = '", bmr_type, "', 
  BMR = ", bmr, "', 
  alpha = ", alpha, ", 
  burnin = ", burnin, ", 
  samples = ", samples, ", 
  seed = ", seed, "
)
plot(fitResults)
summary(fitResults)"
            )
          }
          average_results_reactive(average_results)
          # if (fit_type == "mcmc") {
          #   temp <- as.matrix(fit_results$mcmc_result$PARM_samples)
          #   if (distribution == "normal=ncv") {
          #     colnames(temp) <- c(letters[1:(ncol(temp)-2)], "Non-constant var", "log(sig2)")
          #   } else {
          #     colnames(temp) <- c(letters[1:(ncol(temp)-1)], "log(sig2)")
          #   }
          #   mcmc_results_reactive(temp)
          # }
        }
        command_str_reactive(command_str)
      })
      observe({
        if (!is.null(fit_results_reactive())) {
          updateTabsetPanel(session=session, inputId="tabs", selected = "results")
          output$plotData <- renderPlot({
            plot(fit_results_reactive())
          })
          output$results <- renderPrint({
            if (is.list(fit_results_reactive()) && !is.null(fit_results_reactive()$error)) {
              print(fit_results_reactive()$error)
            } else {
              summary(fit_results_reactive())
            }
          })
        }
        if (!is.null(average_results_reactive())) {
          updateTabsetPanel(session=session, inputId="tabs", selected = "results")
          output$plotData <- renderPlot({
            plot(average_results_reactive())
          })

          output$results <- renderPrint({
            summary(average_results_reactive())
          })
        }
        if (!is.null(command_str_reactive())) {
          output$commandText <- renderText(command_str_reactive())
        }
      })
      observe({
        if (!is.null(mcmc_results_reactive())) {
          output$tracePlot <- renderPlot({
            bayesplot::mcmc_trace(mcmc_results_reactive())
            # ggplot(mcmc_results_reactive(), aes(x=Sample, y = Value)) + geom_line() + facet_wrap(~Parameter, scales = "free")
          })
        }
      })
      output$submitDoseResponseButton <- renderUI({
        actionButton(NS(id, "submitDoseResponse"), label = "Run dose-response analysis")
      })
    }
  )
}
