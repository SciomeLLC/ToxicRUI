
#' Shiny module for loading data - server side
#'
#' @param id character, id of module
#'
#' @return reactive object, loaded data
#'
#' @import shiny
#' @import shinujs
#' @importFrom shinyjs toggle hidden enable disable toggleState
#' @importFrom shinytoastr toastr_error toastr_warning toastr_success
#' @importFrom utils read.csv
#' @export
dataServer <- function(id) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      initialData <- reactiveVal(NULL)
      subsetData <- reactiveVal(NULL)
      inputData <- reactiveVal(NULL)
      proceedToFitClicked <- reactiveVal(FALSE)
      message <- reactiveValues(
        subset = NULL,
        outlier = NULL,
        missing = NULL
      )
      std_dev <- reactiveVal(NULL)
      selectedResponse <- reactiveVal(NULL)
      samples <- reactiveVal(NULL)
      outcome <- reactiveVal(NULL)
      ## 1. Load Data ##
      observeEvent(input$dataLoaded, {
        inFile <- input$dataLoaded
        if (is.null(inFile) || input$dataLoaded$size == 0) {
          toastr_error("Unable to read file!", "Error")
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
        if (inherits(tmpData, "error")) {
          toastr_error(paste0("Error loading data:", tmpData$message), "Error")
          return()
        }
        initialData(tmpData)
      })

      observe({
        subsetData(initialData())
      })
      # Show all data (including outliers)
      dataLoadedOut <- reactive({
        validate(need(subsetData(), "No data loaded"))
        # Remove samples containing NA from data
        stats::na.omit(subsetData())
      })


      output$dichotomous_inputs <- renderUI({
        req(input$dataType == "dichotomous")
        column(
          12,
          selectInput(
            inputId = ns("dichotomous_response"),
            label = "Select Response Column:",
            choices = colnames(initialData()), # Dynamically populate based on loaded data
            selected = NULL
          ),
          selectInput(
            inputId = ns("dichotomous_samples"),
            label = "Select Samples Column:",
            choices = colnames(initialData()),
            selected = NULL
          )
        )
      })

      output$continuous_summary_inputs <- renderUI({
        req(input$dataType == "continuous-summary")
        column(
          12,
          selectInput(
            inputId = ns("summary_response"),
            label = "Select Response Column:",
            choices = colnames(initialData()),
            selected = NULL
          ),
          selectInput(
            inputId = ns("summary_samples"),
            label = "Select Samples Column:",
            choices = colnames(initialData()),
            selected = NULL
          ),
          selectInput(
            inputId = ns("summary_sd"),
            label = "Select Standard Deviation Column:",
            choices = colnames(initialData()),
            selected = NULL
          )
        )
      })

      output$continuous_full_inputs <- renderUI({
        req(input$dataType == "continuous")
        column(
          6,
          selectizeInput(
            inputId = ns("continuous_response"),
            label = "Select Response Columns:",
            choices = colnames(initialData()),
            selected = NULL,
            multiple = TRUE,
            options = list(
              maxOptions = 1000,
              placeholder = "Select response columns",
              plugins = list("remove_button")
            )
          )
          # selectInput(
          #   inputId = ns("continuous_response"),
          #   label = "Select Response Column:",
          #   choices = colnames(initialData()),
          #   selected = NULL,
          #   multiple = FALSE
          # )
        )
      })

      selectedResponse <- reactive({
        outcome <- input$dataType
        if (outcome == "dichotomous") {
          input$dichotomous_response
        } else if (outcome == "continuous-summary") {
          input$summary_response
        } else if (outcome == "continuous") {
          input$continuous_response
        } else {
          NULL
        }
      })

      samples <- reactive({
        outcome <- input$dataType
        if (outcome == "dichotomous") {
          input$dichotomous_samples
        } else if (outcome == "continuous-summary") {
          input$summary_samples
        } else if (outcome == "continuous") {
          NULL
        } else {
          NULL
        }
      })

      std_dev <- reactive({
        outcome <- input$dataType
        if (outcome == "continuous-summary") {
          input$summary_sd
        } else {
          NULL
        }
      })
      ## 3. Select response(s)
      observe({
        req(initialData())

        updateSelectInput(
          session = session, inputId = "continuous_response",
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
          if (nMissing > 0) {
            toastr_warning(paste0("There were", nMissing, "records excluded from the data due to missing values."), "Warning")
          },
          br(),
          toastr_info("You can select rows in the table that should be excluded from the analysis (outliers).", "Info")
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
          toastr_warning(message$outlier, "Warning")
        }
      })

      ## 6. Enable "Proceed to Fit Models" Button When All Inputs are Selected ##
      observe({
        input$dichotomous_response
        input$selectedResponseDosage
        input$dichotomous_samples
        input$summary_response
        input$summary_samples
        input$summary_sd
        input$continuous_response
        if (input$dataType == "dichotomous") {
          fileLoaded <- !is.null(inputData())
          dosageSelected <- !is.null(input$selectedResponseDosage) && input$selectedResponseDosage != ""
          responsesSelected <- !is.null(input$dichotomous_response) && !is.null(input$dichotomous_samples)
        } else if (input$dataType == "continuous-summary") {
          fileLoaded <- !is.null(inputData())
          dosageSelected <- !is.null(input$selectedResponseDosage) && input$selectedResponseDosage != ""
          responsesSelected <- !is.null(input$summary_response) && !is.null(input$summary_samples) &&
            !is.null(input$summary_sd)
        } else if (input$dataType == "continuous") {
          fileLoaded <- !is.null(inputData())
          dosageSelected <- !is.null(input$selectedResponseDosage) && input$selectedResponseDosage != ""
          responsesSelected <- !is.null(input$continuous_response) & length(input$continuous_response) > 0
        } else {
          fileLoaded <- FALSE
          dosageSelected <- FALSE
          responsesSelected <- FALSE
        }
        if (fileLoaded && dosageSelected && responsesSelected) {
          updateActionButton(session = session, "proceedToFit", disabled = FALSE)
        } else {
          updateActionButton(session = session, "proceedToFit", disabled = TRUE)
        }
      })

      proceedToFitClicked <- reactive(TRUE) %>% bindEvent(input$proceedToFit)

      output$dataOutlier <- renderUI({
        req(input$dataOverview_rows_selected)

        tags$div(
          style = "margin-top:60px; margin-bottom:20px;",
          tags$em(paste("You have selected", length(input$dataOverview_rows_selected), "outlier(s).")),
        )
      })

      ## 7. Output of dataServer
      list(
        loadedData = reactive(inputData()),
        selectedResponse = reactive(selectedResponse()),
        samples = reactive(samples()),
        std_dev = reactive(std_dev()),
        outcome = reactive(input$dataType),
        selectedResponseDosage = reactive(input$selectedResponseDosage),
        message = reactive(message),
        inFile = reactive(input$dataLoaded),
        proceedToFitClicked = proceedToFitClicked
      )
    }
  )
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
#' @importFrom shinytoastr toastr_error toastr_warning toastr_success
#' @export
readDataBMD <- function(fileName, filePath, separator, decimal) {
  require(shinytoastr)
  extension <- substring(
    fileName,
    first = (nchar(fileName) - 2),
    last = nchar(fileName)
  )

  tryCatch({
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
        tmpData <- read.table(filePath, header = TRUE, sep = separator, dec = decimal)
      }
    }

    return(tmpData)
  },
  error = function(err) {
    toastr_error(err, "Error")
  })
}