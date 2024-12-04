
#' Shiny module for loading data - server side
#'
#' @param id character, id of module
#'
#' @return reactive object, loaded data
#'
#' @import shiny
#' @importFrom shinyjs toggle hidden
#' @importFrom shinytoastr toastr_error toastr_warning toastr_success
#' @importFrom utils read.csv
#' @export
dataServer <- function(id) {
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


      ## 3. Select response(s)
      observe({
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
          if (nMissing > 0) {
            toastr_warning(paste0("There were", nMissing, "records excluded from the data due to missing values."), "Warning")
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
          toastr_warning(message$outlier, "Warning")
        }
      })
      ## 6. Enable "Proceed to Fit Models" Button When All Inputs are Selected ##
      observe({
        fileLoaded <- !is.null(input$dataLoaded) && input$dataLoaded$size > 0
        dosageSelected <- input$selectedResponseDosage != ""
        responsesSelected <- length(input$selectedResponse) > 0 && any(input$selectedResponse != "")

        if (fileLoaded && dosageSelected && responsesSelected) {
          shinyjs::enable("proceedToFit")
        } else {
          shinyjs::disable("proceedToFit")
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

      ## 6. Output of dataServer
      list(
        loadedData = reactive(inputData()),
        selectedResponse = reactive(input$selectedResponse),
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