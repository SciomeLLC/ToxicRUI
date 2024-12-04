
#' Shiny module for model fitting - server side
#'
#' @param id character. id of module
#' @param loadedData data frame, data after subsetting
#' @param message list, messages from data loading tab for report
#' @param selectedResponse character, selected responses
#' @param selectedResponseDosage character, selected dosage
#' @param inFile reactive, input file
#' @return list with fitted models and analysis data
#'
#' @import shiny
#' @importFrom shinyBS bsCollapsePanel
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_col hot_to_r
#' @importFrom shinyFeedback feedbackWarning
#' @importFrom magrittr %>%
#' @importFrom utils packageVersion
#' @importFrom ggplotify as.ggplot
#' @importFrom mailR send.mail
#' @importFrom stats na.omit
#' @importFrom bayesplot mcmc_trace
#' @importFrom shinycssloaders withSpinner
#'
#' @export
fitServer <- function(id, loadedData, message, selectedResponse, selectedResponseDosage, inFile) {
  require(shinyBS)
  require(shinytoastr)
  moduleServer(
    id,
    function(input, output, session, res) {
      ns <- session$ns
      proceedToResultsClicked <- reactiveVal(FALSE)
      # Enable/Disable the submit button based on selection
      proceedToResultsClicked <- reactive(TRUE) %>% bindEvent(input$submitDoseResponse)
      analysis_result <- eventReactive(input$submitDoseResponse, {
        shinyjs::disable("submitDoseResponse")
        command_str_list <- list()
        fit_results_list <- list()
        mcmc_results_list <- list()
        average_results_list <- list()
        withProgress(message = "Running dose-response analysis...", value = 0, {
          num_resp <- length(selectedResponse())
          for (i in seq_along(selectedResponse())) {
            resp <- selectedResponse()[i]
            incProgress(1 / num_resp, message = paste0("Running dose-response analysis for ", resp))
            dosage <- selectedResponseDosage()
            D <- loadedData()[[dosage]]
            Y <- loadedData()[[resp]]

            modelling_type <- input$average_or_fit
            fit_type <- input$fit_type
            bmr_type <- input$bmr_type
            bmr <- input$bmr
            alpha <- input$alpha
            samples <- input$samples
            burnin <- input$burnin
            outcome_type <- input$outcome_type
            seed <- input$seed
            fileInfo <- inFile()

            fit_res <- NULL
            mcmc_res <- NULL
            average_res <- NULL
            command_str <- NULL

            if (modelling_type == "fit") {
              model_type <- input$model_type
              ewald <- input$ewald
              distribution <- input$distribution

              fit_res <- tryCatch({
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
                toastr_error(e$message, "Error")
              }, warning = function(e) {
                toastr_warning(e$message, "Warning")
              })

              # Construct command string for reproducibility
              if (outcome_type == "continuous") {
                command_str <- paste0(
                  "library(ToxicR)\n",
                  "library(ToxicRUI)\n",
                  "fileData <- readDataBMD(\n",
                  "  fileName='", fileInfo$name, "', \n",
                  "  filePath='", fileInfo$datapath, "', \n",
                  "  separator=',', \n",
                  "  decimal='.'\n",
                  ")\n",
                  "D <- fileData[['", dosage, "']]\n",
                  "Y <- fileData[['", resp, "']]\n",
                  "fitResults <- single_continuous_fit(\n",
                  "  D = D, \n",
                  "  Y = Y, \n",
                  "  model_type = '", model_type, "', \n",
                  "  fit_type = '", fit_type, "', \n",
                  "  BMR_TYPE = '", bmr_type, "', \n",
                  "  BMR = ", bmr, ", \n",
                  "  distribution = '", distribution, "', \n",
                  "  alpha = ", alpha, ", \n",
                  "  burnin = ", burnin, ", \n",
                  "  samples = ", samples, ", \n",
                  "  ewald = ", ewald, ", \n",
                  "  seed = ", seed, "\n",
                  ")\n",
                  "plot(fitResults)\n",
                  "summary(fitResults)\n"
                )
              } else {
                command_str <- paste0(
                  "library(ToxicR)\n",
                  "library(ToxicRUI)\n",
                  "fileData <- readDataBMD(\n",
                  "  fileName='", fileInfo$name, "', \n",
                  "  filePath='", fileInfo$datapath, "', \n",
                  "  separator=',', \n",
                  "  decimal='.'\n",
                  ")\n",
                  "D <- fileData[['", dosage, "']]\n",
                  "Y <- fileData[['", resp, "']]\n",
                  "fitResults <- single_dichotomous_fit(\n",
                  "  D = D, \n",
                  "  Y = Y, \n",
                  "  model_type = '", model_type, "', \n",
                  "  fit_type = '", fit_type, "', \n",
                  "  BMR_TYPE = '", bmr_type, "', \n",
                  "  BMR = ", bmr, ", \n",
                  "  distribution = '", distribution, "', \n",
                  "  alpha = ", alpha, ", \n",
                  "  burnin = ", burnin, ", \n",
                  "  samples = ", samples, ", \n",
                  "  ewald = ", ewald, ", \n",
                  "  seed = ", seed, "\n",
                  ")\n",
                  "plot(fitResults)\n",
                  "summary(fitResults)\n"
                )
              }

              # Handle MCMC results
              if (fit_type == "mcmc" && outcome_type == "continuous") {
                temp <- as.matrix(fit_res$mcmc_result$PARM_samples)
                if (distribution == "normal-ncv") {
                  colnames(temp) <- c(letters[1:(ncol(temp) - 2)], "Non-constant var", "log(sig2)")
                } else {
                  colnames(temp) <- c(letters[1:(ncol(temp) - 1)], "log(sig2)")
                }
                mcmc_res <- temp
              }
            }

            if (modelling_type == "average") {
              average_res <- tryCatch({
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
                toastr_error(e$message, "Error")
              }, warning = function(e) {
                toastr_warning(e$message, "Warning")
              })

              # Construct command string for average fitting
              if (outcome_type == "continuous") {
                command_str <- paste0(
                  "library(ToxicR)\n",
                  "library(ToxicRUI)\n",
                  "fileData <- readDataBMD(\n",
                  "  fileName='", fileInfo$name, "', \n",
                  "  filePath='", fileInfo$datapath, "', \n",
                  "  separator=',', \n",
                  "  decimal='.'\n",
                  ")\n",
                  "D <- fileData[['", dosage, "']]\n",
                  "Y <- fileData[['", resp, "']]\n",
                  "fitResults <- ma_continuous_fit(\n",
                  "  D = D, \n",
                  "  Y = Y, \n",
                  "  fit_type = '", fit_type, "', \n",
                  "  BMR_TYPE = '", bmr_type, "', \n",
                  "  BMR = ", bmr, ", \n",
                  "  alpha = ", alpha, ", \n",
                  "  burnin = ", burnin, ", \n",
                  "  samples = ", samples, ", \n",
                  "  seed = ", seed, "\n",
                  ")\n",
                  "plot(fitResults)\n",
                  "summary(fitResults)\n"
                )
              } else {
                command_str <- paste0(
                  "library(ToxicR)\n",
                  "library(ToxicRUI)\n",
                  "fileData <- readDataBMD(\n",
                  "  fileName='", fileInfo$name, "', \n",
                  "  filePath='", fileInfo$datapath, "', \n",
                  "  separator=',', \n",
                  "  decimal='.'\n",
                  ")\n",
                  "D <- fileData[['", dosage, "']]\n",
                  "Y <- fileData[['", resp, "']]\n",
                  "fitResults <- ma_dichotomous_fit(\n",
                  "  D = D, \n",
                  "  Y = Y, \n",
                  "  fit_type = '", fit_type, "', \n",
                  "  BMR_TYPE = '", bmr_type, "', \n",
                  "  BMR = ", bmr, ", \n",
                  "  alpha = ", alpha, ", \n",
                  "  burnin = ", burnin, ", \n",
                  "  samples = ", samples, ", \n",
                  "  seed = ", seed, "\n",
                  ")\n",
                  "plot(fitResults)\n",
                  "summary(fitResults)\n"
                )
              }
            }

            # Store results in lists
            command_str_list[[resp]] <- command_str
            fit_results_list[[resp]] <- fit_res
            mcmc_results_list[[resp]] <- mcmc_res
            average_results_list[[resp]] <- average_res
          }
        })

        shinyjs::enable("submitDoseResponse")
        list(
          command_str = command_str_list,
          fit_results = fit_results_list,
          mcmc_results = mcmc_results_list,
          average_result = average_results_list
        )
      }, ignoreNULL = FALSE)

      list(
        analysis_result = analysis_result,
        proceedToResultsClicked = proceedToResultsClicked
      )
    }
  )
}