
#' Shiny module for displaying results - server side
#'
#' @param id character. id of module
#' @param fit_result analysis results
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
resultsServer <- function(id, analysis_result) {
  require(shinyBS)
  require(shinytoastr)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      generatePanels <- function(analysis_result) {
        panels <- list()
        cmds <- analysis_result()$command_str
        fits <- analysis_result()$fit_results
        avgs <- analysis_result()$average_result
        mcmcs <- analysis_result()$mcmc_results
        # Iterate over each response to create its accordion panel
        if (length(avgs) == 0) {
          for (resp in names(cmds)) {
            # Define unique output IDs for each response
            cmd_id <- paste0("commandText_", resp)
            res_id <- paste0("results_", resp)
            plot_id <- paste0("plotData_", resp)
            trace_id <- paste0("tracePlot_", resp)

            # Create output placeholders
            output[[cmd_id]] <- renderText({
              cmds[[resp]]
            })

            output[[res_id]] <- renderPrint({
              fit_res <- fits[[resp]]
              if (!is.null(fit_res)) {
                if (is.list(fit_res) && !is.null(fit_res$error)) {
                  cat("Error:", fit_res$error)
                } else {
                  summary(fit_res)
                }
              } else {
                cat("No fit results available.")
              }
            })

            output[[plot_id]] <- renderPlot({
              fit_res <- fits[[resp]]
              if (!is.null(fit_res)) {
                plot(fit_res)
              } else {
                plot.new()
                text(0.5, 0.5, "No plot available.", col = "blue")
              }
            })

            output[[trace_id]] <- renderPlot({
              mcmc_res <- mcmcs[[resp]]
              if (!is.null(mcmc_res)) {
                bayesplot::mcmc_trace(mcmc_res)
              } else {
                plot.new()
                text(0.5, 0.5, "No MCMC trace available.", col = "blue")
              }
            })

            # Create the accordion panel for the current response
            panels[[resp]] <- bsCollapsePanel(
              title = paste("Response:", resp),
              tags$h4("Command Output:"),
              verbatimTextOutput(ns(cmd_id)),

              tags$h4("Model Summary:"),
              verbatimTextOutput(ns(res_id)),

              tags$h4("Fit Plot:"),
              plotOutput(ns(plot_id)) %>% withSpinner(),

              tags$h4("MCMC Trace Plot:"),
              plotOutput(ns(trace_id)) %>% withSpinner(),
              value = paste0("panel_", resp) # Unique value attribute
            )
          }
        }

        # Handle Average Models if needed
        if (length(avgs) > 0) {
          for (resp in names(avgs)) {
            # Define unique output IDs for average responses
            avg_resp <- paste0(resp, " (Average)")
            cmd_id <- paste0("commandText_avg_", resp)
            res_id <- paste0("results_avg_", resp)
            plot_id <- paste0("plotData_avg_", resp)
            trace_id <- paste0("tracePlot_avg_", resp) # May not be applicable for averages

            # Create output placeholders
            output[[cmd_id]] <- renderText({
              cmds[[resp]]
            })

            output[[res_id]] <- renderPrint({
              avg_res <- avgs[[resp]]
              if (!is.null(avg_res)) {
                if (is.list(avg_res) && !is.null(avg_res$error)) {
                  cat("Error:", avg_res$error)
                } else {
                  summary(avg_res)
                }
              } else {
                cat("No average fit results available.")
              }
            })

            output[[plot_id]] <- renderPlot({
              avg_res <- avgs[[resp]]
              if (!is.null(avg_res)) {
                plot(avg_res)
              } else {
                plot.new()
                text(0.5, 0.5, "No average plot available.", col = "blue")
              }
            })

            output[[trace_id]] <- renderPlot({
              # Assuming average models do not produce MCMC traces
              plot.new()
              text(0.5, 0.5, "No MCMC trace available for average models.", col = "blue")
            })

            # Create the accordion panel for the average response
            panels[[resp]] <- bsCollapsePanel(
              title = paste("Response:", avg_resp),
              tags$h4("Command Output:"),
              verbatimTextOutput(ns(cmd_id)),
              tags$h4("Model Summary:"),
              verbatimTextOutput(ns(res_id)),
              tags$h4("Fit Plot:"),
              plotOutput(ns(plot_id)) %>% withSpinner(),
              tags$h4("MCMC Trace Plot:"),
              plotOutput(ns(trace_id)) %>% withSpinner(),
              value = paste0("panel_avg_", resp) # Unique value attribute
            )
          }
        }
        panels
      }
      output$resultsDisplay <- renderUI({
        req(analysis_result())
        cmds <- analysis_result()$command_str
        fits <- analysis_result()$fit_results
        avgs <- analysis_result()$average_result
        mcmcs <- analysis_result()$mcmc_results

        if (length(cmds) == 0) {
          tags$p("No results to display. Please run the analysis.")
        } else {
          panels <- generatePanels(analysis_result())

          # Ensure that panels list is not empty
          if (length(panels) == 0) {
            tags$p("No results to display. Please run the analysis.")
          } else {
            # Validate that all panels have 'value' attributes
            valid_panels <- all(
              sapply(
                panels,
                function(panel) {
                  !is.null(panel$attribs$value)
                }
              ))
            if (!valid_panels) {
              toastr_error("Error", "Dose-Response Analysis Error!")
              tags$p("Error: One or more panels are missing the 'value' attribute.")
            } else {
              # Collect panel values
              panel_values <- sapply(panels, function(panel) panel$attribs$value)
              # Determine the first valid 'open' panel
              if (length(panel_values) > 0 && !is.null(panel_values[1])) {
                open_panel <- panel_values[1]
              } else {
                open_panel <- NULL
              }
              if (!is.null(open_panel) && open_panel %in% panel_values) {
                do.call(bsCollapse, c(
                  list(
                    id = ns("resultsAccordion"),
                    multiple = TRUE,
                    open = open_panel
                  ),
                  panels
                ))
              } else {
                bsCollapse(
                  id = ns("resultsAccordion"),
                  multiple = TRUE,
                  panels
                )
              }
            }
          }
        }
      })
    }
  )
}