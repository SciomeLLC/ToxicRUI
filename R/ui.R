#' Main UI function
#'
#' @return no return value
#'
#' @import shiny
#' @import shinyjs
#' @import shinycssloaders
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinytoastr useToastr
#' @importFrom shinytoastr toastr_success toastr_error
#' @export
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    text = "shinyjs.test = function() { location.reload(); }", functions = "test"),
  shinyjs::extendShinyjs(
    text = "shinyjs.pageCol = function(params){$('body').css('background', params);}", functions = "pageCol"),
  shinyjs::extendShinyjs(
    text = "shinyjs.scrollTo = function(params){$('html, body').animate({scrollTop: $(params['id']).offset().top}, 2000);}", functions = "scrollTo"),
  useShinyFeedback(),
  useToastr(),
  fluidRow(
    column(
      8,
      titlePanel(
        title = div(
          img(src = "www/SCIOME-logo-144.png", float = "top", height = "30px", hspace = "50px"),
          img(
            src = "www/niehs-logo.svg",
            height = "30px", hspace = "50px"),
          "ToxicR Data Analysis"
        ),
        windowTitle = "ToxicR Data Analysis"
      )
    ),
    column(
      4,
    )
  ),
  br(),
  tabsetPanel(
    id = "tabs",
    dataUI(id = "data"),
    fitUI(id = "fit"),
    resultsUI(id = "results")
  )
)
