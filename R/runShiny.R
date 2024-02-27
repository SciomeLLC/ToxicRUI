#' Run the shiny application
#' @param installDependencies logical, whether to install all dependent packages or not
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @return no return value
#' @importFrom shiny runApp
#' @importFrom stats update
#' @importFrom remotes dev_package_deps
#' @importFrom utils install.packages
#'
#' @export
runShiny <- function(installDependencies = FALSE, ...) {
  if (installDependencies) {
    ## (a) CRAN packages
    update(
      remotes::dev_package_deps(pkgdir = system.file("app",
        package = "ToxicRUI"
      ), dependencies = "Suggests")
    )
    ## (b) OA packages
    utils::install.packages("oaStyle", repos = "https://repos.openanalytics.eu/repo/public")
    utils::install.packages("efsaStyle", repos = "https://repos.openanalytics.eu/repo/public")
  }
  addResourcePath("www", system.file("app/www", package = "ToxicRUI"))
  shinyApp(
    ui = ui,
    server = serverFunction,
    onStart = globalFunction
  )
}
