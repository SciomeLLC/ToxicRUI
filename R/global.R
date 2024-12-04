#' Global function - run before serverFunction and uiFunction
#'
#' @import ToxicR
#'
#' @return no return value
#'
#' @export
globalFunction <- function() {
  requireNamespace("dplyr")
}