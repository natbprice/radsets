#' Launches the shiny app
#'
#' @return shiny application object
#'
#' @examples 
#' \donttest{
#' launchApp()
#' }
#'
#' @importFrom magrittr %>% 
#' @import shiny
#' @import dplyr
#' @import circlize
#' @import RColorBrewer
#'
#' @export launchApp
launchApp <- function() {
  appDir <- system.file("shiny-app", package = "radsets")
  if (appDir == "") {
    stop("Could not find `shiny-app`. Try re-installing `radsets`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
