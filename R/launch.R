#' Run the app in a web browser
#' 
#' @param inbrowser Launch the app inside the system's default browser (default TRUE) or not. 
#' 
#' @import shiny
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus box
#' @importFrom shinydashboardPlus dashboardHeader
#' @importFrom shinydashboardPlus dropdownBlock
#' @import ape
#' @import TreeSim
#' @import MorphSim
#' @import FossilSim
#'  
#' @examples 
#' if (interactive()) {
#'   launchMorphSimShiny()
#' }
#'
#' @export

launchMorphSimShiny <- function(inbrowser = TRUE) {
  appDir <- system.file("shinyApp", package = "MorphSimShiny")
  if (appDir == "") {
    stop("Could not find shinyApp. Try re-installing `MorphSimShiny`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = inbrowser)
}
