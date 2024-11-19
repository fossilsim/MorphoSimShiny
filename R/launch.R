#' Run the app in a web browser
#' 
#' @description 
#' This the function used to launch the shiny app.
#' 
#' @param inbrowser Launch the app inside the system's default browser or not.
#' @return No return value, called for side effects.
#' 
#' 
#' @import shiny
#' @import MorphoSim
#' @import FossilSim
#' @export

launchMorphoSimShiny <- function(inbrowser = TRUE) {
  appDir <- system.file("shinyApp", package = "MorphoSimShiny")
  if (appDir == "") {
    stop("Could not find shinyApp. Try re-installing `MorphoSimShiny`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = inbrowser)
}
