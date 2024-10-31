launchMorphoSimShiny <- function(inbrowser = TRUE) {
  appDir <- system.file("shinyApp", package = "MorphoSimShiny")
  if (appDir == "") {
    stop("Could not find shinyApp. Try re-installing `MorphoSimShiny`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = inbrowser)
}
