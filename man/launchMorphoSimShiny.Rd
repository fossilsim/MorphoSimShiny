% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launch.R
\name{launchMorphoSimShiny}
\alias{launchMorphoSimShiny}
\title{Run the app in a web browser}
\usage{
launchMorphoSimShiny(inbrowser = TRUE)
}
\arguments{
\item{inbrowser}{Launch the app inside the system's default browser or not.}
}
\value{
No return value, called for side effects.
}
\description{
This the function used to launch the shiny app.
}
\examples{
if (interactive()) {
  launchMorphoSimShiny()
}

}
