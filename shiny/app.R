library(BiocManager)
options(repos = BiocManager::repositories())

library(ape)
library(dplyr)
library(cowplot)
library(ggplot2)
library(ggtree)
library(magrittr)
library(phangorn)
library(stringr)
library(tibble)
library(tidyr)
library(dispRity)
library(FossilSim)
library(phytools)

source("construction_site.R")

ui <- fluidPage(pageWithSidebar(
  headerPanel = headerPanel("Simulating evolutionary traits along phylogenies"),
  
  sidebarPanel(
    sliderInput(
      inputId = "N",
      label = "Number of species",
      value = 10,
      min = 2,
      max = 20,
      step = 1
    ),
    
    helpText("The number of species to reach at which the simulation is stopped"),
    
    sliderInput(
      inputId = "b",
      label = "Speciation rate",
      value = 1,
      min = 0.05,
      max = 5,
      step = 0.05
    ),
    
    helpText("Rate at which species are diverging"),
    
    sliderInput(
      inputId = "d",
      label = "Death rate",
      value = 0.5,
      min = 0,
      max = 5,
      step = 0.05
    ),
    
    helpText("Rate at which species are going extinct"),
    
    sliderInput(
      inputId = "L",
      label = "Sequence length",
      value = 5,
      min = 1,
      max = 20,
      step = 1
    ),
    
    helpText("The length of the DNA sequence to simulate. Each position starts as blue in the ancestor, then can mutate to other states/colors. Mutations can re-enter previous states"),
    
    sliderInput(
      inputId = "c",
      label = "Character states",
      value = 4,
      min = 2,
      max = 20,
      step = 1
    ),
    
    helpText("The number of different character states the trait is able to switch between. Examples: 2 for simplicity, 4 for nucleotides, 20 for amino acids."),
    
    sliderInput(
      inputId = "u",
      label = "Mutation rate",
      value = 0.1,
      min = 0.01,
      max = 1,
      step = 0.01
    ),
    
    helpText("Mutation rate"),
    
    actionButton("goButton", "GO"),
    
  ),
  
  mainPanel =  mainPanel(
    
    plotOutput(outputId = 'viz1'),
    
    
    plotOutput(outputId = 'viz2')
    
  )))


#back end code and response to user input
server <- function(input, output) {
  rand <- eventReactive(input$goButton, {
    # return(list(N = input$N, L = input$L, u = input$u))
    simulate_treeseq(N = input$N, L = input$L, u = input$u, c = input$c, d = input$d, b = input$b)
  })
  
  output$viz1 <- renderPlot({
    s <- rand()
    plot_treeseq(s)
  })
  
  output$viz2 <- renderPlot({
    s <- rand()
    plot.BM(N)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
