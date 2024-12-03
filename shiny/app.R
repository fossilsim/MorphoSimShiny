# Load necessary packages
require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
require(dplyr)
require(FossilSim)
require(phytools)
require(TreeSim)
require(ggtree2)
devtools::install_github("https://github.com/fossilsim/morphosim")
library(MorphoSim)

# source the backbone functions
source("content.R")

# Define UI
ui <- shinydashboard::dashboardPage(
  skin = "black",
  header = shinydashboardPlus::dashboardHeader(
    title = "MorphoSim Shiny",
    # this is for the Ui display
    leftUi = tagList(
      # Controls for # of species
      dropdownBlock(
        id = "controls_species",
        title = "Species",
        icon = icon("hippo"),
        badgeStatus = NULL,
        
        sliderInput(
          inputId = "n",
          label = "Number of species",
          value = 10,
          min = 2,
          max = 20,
          step = 1
        ),
        helpText("The number of species to reach at which the simulation is stopped")
      ),
      
      # origination/extion dropdown
      dropdownBlock(
        id = "controls_bir_death",
        title = "Birth/Death Rates",
        icon = icon("cake-candles"),
        badgeStatus = NULL,
        
        sliderInput(
          inputId = "b",
          label = "Speciation rate",
          value = 1,
          min = 0.1,
          max = 5,
          step = 0.1
        ),
        helpText("Rate at which species are diverging"),
        
        sliderInput(
          inputId = "d",
          label = "Extinction rate",
          value = 0.5,
          min = 0,
          max = 5,
          step = 0.1
        ),
        helpText("Rate at which species are going extinct")
      ),
      
      # dropdown for trait parameters
      dropdownBlock(
        id = "controls_trait_params",
        title = "Trait Parameters",
        icon = icon("hashtag"),
        badgeStatus = NULL,
        
        sliderInput(
          inputId = "l",
          label = "Number of traits",
          value = 5,
          min = 1,
          max = 20,
          step = 1
        ),
        
        sliderInput(
          inputId = "k",
          label = "Character states",
          value = 2,
          min = 2,
          max = 8,
          step = 1
        ),
        helpText("The number of different character states for traits")
      ),
      
      # Clock Rate dropdown
      dropdownBlock(
        id = "controls_clock_rate",
        title = "Clock Rate",
        icon = icon("clock"),
        badgeStatus = NULL,
        
        sliderInput(
          inputId = "r",
          label = "Clock rate",
          value = 0.2,
          min = 0.1,
          max = 1,
          step = 0.1
        ),
        helpText("Clock rate for trait evolution")
      ),
      
      #  Simulation button
      actionButton("goButton", "Start Simulation"),
      
      # Post plotting Character slider
      dropdownBlock(
        id = "Plotted Characters",
        title = "Plotted Characters",
        icon = icon("sliders-h"),
        badgeStatus = NULL,
        
      sliderInput(
        inputId = "s",
        label = "Shown Character",
        value = 1,
        min = 1,
        max = 20,
        step = 1
      )
    ),

    # for a user specific tree this breaks currently
    dropdownBlock(
      id = "controls_newick_tree",
      title = "User specific Tree",
      icon = icon("tree"),
      badgeStatus = NULL,
      
      textAreaInput(
        inputId = "newickTree",
        label = "Enter Newick String for Tree",
        value = "",
        rows = 3,
        placeholder = "Enter Newick string here"
      ),
      helpText("If empty, a random tree will be generated.")
    ))
  ),
  
  dashboardSidebar(disable = TRUE),
    # for the main plots tree first matrix second
  dashboardBody(
    fluidRow(
      box(
        title = "Simulated Phylogeny",
        status = NULL,
        solidHeader = TRUE,
        width = 8,
        plotOutput(outputId = "plot1")
      ),
      box(
        title = "Character Matrix",
        status = NULL,
        solidHeader = TRUE,
        width = 4,
        plotOutput(outputId = "plot2")
      )
    )
  )
)


# Server Logic
server <- function(input, output, session) {
  
  # reactiveVal to store the data
  savedData <- reactiveVal(NULL)
  

  # User specific tree stuff
  parseNewickTree <- function(newickString) {
    if (newickString != "") {
      tryCatch({
        tree <- ape::read.tree(text = newickString)
        return(tree)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Invalid Newick string. Please check the format.")
        ))
        return(NULL)
      })
    }
    return(NULL)
  }
  
  # Run the simulation and save data upon click of the button to rule them all
  observeEvent(input$goButton, {
    req(input$b > input$d)  # Ensure speciation rate is greater than extinction rate
    
    # Check if Newick string is provided, if not generate tree
    tree <- parseNewickTree(input$newickTree)
    if (is.null(tree)) {
      # If no valid Newick string, generate random tree using sim.bd.taxa
      tree <- TreeSim::sim.bd.taxa(n = input$n, numbsim = 1, lambda = input$b, mu = input$d, frac = 1)[[1]]
    }
    
    # Generate data with the tree
    data <- sim.morpho.completeprocess(
      time.tree = tree, 
      br.rates = input$r,
      k = input$k, 
      trait.num = input$l
    )
    # Save the data for later 
    savedData(data)
  })
  
  # Render Phylo
  output$plot1 <- renderPlot({
    # Access the saved data
    data <- savedData()
    
    if (!is.null(data)) {
      # Replot the phylogeny
      plot.morpho(data, data$tree, show.tip.label = F, l = input$s)
    } else if (input$b < input$d){
      plot(NA, type = "n", xlim = c(0, 5), ylim = c(0, 3), ann = FALSE, bty = "n", xaxt = "n", yaxt = "n")
      text(x = 2.5, y = 1.5, labels = "Choose a speciation rate > extinction rate", cex = 1.5, col = "#800020")
    } else {
      # Display an error message if no data
      plot(NA, type = "n", xlim = c(0, 5), ylim = c(0, 3), ann = FALSE, bty = "n", xaxt = "n", yaxt = "n")
      text(x = 2.5, y = 1.5, labels = "Run the simulation", cex = 1.5, col = "#800020")
    }
  })
  
  # Render Character Matrix 
  output$plot2 <- renderPlot({
    data <- savedData()
    if (!is.null(data)) {
      plot.morpho.grid(data)
    } else {
      plot(NA, type = "n", xlim = c(0, 5), ylim = c(0, 3), ann = FALSE, bty = "n", xaxt = "n", yaxt = "n")
      text(x = 2.5, y = 1.5, labels = "No data to display", cex = 1.5, col = "#800020")
    }
  })
  observeEvent(input$l, {
    updateSliderInput(session, "s", max = input$l)
  })
}


# Run the Shiny App
shinyApp(ui = ui, server = server)
