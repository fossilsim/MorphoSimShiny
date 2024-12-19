# source the backbone functions
source("content.R")

# Define UI
ui <- shinydashboard::dashboardPage(
  skin = "black",
  header = shinydashboardPlus::dashboardHeader(
    title = "MorphoSim Shiny",
    # this is for the Ui display
    leftUi = shiny::tagList(
      # Controls for # of species
      shinydashboardPlus::dropdownBlock(
        id = "controls_species",
        title = "Tree Parameters",
        icon = icon("hippo"),
        badgeStatus = NULL,
        
        shiny::numericInput(
          inputId = "n",
          label = "Number of species",
          value = 10,
          min = 2,
          max = 20,
          step = 1,
        ),
        helpText("The number of extant tips to reach at which the simulation is stopped"),
        
      shiny::sliderInput(
        inputId = "b",
        label = "Speciation rate",
        value = 1,
        min = 0.1,
        max = 5
      ),
      helpText("Rate at which lineages are diverging"),
      
      shiny::sliderInput(
        inputId = "d",
        label = "Extinction rate",
        value = 0.5,
        min = 0,
        max = 5,
        step = 0.1
      ),
      helpText("Rate at which lineages are going extinct")
    ),
      
      # dropdown for trait parameters
      shinydashboaardPlus::dropdownBlock(
        id = "controls_trait_params",
        title = "Trait Parameters",
        icon = icon("hashtag"),
        badgeStatus = NULL,
        
        shiny::numericInput(
          inputId = "l",
          label = "Number of traits",
          value = 5,
          min = 1,
          max = 20
        ),
        
        shiny::numericInput(
          inputId = "k",
          label = "Character states",
          value = 2,
          min = 2,
          max = 8
        ),
        helpText("The number of different character states for traits")
      ),
      
      # Clock Rate dropdown
      shinydashboardPlus::dropdownBlock(
        id = "controls_clock_rate",
        title = "Clock Rate",
        icon = icon("clock"),
        badgeStatus = NULL,
        
        shiny::sliderInput(
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
      shiny::actionButton("goButton", "Start Simulation"),
      
      # Post plotting Character slider
      shinydashboardPlus::dropdownBlock(
        id = "Plotted Characters",
        title = "Plotted Characters",
        icon = icon("sliders-h"),
        badgeStatus = NULL,
        
      shiny::numericInput(
        inputId = "s",
        label = "Shown Character",
        value = 1,
        min = 1,
        max = 20
      )
    ),

    # for a user specific tree this breaks currently
    shinydashboardPlus::dropdownBlock(
      id = "controls_newick_tree",
      title = "User specific Tree",
      icon = icon("tree"),
      badgeStatus = NULL,
      
      shiny::textAreaInput(
        inputId = "newickTree",
        label = "Enter Newick String for Tree",
        value = "",
        rows = 3,
        placeholder = "Enter Newick string here"
      ),
      helpText("If empty, a random tree will be generated.")
    ))
  ),
  
  shinydashboard::dashboardSidebar(disable = TRUE),
    # for the main plots tree first matrix second
  shinydashboard::dashboardBody(
    shiny::fluidRow(
      shiny::uiOutput("paramWarning"),
      box(
        title = "Simulated Phylogeny",
        status = NULL,
        solidHeader = TRUE,
        width = 8,
        plotOutput(outputId = "plot1")
      ),
      shinydashboardPlus::box(
        title = "Character Matrix",
        status = NULL,
        solidHeader = TRUE,
        width = 4,
        plotOutput(outputId = "plot2")
      ),
      shiny::fluidRow(
        shinydashboardPlus::box(
          title = "Simulation Details",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          uiOutput("simulationInfo")
        )
      )
    ) 
  )
)


# Server Logic
server <- function(input, output, session) {
  
  # reactiveVal to store the data
  savedData <- shiny::reactiveVal(NULL)
  
  
  # Run the simulation and save data upon click of the button to rule them all
  shiny::observeEvent(input$goButton, {
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
      plot(data, timetree = T, trait = input$s, br.rates = input$r)
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
  output$plot2 <- shiny::renderPlot({
    data <- savedData()
    if (!is.null(data)) {
      shiny.grid(data, l = input$s)
    } else {
      plot(NA, type = "n", xlim = c(0, 5), ylim = c(0, 3), ann = FALSE, bty = "n", xaxt = "n", yaxt = "n")
      text(x = 2.5, y = 1.5, labels = "No data to display", cex = 1.5, col = "#800020")
    }
  })
  shiny::observeEvent(input$l, {
    shiny::updateNumericInput(session, "s", max = input$l)
  })
  
  output$simulationInfo <- shiny::renderUI({
    data <- savedData()
    if (!is.null(data)) {
      shiny::tagList(
        shiny::h4("Simulation Tree"),
        shiny::textOutput("simDetails"),
        shiny::textAreaInput(
          inputId = "resultText",
          label = "Newick String",
          value = paste(write.tree(data$tree), \n,
                       input$n ,
                        sep = ""),
          rows = 5
        )
      )
    } else {
      shiny::tagList(
        shiny::h4("No Simulation Run Yet"),
        shiny::p("Run the simulation to see results.")
      )
    }
  })
  # Reactive value to track parameter changes
  paramsChanged <- shiny::reactiveVal(FALSE)
  
  # Observe changes in parameters
  shiny::observe({
    # List of input parameters to track
    paramInputs <- list(input$n, input$b, input$d, input$l, input$k, input$r, input$newickTree)
    
    # If any parameter changes, set the flag to TRUE
    shiny::paramsChanged(TRUE)
  })
  
  # Reset the flag when the simulation button is clicked
  shiny::observeEvent(input$goButton, {
    shiny::paramsChanged(FALSE) # Parameters are now up-to-date
  })
  
  # Display a notification if parameters are changed but the simulation is not yet re-run
  output$paramWarning <- shiny::renderUI({
    if (shiny::paramsChanged()) {
      shiny::div(
        class = "alert alert-warning",
        role = "alert",
        strong("Notice: "),
        "You have changed parameters but have not run a new simulation yet."
      )
    }
  })
}


# Run the Shiny App
shinyApp(ui = ui, server = server)
