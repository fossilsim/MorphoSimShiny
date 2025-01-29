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
        title = "Tree",
        icon = icon("hippo"),
        badgeStatus = NULL,

        shiny::numericInput(
          inputId = "n",
          label = "Number of species",
          value = 10,
          min = 2,
          max = 20,
          step = 1
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



      # dropdown for trait parameters
    shinydashboardPlus::dropdownBlock(
      id = "controls_trait_partitions",
      title = "Partitions",
      icon = icon("hashtag"),
      badgeStatus = NULL,

      shiny::numericInput(
        inputId = "l",
        label = "Number of partitions",
        value = 1,
        min = 1,
        max = 20
      ),
      helpText("The number of partitions for the trait evolution")
    ),

    # dropdown for dynamic trait inputs based on number of partitions
    shinydashboardPlus::dropdownBlock(
      id = "controls_trait_params",
      title = "Traits",
      icon = icon("hashtag"),
      badgeStatus = NULL,

      uiOutput("group_inputs")  # Render the dynamic trait inputs here
    ),


    shinydashboardPlus::dropdownBlock(
      id = "controls_trait_states",
      title = "States",
      icon = icon("hashtag"),
      badgeStatus = NULL,

      uiOutput("state_inputs")  # Render the dynamic state inputs here
    ),


    # Variable Coding - Yes/No checkbox
    shinydashboardPlus::dropdownBlock(
      id = "controls_variable_coding",
      title = "Variable Coding",
      icon = icon("cogs"),
      badgeStatus = NULL,

      shiny::checkboxInput(
        inputId = "variableCoding",
        label = "Simulate varying traits only",
        value = FALSE
      )
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
        label = "Show Trait",
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
      shinydashboardPlus::box(
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

      div(
        style = "margin-left: 20px;",  # Apply style to the div
        checkboxInput("keepTreeFixed", "Fix tree", value = FALSE)
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
# Dynamically generate dropdowns for the number of people per group
  output$group_inputs <- renderUI({
    req(input$l) # Ensure num_groups is available
    num_groups <- as.numeric(input$l)

    # Generate dropdowns for each group
    lapply(1:num_groups, function(i) {
      selectInput(
        inputId = paste0("group_", i),
        label = paste("Number of traits in partition", i),
        choices = 1:100,
        selected = 1
      )

    })
  })

  output$state_inputs <- renderUI({
    req(input$l)  # Ensure number of partitions is available
    num_partitions <- as.numeric(input$l)

    # Generate dropdowns for the number of states per partition
    lapply(1:num_partitions, function(i) {
      selectInput(
        inputId = paste0("state_", i),
        label = paste("Number of states in partition", i),
        choices = 2:10,  # Assuming the states range from 1 to 10
        selected = 1
      )
    })
  })





  # reactiveVal to store the data
  savedData <- shiny::reactiveVal(NULL)
  currentTree <- shiny::reactiveVal(NULL)

  # Run the simulation and save data upon click of the button to rule them all
  shiny::observeEvent(input$goButton, {
    req(input$b > input$d)  # Ensure speciation rate is greater than extinction rate

    if (is.null(currentTree()) || !input$keepTreeFixed) {
      tree <- parseNewickTree(input$newickTree)
      if (is.null(tree)) {
        tree <- TreeSim::sim.bd.taxa(
          n = input$n,
          numbsim = 1,
          lambda = input$b,
          mu = input$d,
          frac = 1
        )[[1]]
      }
      currentTree(tree)  # Update reactive value
    } else {
      tree <- currentTree()  # Use existing reactive value
    }

   # get information for partitions

    # Create a vector of number of traits for each partition (from the dynamic inputs)
    num_traits_vector <- as.numeric(unlist(sapply(1:as.numeric(input$l), function(i) {
    input[[paste0("group_", i)]]
    })))

    # Create a vector of states for each partition (from the dynamic inputs)
    num_states_vector <- sapply(1:as.numeric(input$l), function(i) {
      as.numeric(input[[paste0("state_", i)]])  # Access the number of states for each partition
    })


    # Access the variable coding selection (Yes/No)
    variable_coding <- input$variableCoding

    # Generate data with the tree
    data <- MorphoSim::sim.morpho(
      time.tree = tree,
      br.rates = input$r,
      k = num_states_vector,
      trait.num = sum(num_traits_vector),
      partition = num_traits_vector,
      variable = variable_coding
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
          value = paste(ape::write.tree(data$tree),
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
    paramsChanged(TRUE)
  })

  # Reset the flag when the simulation button is clicked
  shiny::observeEvent(input$goButton, {
    paramsChanged(FALSE) # Parameters are now up-to-date
  })

  # Display a notification if parameters are changed but the simulation is not yet re-run
  output$paramWarning <- shiny::renderUI({
    if (paramsChanged()) {
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
