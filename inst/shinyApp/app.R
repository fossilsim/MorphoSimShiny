# source the backbone functions
source("content.R")


#### UI #####
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
        icon = icon("diagram-project"),
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


    ## sampling
    shinydashboardPlus::dropdownBlock(
      id = "controls_sampling",
      title = "Sampling",
      icon = icon("hippo"),
      badgeStatus = NULL,

      shiny::sliderInput(
        inputId = "psi",
        label = "Fossil Sampling",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.1
      ),
      helpText("Extinct species sampling rate"),

      shiny::sliderInput(
        inputId = "rho",
        label = "Extant Sampling",
        value = 1,
        min = 0,
        max = 1,
        step = 0.1
      ),
      helpText("Extant species sampling rate")
    ),

    # Clock Rate dropdown
    shinydashboardPlus::dropdownBlock(
      id = "controls_clock_rate",
      title = "Clock",
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
      helpText("Clock rate for trait evolution using a strict clock model")
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
      title = "Morphological Model",
      icon = icon("cogs"),
      badgeStatus = NULL,

      shiny::checkboxInput(
        inputId = "variableCoding",
        label = "Simulate varying traits only (MkV)",
        value = FALSE
      ),
      shiny::checkboxInput(
        inputId = "useGamma",
        label ="Simulate rate variation (Gamma)",
        value = FALSE
        ),
      helpText("By default use the Mk model")
    ),

      #  Simulation button
      shiny::actionButton("goButton", "Start Simulation"),

      # Post plotting Character slider
      shinydashboardPlus::dropdownBlock(
        id = "Plotted Characters",
        title = "Plotting",
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
      title = "Provide a Tree",
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

  shinydashboard::dashboardSidebar(disable = TRUE, collapsed = T),
    # for the main plots tree first matrix second
  shinydashboard::dashboardBody(
    shiny::fluidRow(

      fluidRow(
        column(1,checkboxInput("keepTreeFixed", "Fix tree", value = FALSE), style = "padding-right: 5px;"),
        column(1, checkboxInput("fossils", "Show fossils", value = FALSE)),
        column(1, checkboxInput("reconstructed", "Show reconstructed tree", value = FALSE))
      ),


      shiny::uiOutput("paramWarning"),
      shinydashboardPlus::box(
        title = "Simulated Phylogeny",
        status = NULL,
        solidHeader = TRUE,
        width = 7,
        plotOutput(outputId = "plot1")
      ),
      shinydashboardPlus::box(
        title = "Character Matrix",
        status = NULL,
        solidHeader = TRUE,
        width = 5,
        plotOutput(outputId = "plot2")
      ),

    fluidRow(
      column(5, offset = 7,
    uiOutput("explanation_text"),
    uiOutput("dropdown_ui"),
    uiOutput("goButton2_ui"))),




      shinydashboardPlus::dropdownBlock(
        id = "Colorblind",
        title = "Colorblindness",
        icon = icon("glasses"),
        badgeStatus = NULL,

        shiny::selectInput(
          inputId = "cbType",
          label = "Select your condition:",
          choices = list(
            "None" = "none",
            "Protanopia (Red-Blind)" = "protanopia",
            "Deuteranopia (Green-Blind)" = "deuteranopia",
            "Tritanopia (Blue-Blind)" = "tritanopia"
          ),
          selected = "none"
        ),

        helpText("If you have a visual condition, selecting your condition might help with visibility")
      ),

      shiny::fluidRow(
        shinydashboardPlus::box(
          title = "Simulation Details",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          uiOutput("simulationInfo")
        )
      ),
      shiny::tags$script(HTML("
      Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
       navigator.clipboard.writeText(message).then(function() {
        alert('Newick string copied to clipboard!');
       }).catch(function(err) {
        console.error('Could not copy text: ', err);
        });
        });
      "))
    )
  )
)


##### Server ####
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
        selected = 2
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
        choices = 2:10,
        selected = 2
      )
    })
  })

  # reactiveVal to store the data
  savedData <- shiny::reactiveVal(NULL)
  currentTree <- shiny::reactiveVal(NULL)
  totalTraits <- shiny::reactiveVal(NULL)
  simulation_started <- reactiveVal(FALSE)


  # Run the simulation and save data upon click of the button to rule them all
  shiny::observeEvent(input$goButton, {

    buttonClicked <- reactiveVal(FALSE)

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

    f = FossilSim::sim.fossils.poisson(rate = input$psi,
                                       tree = tree,
                                       root.edge = F)

    # get information for partitions
    # Create a vector of number of traits for each partition (from the dynamic inputs)
    num_traits_vector <- as.numeric(unlist(sapply(1:as.numeric(input$l), function(i) {
      ifelse(is.null(input[[paste0("group_", i)]]), 2, input[[paste0("group_", i)]])
    })))

    num_states_vector <- as.numeric(unlist(sapply(1:as.numeric(input$l), function(i) {
      ifelse(is.null(input[[paste0("state_", i)]]), 2, input[[paste0("state_", i)]])
    })))



    totalTraits(sum(num_traits_vector))
    # Access the variable coding selection (Yes/No)
    variable_coding <- input$variableCoding

    acrv_value <- if (input$useGamma) "gamma" else NULL

    f_vale <- NULL
    if (!is.null(tree)) {
      f_vale <- FossilSim::sim.fossils.poisson(rate = input$psi, tree = tree, root.edge = FALSE)
    }

    # Generate data with the tree
    data <- MorphoSim::sim.morpho(
      time.tree = tree,
      br.rates = input$r,
      k = num_states_vector,
      trait.num = sum(num_traits_vector),
      partition = num_traits_vector,
      variable = variable_coding,
      ACRV = acrv_value,
      fossil = f_vale
    )
    # Save the data for later
    savedData(data)

    ## Create button for simulate missing data
    output$goButton2_ui <- renderUI({
      actionButton("goButton2", "Simulate Missing data")
  })

    output$explanation_text <- renderUI({
      tagList(
        span("The above simulation generated a complete matrix (i.e, there are no missing character traits).
              When gathering data from the fossil record however, we will often have a number of missing traits
             for different taxa. Here you can simulate missing data for you matrix according to a given probability",
             style = "font-size: 14px; color: gray;")
  )
    })

    ## options for missing data

    output$dropdown_ui <- renderUI({
      shiny::sliderInput(
        inputId = "missing",
        label = "Probability of missing data",
        value = 0.2,
        min = 0.0,
        max = 1,
        step = 0.01

      )
    })

  })

  # Render Phylo
  output$plot1 <- renderPlot({
    # Access the saved data
    data <- savedData()

    if (!is.null(data)) {
      shinyplot(
        data,
        timetree = TRUE,
        trait = input$s,
        cbType = input$cbType,
        fossil = if (isTRUE(input$fossils)) !is.null(data$fossil) else FALSE,
        root.edge = FALSE,
        reconstructed = isTRUE(input$reconstructed)
      )
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
      shiny.grid(data, l = input$s, cbType = input$cbType)
    } else {
      plot(NA, type = "n", xlim = c(0, 5), ylim = c(0, 3), ann = FALSE, bty = "n", xaxt = "n", yaxt = "n")
      text(x = 2.5, y = 1.5, labels = "No data to display", cex = 1.5, col = "#800020")
    }
  })
  shiny::observeEvent(totalTraits(), {
    shiny::updateNumericInput(session, "s", max = totalTraits())
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
        ),
        shiny::actionButton(
          inputId = "copyButton",
          label = "Copy",
          icon = icon("copy"),
          style = "margin-left: 10px;"
        )
      )
    } else {
      shiny::tagList(
        shiny::h4("No Simulation Run Yet"),
        shiny::p("Run the simulation to see results.")
      )
    }
  })


   ## code for sim.missing.data

  observeEvent(input$goButton2,{
   req(input$missing)
    # Only run the simulation if the button has been clicked
    data <- savedData()
    missing.data <- shiny.missing(data = data, missing = input$missing)
     output$plot2 <- renderPlot({
      shiny.grid(missing.data, l = input$s, cbType = input$cbType)

    })
    }
  )

  shiny::observeEvent(input$goButton, {  # Render Character Matrix
    output$plot2 <- shiny::renderPlot({
      data <- savedData()
      if (!is.null(data)) {
        shiny.grid(data, l = input$s, cbType = input$cbType)
      } else {
        plot(NA, type = "n", xlim = c(0, 5), ylim = c(0, 3), ann = FALSE, bty = "n", xaxt = "n", yaxt = "n")
        text(x = 2.5, y = 1.5, labels = "No data to display", cex = 1.5, col = "#800020")
      }
    })
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
 shiny::observeEvent(input$copyButton, {
  data <- savedData()
  if (!is.null(data)) {
    session$sendCustomMessage("copyToClipboard", ape::write.tree(data$tree))
  }
})

}


# Run the Shiny App
shinyApp(ui = ui, server = server)
