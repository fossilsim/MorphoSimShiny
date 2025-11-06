# source the backbone functions
source("inst/shinyApp/content.R")

#### UI ####
ui <- shinydashboard::dashboardPage(
  skin = "black",
  header = shinydashboardPlus::dashboardHeader(
    title = "MorphSim Shiny",
    leftUi = shiny::tagList(

      ##### Tree Controls ####
      shinydashboardPlus::dropdownBlock(
        id = "controls_species",
        title = "Tree",
        icon = icon("diagram-project"),
        badgeStatus = NULL,
        shiny::numericInput("n", "Number of species", value = 10, min = 2, max = 20, step = 1),
        helpText("The number of extant tips to reach at which the simulation is stopped"),
        shiny::sliderInput("b", "Speciation rate", value = 1, min = 0.1, max = 5),
        helpText("Rate at which lineages are diverging"),
        shiny::sliderInput("d", "Extinction rate", value = 0.5, min = 0, max = 5, step = 0.1),
        helpText("Rate at which lineages are going extinct")
      ),

      #### Sampling ####
      shinydashboardPlus::dropdownBlock(
        id = "controls_sampling",
        title = "Sampling",
        icon = icon("hippo"),
        badgeStatus = NULL,
        shiny::sliderInput("psi", "Fossil Sampling", value = 0.5, min = 0, max = 1, step = 0.1),
        helpText("Extinct species sampling probability"),
        shiny::sliderInput("rho", "Extant Sampling", value = 1, min = 0, max = 1, step = 0.1),
        helpText("Extant species sampling rate")
      ),

      ##### Clock ####
      shinydashboardPlus::dropdownBlock(
        id = "controls_clock_rate",
        title = "Clock",
        icon = icon("clock"),
        badgeStatus = NULL,
        shiny::sliderInput("r", "Clock rate", value = 0.2, min = 0.1, max = 1, step = 0.1),
        helpText("Clock rate for trait evolution using a strict clock model")
      ),

      ##### Trait partitions ####
      shinydashboardPlus::dropdownBlock(
        id = "controls_trait_partitions",
        title = "Partitions",
        icon = icon("hashtag"),
        badgeStatus = NULL,
        shiny::numericInput("l", "Number of partitions", value = 1, min = 1, max = 20),
        helpText("The number of partitions for the trait evolution")
      ),

      #### Dynamic trait UI ####
      shinydashboardPlus::dropdownBlock(
        id = "controls_trait_params",
        title = "Traits",
        icon = icon("hashtag"),
        badgeStatus = NULL,
        uiOutput("group_inputs")
      ),

      shinydashboardPlus::dropdownBlock(
        id = "controls_trait_states",
        title = "States",
        icon = icon("hashtag"),
        badgeStatus = NULL,
        uiOutput("state_inputs")
      ),

      #### Morphological model ####
      shinydashboardPlus::dropdownBlock(
        id = "controls_variable_coding",
        title = "Morphological Model",
        icon = icon("cogs"),
        badgeStatus = NULL,
        shiny::checkboxInput("variableCoding", "Simulate varying traits only (MkV)", value = FALSE),
        shiny::checkboxInput("useGamma", "Simulate rate variation (Gamma)", value = FALSE),
        helpText("By default use the Mk model")
      ),

      ##### Simulation ####
      shiny::actionButton("goButton", "Start Simulation"),

      ##### Plotting ####
      shinydashboardPlus::dropdownBlock(
        id = "Plotted Characters",
        title = "Plotting",
        icon = icon("sliders-h"),
        badgeStatus = NULL,
        shiny::numericInput("s", "Show Trait", value = 1, min = 1, max = 20)
      )
    )
  ),

  shinydashboard::dashboardSidebar(disable = TRUE, collapsed = TRUE),

  ##### Main Body ####
  shinydashboard::dashboardBody(
    # Remove spinner gears
    tags$head(
      tags$style(HTML("
        .fa-gears, .loading-icon {
          display: none !important;
        }
      "))
    ),

    shiny::fluidRow(
      column(2, checkboxInput("keepTreeFixed", "Fix tree", value = FALSE), style = "padding-left: 30px;"),
      column(2, checkboxInput("fossils", "Show samples", value = FALSE)),
      column(2, checkboxInput("reconstructed", "Show reconstructed tree", value = FALSE))
    ),

    uiOutput("paramWarning"),

    shiny::fluidRow(
      shinydashboardPlus::box(
        title = "Simulated Phylogeny", width = 7, solidHeader = TRUE,
        plotOutput("plot1", width = "100%")
      ),
      shinydashboardPlus::box(
        title = "Character Matrix", width = 5, solidHeader = TRUE,
        plotOutput("plot2", width = "100%")
      )
    ),

    shiny::fluidRow(
      column(
        5, offset = 7,
        uiOutput("explanation_text"),
        uiOutput("dropdown_ui"),
        uiOutput("goButton2_ui")
      )
    ),

    # Downloads
    fluidRow(
      style = "margin-top: 20px;",
      shinydashboardPlus::box(
        title = "Download Simulation Outputs",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        icon = icon("download"),

        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",  # adjust spacing here
          downloadButton("downloadTree", "Tree (.nwk)", class = "btn-sm btn-primary"),
          downloadButton("downloadReconstructedTree", "Reconstructed Tree (.nwk)", class = "btn-sm btn-primary"),
          downloadButton("downloadFossils", "Fossil Ages (.csv)", class = "btn-sm btn-primary"),
          downloadButton("downloadMatrix", "Trait Matrix (.csv)", class = "btn-sm btn-primary"),
          downloadButton("downloadReconstructedMatrix", "Reconstructed Matrix (.csv)", class = "btn-sm btn-primary")
        )
      )
    ),

    ##### Accessibility ####
    shinydashboardPlus::dropdownBlock(
      id = "Colorblind",
      title = "Colorblindness",
      icon = icon("glasses"),
      badgeStatus = NULL,
      shiny::selectInput(
        "cbType",
        "Select your condition:",
        choices = list(
          "None" = "none",
          "Protanopia (Red-Blind)" = "protanopia",
          "Deuteranopia (Green-Blind)" = "deuteranopia",
          "Tritanopia (Blue-Blind)" = "tritanopia"
        ),
        selected = "none"
      ),
      helpText("If you have a visual condition, selecting your condition might help with visibility")
    )
  )
)

#### SERVER ####
server <- function(input, output, session) {

  ##### Reactives ####
  savedData <- reactiveVal(NULL)
  missingData <- reactiveVal(NULL)
  currentTree <- reactiveVal(NULL)
  totalTraits <- reactiveVal(NULL)
  paramsChanged <- reactiveVal(FALSE)

  ##### Dynamic UI generation ####
  output$group_inputs <- renderUI({
    req(input$l)
    lapply(1:input$l, function(i) {
      selectInput(
        paste0("group_", i),
        paste("Number of traits in partition", i),
        choices = 1:100,
        selected = 2
      )
    })
  })

  output$state_inputs <- renderUI({
    req(input$l)
    lapply(1:input$l, function(i) {
      selectInput(
        paste0("state_", i),
        paste("Number of states in partition", i),
        choices = 2:10,
        selected = 2
      )
    })
  })

  ##### Simulation logic ####
  observeEvent(input$goButton, {
    req(input$b > input$d)

    # Choose or reuse tree
    tree <- if (is.null(currentTree()) || !input$keepTreeFixed) {
      TreeSim::sim.bd.taxa(
        n = input$n, numbsim = 1, lambda = input$b, mu = input$d, frac = 1
      )[[1]]
    } else currentTree()
    currentTree(tree)

    # Fossils
    f <- FossilSim::sim.fossils.poisson(rate = input$psi, tree = tree, root.edge = FALSE)

    # Extant
    fossils <- FossilSim::sim.extant.samples(fossils = f, tree = tree, rho = input$rho)

    # Partition info
    num_traits_vector <- as.numeric(sapply(1:input$l, function(i)
      input[[paste0("group_", i)]] %||% 2))
    num_states_vector <- as.numeric(sapply(1:input$l, function(i)
      input[[paste0("state_", i)]] %||% 2))

    totalTraits(sum(num_traits_vector))

    # Model options
    variable_coding <- input$variableCoding
    acrv_value <- if (input$useGamma) "gamma" else NULL

    # Simulate data
    data <- MorphSim::sim.morpho(
      time.tree = tree,
      br.rates = input$r,
      k = num_states_vector,
      trait.num = sum(num_traits_vector),
      partition = num_traits_vector,
      variable = variable_coding,
      ACRV = acrv_value,
      fossil = fossils
    )
    savedData(data)
    missingData(NULL)

    # Add missing-data UI
    output$goButton2_ui <- renderUI({
      actionButton("goButton2", "Simulate Missing Data")
    })
    output$dropdown_ui <- renderUI({
      sliderInput("missing", "Probability of missing data", value = 0.2, min = 0, max = 1, step = 0.01)
    })
    output$explanation_text <- renderUI({
      span("The above simulation generated a complete matrix (no missing traits).
            You can simulate missing data for your matrix according to a given probability.",
           style = "font-size: 14px; color: gray;")
    })

    paramsChanged(FALSE)
  })

  ### Missing data simulation
  observeEvent(input$goButton2, {
    req(savedData(), input$missing)
    missingData(shiny.missing(data = savedData(), missing = input$missing))
  })

  ## Plotting helpers
  emptyPlot <- function(msg) {
    plot(NA, type = "n", xlim = c(0, 5), ylim = c(0, 3),
         ann = FALSE, bty = "n", xaxt = "n", yaxt = "n")
    text(2.5, 1.5, msg, cex = 1.5, col = "#800020")
  }

  ##### Tree Plot ####
  output$plot1 <- renderPlot({
    data <- savedData()
    par(mar = c(6, 4, 2.5, 2))
    if (is.null(data) && input$b < input$d) return(emptyPlot("Speciation rate must exceed extinction rate"))
    if (is.null(data)) return(emptyPlot("Run the simulation"))
    shinyplot(
      data,
      timetree = TRUE,
      trait = input$s,
      cbType = input$cbType,
      show.fossil = isTRUE(input$fossils) && !is.null(data$fossil),
      root.edge = FALSE,
      reconstructed = isTRUE(input$reconstructed)
    )
  })

  ##### Character Matrix Plot ####
  output$plot2 <- renderPlot({
    data <- missingData() %||% savedData()
    if (is.null(data)) return(emptyPlot("No data to display"))
    shiny.grid(data, l = input$s, cbType = input$cbType)
  })

  ##### Trait slider update ####
  observeEvent(totalTraits(), {
    updateNumericInput(session, "s", max = totalTraits())
  })

  ## Parameter change warning
  observe({
    # Track relevant inputs
    list(input$n, input$b, input$d, input$l, input$r)
    paramsChanged(TRUE)
  })

  output$paramWarning <- renderUI({
    if (paramsChanged()) {
      div(
        class = "alert alert-warning",
        role = "alert",
        strong("Notice: "),
        "You have changed parameters but have not run a new simulation yet."
      )
    }
  })

  #### Downloads ####
  output$downloadTree <- downloadHandler(
    filename = function() {
      paste0("simulated_tree_", Sys.Date(), ".nwk")
    },
    content = function(file) {
      req(currentTree())
      ape::write.tree(currentTree(), file)
    }
  )

  output$downloadReconstructedTree <- downloadHandler(
    filename = function() {
      paste0("recon_tree_", Sys.Date(), ".nwk")
    },
    content = function(file) {
      req(currentTree())
      shiny.reconstructed.tree(savedData(), file)
    }
  )
}

# Run App
shinyApp(ui = ui, server = server)
