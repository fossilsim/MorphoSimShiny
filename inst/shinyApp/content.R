shiny.morpho <- function(n , b , d , l , k, r) {
  tree <-TreeSim::sim.bd.taxa(n, numbsim = 1, lambda = b, mu = d, frac = 1)[[1]]
  data <- MorphoSim::sim.morpho.completeprocess(
    time.tree = tree, 
    br.rates = r,
    k = k, 
    trait.num = l,
  )

return(data)
}

shiny.grid <- function(data, l ){
MorphoSim::plotMorphoGrid(data, num.trait = l )
}

# User specific tree stuff
parseNewickTree <- function(newickString) {
  if (newickString != "") {
    tryCatch({
      tree <- ape::read.tree(text = newickString)
      #input$n <- length(tree$tip.label)
      return(tree)
    }, error = function(e) {
      shiny::showModal(modalDialog(
        title = "Error",
        paste("Invalid Newick string. Please check the format.")
      ))
      return(NULL)
    })
  }
  return(NULL)
}