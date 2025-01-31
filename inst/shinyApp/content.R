shiny.morpho <- function(n , b , d , l , k, r) {
  tree <-TreeSim::sim.bd.taxa(n, numbsim = 1, lambda = b, mu = d, frac = 1)[[1]]
  data <- MorphoSim::sim.morpho(
    time.tree = tree,
    br.rates = r,
    k = k,
    trait.num = l,
  )

return(data)
}
shinyplot <- function(data, timetree = T, trait,br.rates, cbType = "none"){
  if (cbType == "none"){
    cb = c("#d3d3d3", "#add8e6", "#ffc0cb", "#ffff64", "#008000", "#ffa500", "#e6e6fa", "#ff7f50") # standard
  } else if (cbType == "protanopia"){
    cb = c("#d6d0d1", "#cecee0", "#d2cdd2", "#fff7df", "#7c6d00", "#d0b711", "#e5e5fa", "#b3a25c") # Protanopia
  } else if (cbType == "deuteranopia"){
    cb = c("#e6cbd4", "#d8caea", "#e7c8c7", "#fff6ed", "#8a671d", "#eaad00", "#f4e0fb", "#cb9a49") # Deuteranopia
  } else if (cbType == "tritanopia"){
    cb = c("#d4cfe0", "#acd6e8", "#ffbecd", "#fff5fa", "#3a757f", "#ff9ba5", "#e5e5f8", "#ff7982") # Tritanopia
  } else{print("there was an error with the color scheme")}
  plot(data, timetree = T, trait = trait, br.rates = br.rates, col = cb)
  
}


shiny.grid <- function(data, l, cbType = "none" ){
  if (cbType == "none"){
    cb = c("#d3d3d3", "#add8e6", "#ffc0cb", "#ffff64", "#008000", "#ffa500", "#e6e6fa", "#ff7f50") # standard
  } else if (cbType == "protanopia"){
    cb = c("#d6d0d1", "#cecee0", "#d2cdd2", "#fff7df", "#7c6d00", "#d0b711", "#e5e5fa", "#b3a25c") # Protanopia
  } else if (cbType == "deuteranopia"){
    cb = c("#e6cbd4", "#d8caea", "#e7c8c7", "#fff6ed", "#8a671d", "#eaad00", "#f4e0fb", "#cb9a49") # Deuteranopia
  } else if (cbType == "tritanopia"){
    cb = c("#d4cfe0", "#acd6e8", "#ffbecd", "#fff5fa", "#3a757f", "#ff9ba5", "#e5e5f8", "#ff7982") # Tritanopia
  } else{print("there was an error with the color scheme")}
MorphoSim::plotMorphoGrid(data, num.trait = l, col = cb)
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
