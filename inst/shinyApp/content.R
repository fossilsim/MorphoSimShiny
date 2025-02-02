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

shiny.missing <- function(data, missing){
 missing.data <- MorphoSim::sim.missing.data(data = data,
                   method = "random",
                   probability = missing)

  return(missing.data)
}


shinyplot <- function(data, timetree = T, trait,br.rates, cbType = "none"){
  if (cbType == "none"){
    cb = c("#d3d3d3", "#add8e6", "#ffc0cb", "#ffff64", "#90ee90", "#ffa500", "#e6e6fa", "#ff7f50", "#f5de63", "#ffdeb9", "#60e0e6", "#e0ffff") # standard
  } else if (cbType == "protanopia"){
    cb = c("#e6e6fa", "#b0e0e6", "#f5deb3","#40d0d0","#fafad2", "#98fb98", "#d8bfd8", "#468264", "#87ceeb", "#ffe4b5", "#add8e6", "#ffdead") # Protanopia
  } else if (cbType == "deuteranopia"){
    cb = c("#d8bfd8", "#b0c4de", "#eedd82", "#ffdead", "#87ceeb", "#dda0dd", "#faf0e6","#cd853f", "#f5de63", "#bc8f8f", "#ffe4b5", "#ffebcd") # Deuteranopia
  } else if (cbType == "tritanopia"){
    cb = c("#f4a460", "#ffb6c1", "#faebd7", "#ffcc9a", "#f5de63", "#d8bfd8", "#ffe4b5","#deb887", "#ffdead", "#e6e6fa", "#ffc0cb", "#ffdab9") # Tritanopia
  } else{print("there was an error with the color scheme")}
  plot(data, timetree = T, trait = trait, br.rates = br.rates, col = cb)

}


shiny.grid <- function(data, l, cbType = "none" ){
  if (cbType == "none"){
    cb = c("#d3d3d3", "#add8e6", "#ffc0cb", "#ffff64", "#90ee90", "#ffa500", "#e6e6fa", "#ff7f50", "#f5de63", "#ffdeb9", "#60e0e6", "#e0ffff") # standard
  } else if (cbType == "protanopia"){
    cb = c("#e6e6fa", "#b0e0e6", "#f5deb3","#40d0d0","#fafad2", "#98fb98", "#d8bfd8", "#468264", "#87ceeb", "#ffe4b5", "#add8e6", "#ffdead") # Protanopia
  } else if (cbType == "deuteranopia"){
    cb = c("#d8bfd8", "#b0c4de", "#eedd82", "#ffdead", "#87ceeb", "#dda0dd", "#faf0e6","#cd853f", "#f5de63", "#bc8f8f", "#ffe4b5", "#ffebcd") # Deuteranopia
  } else if (cbType == "tritanopia"){
    cb = c("#f4a460", "#ffb6c1", "#faebd7", "#ffcc9a", "#f5de63", "#d8bfd8", "#ffe4b5","#deb887", "#ffdead", "#e6e6fa", "#ffc0cb", "#ffdab9") # Tritanopia
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
