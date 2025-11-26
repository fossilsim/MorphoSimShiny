simulateMorphData <- function(n, b, d, l, r, psi, rho,
                              variableCoding = FALSE, useGamma = FALSE,
                              fixedTree = NULL,
                              input = NULL) {   # <- pass input explicitly
  # 1. Choose or reuse tree
  tree <- if (is.null(fixedTree)) {
    TreeSim::sim.bd.taxa(n = n, numbsim = 1, lambda = b, mu = d, frac = 1)[[1]]
  } else fixedTree

  # 2. Fossils
  f <- FossilSim::sim.fossils.poisson(rate = psi, tree = tree, root.edge = FALSE)
  fossils <- FossilSim::sim.extant.samples(fossils = f, tree = tree, rho = rho)

  # 3. Partition info from Shiny input
  num_traits_vector <- sapply(1:l, function(i) {
    val <- input[[paste0("group_", i)]]
    if (is.null(val)) 2 else as.numeric(val)
  })

  num_states_vector <- sapply(1:l, function(i) {
    val <- input[[paste0("state_", i)]]
    if (is.null(val)) 2 else as.numeric(val)
  })

  # 4. Simulate trait data
  data <- MorphSim::sim.morpho(
    time.tree = tree,
    br.rates = r,
    k = num_states_vector,
    trait.num = sum(num_traits_vector),
    partition = num_traits_vector,
    variable = variableCoding,
    ACRV = if (useGamma) "gamma" else NULL,
    fossil = fossils
  )

  list(data = data, tree = tree)
}


shiny.morpho <- function(n , b , d , l , k, r) {
  tree <-TreeSim::sim.bd.taxa(n, numbsim = 1, lambda = b, mu = d, frac = 1)[[1]]
  data <- MorphSim::sim.morpho(
    time.tree = tree,
    br.rates = r,
    k = k,
    trait.num = l,
  )

  return(data)
}

shiny.missing <- function(data, missing){
  missing.data <- MorphSim::sim.missing.data(data = data,
                                              method = "random",
                                              seq = "tips",
                                              probability = missing)

  return(missing.data)
}


shinyplot <- function(data, timetree = T, trait,br.rates, cbType = "none", show.fossil, root.edge, reconstructed,
                      edges = 3, label.offset = 0.05, f.cex = 2, e.cex = 1){
  if (cbType == "none"){
    cb = c("#fff5ee", "#add8e6", "#ffc0cb", "#ffff64", "#90ee90", "#ffa500", "#e6e6fa", "#ff7f50", "#f5de63", "#ffdeb9", "#60e0e6", "#e0ffff") # standard
  } else if (cbType == "protanopia"){
    cb = c("#e6e6fa", "#b0e0e6", "#f5deb3","#40d0d0","#fafad2", "#98fb98", "#d8bfd8", "#468264", "#87ceeb", "#ffe4b5", "#add8e6", "#ffdead") # Protanopia
  } else if (cbType == "deuteranopia"){
    cb = c("#d8bfd8", "#b0c4de", "#eedd82", "#ffdead", "#87ceeb", "#dda0dd", "#faf0e6","#cd853f", "#f5de63", "#bc8f8f", "#ffe4b5", "#ffebcd") # Deuteranopia
  } else if (cbType == "tritanopia"){
    cb = c("#f4a460", "#ffb6c1", "#faebd7", "#ffcc9a", "#f5de63", "#d8bfd8", "#ffe4b5","#deb887", "#ffdead", "#e6e6fa", "#ffc0cb", "#ffdab9") # Tritanopia
  } else{print("there was an error with the color scheme")}
  plot(data, timetree = T, trait = trait, col = cb, show.fossil = show.fossil, edges= edges,root.edge = F,
       label.offset = label.offset,reconstructed = reconstructed, f.cex = f.cex, e.cex = e.cex)

}


shiny.grid <- function(data, l, seq = "tips", cbType = "none" ){
  if (cbType == "none"){
    cb = c("#fff5ee", "#add8e6", "#ffc0cb", "#ffff64", "#90ee90", "#ffa500", "#e6e6fa", "#ff7f50", "#f5de63", "#ffdeb9", "#60e0e6", "#e0ffff") # standard
  } else if (cbType == "protanopia"){
    cb = c("#e6e6fa", "#b0e0e6", "#f5deb3","#40d0d0","#fafad2", "#98fb98", "#d8bfd8", "#468264", "#87ceeb", "#ffe4b5", "#add8e6", "#ffdead") # Protanopia
  } else if (cbType == "deuteranopia"){
    cb = c("#d8bfd8", "#b0c4de", "#eedd82", "#ffdead", "#87ceeb", "#dda0dd", "#faf0e6","#cd853f", "#f5de63", "#bc8f8f", "#ffe4b5", "#ffebcd") # Deuteranopia
  } else if (cbType == "tritanopia"){
    cb = c("#f4a460", "#ffb6c1", "#faebd7", "#ffcc9a", "#f5de63", "#d8bfd8", "#ffe4b5","#deb887", "#ffdead", "#e6e6fa", "#ffc0cb", "#ffdab9") # Tritanopia
  } else{print("there was an error with the color scheme")}
  MorphSim::plotMorphoGrid(data, timetree = TRUE, num.trait = l, seq = seq, col = cb)
}

shiny.reconstructed.tree <- function (data, file){
  MorphSim::write.recon.tree(data, file)
}

shiny.ages <- function (data, file){
  MorphSim::write.tsv(data, file)
}

shiny.matrix <- function(data, file, keep_matrix = F){
  MorphSim::write.recon.matrix(data, file, keep_matrix = F)
}

shiny.get.reconstructed <- function(data){
  MorphSim::get.reconstructed(data)
}


