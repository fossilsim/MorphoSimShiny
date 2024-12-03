shiny.morpho <- function(n , b , d , l , k, r) {
  tree <-TreeSim::sim.bd.taxa(n, numbsim = 1, lambda = b, mu = d, frac = 1)[[1]]
  data <- sim.morpho.completeprocess(
    time.tree = tree, 
    br.rates = r,
    k = k, 
    trait.num = l,
  )

return(data)
}

shiny.grid <- function(data){
library(MorphoSim)
plot.morpho.grid(data)
}