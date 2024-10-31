#plot(rphylo(n = 10, birth = 1, death = 0.5, fossils = T))

##Forward time simulation
simulate_treeseq <- function(N, b, d, L, u, c) {
  
  phy <- rphylo(N, b, d, fossils = T)
  
  data <- simSeq(
    phy, 
    l = L, 
    type = "USER", 
    levels = as.character(0:(c-1)), 
    rate = u,
    rootseq = rep("0", L), 
    ancestral = TRUE
  )
  
  list(phy = phy, data = data, N = N, L = L, c = c, b = b, d = d)
  
}

plot_treeseq <- function(.l) {
  
  phy <- .l$phy
  data <- as.character(.l$data)
  N <- .l$N
  L <- .l$L
  c <- .l$c
  
  palet <- c("#0088ff","#ff8800","#ffff88","#88ff88","#8844ff","#ff4488","#dcdc00","#00dd88","#88ddff","#dd7799","#cdcd99","#77dc11","#4477aa","#aa4477","#eeee77","#43cd93","#0f0f4f","#400040","#000000","#ffffff")
  coolers <- palet[1:c]
  
  x1 <- as.data.frame(phy$edge)
  colnames(x1) <- c("parent", "node")
  x1$parent <- as.character(x1$parent)
  x1$node <- ifelse(x1$node <= N, paste0("t", x1$node), as.character(x1$node))
  
  x2 <- as.data.frame(data)
  x2 <- tibble::rownames_to_column(x2, "node")
  x2 <- dplyr::mutate_if(x2, is.factor, as.character)
  x2 <- tidyr::pivot_longer(x2, cols = -node, names_to = "position")
  
  x3 <- dplyr::left_join(dplyr::select(x1, node = parent), x2, by = "node")
  x3 <- dplyr::rename(x3, parent = node, parent_value = value)
  
  x4 <- dplyr::full_join(x2, x1, by = "node")
  x4 <- dplyr::full_join(x4, x3, by = c("parent", "position"))
  x4 <- dplyr::distinct(x4)
  x4 <- dplyr::filter(x4, !is.na(parent_value))
  x4.1 <- dplyr::mutate(x4, 
                        label = node, 
                        node = as.integer(stringr::str_remove(node, "^t")),
                        is_mutated = value != parent_value,
                        mutation_position = ifelse(is_mutated, stringr::str_remove(position, "^V"), NA)
  )
  x4.1 <- dplyr::filter(x4.1, !is.na(mutation_position))
  x4.1 <- dplyr::select(x4.1, parent, node, is_mutated, mutation_position)
  x4.1 <- dplyr::group_by(x4.1, parent, node)
  x4.1 <- dplyr::summarize(x4.1, edge_label_m = stringr::str_c(mutation_position, collapse = ","))
  #  x4.2 <-  x4 <- dplyr::mutate(x4, 
  #                               label = node, 
  #                               node = as.integer(stringr::str_remove(node, "^t")),
  #                               is_returned = is.mutated != 1,
  #                               returned_position = ifelse(is_returned, stringr::str_remove(position, "^V"), NA)
  #  )
  #  x4.2 <- dplyr::filter(x4.2, !is.na(returned_position))
  #  x4.2 <- dplyr::select(x4.2, parent, node, is_returned, returned_position)
  #  x4.2 <- dplyr::group_by(x4.2, parent, node)
  #  x4.2 <- dplyr::summarize(x4.2, edge_label_r = stringr::str_c(mutation_position, collapse = ","))
  
  p <- ggtree(phy, ladderize = FALSE, size = 2) + 
    geom_rootedge() +
    ggtitle("Species tree")
  
  p <- p %<+% x4.1 + 
    geom_label(aes(x = branch, label = edge_label_m), fill = "#ff00bb") +
    scale_y_continuous(limits = c(0.5, N + 0.5)) +
    theme(legend.position = "none", text = element_text(size = 18))
  
  #  p <- p %<+% x4.2 + 
  #    geom_label(aes(x = branch, label = edge_label_r), fill = "steelblue") +
  #    scale_y_continuous(limits = c(0.5, N + 0.5)) +
  #    theme(legend.position = "none", text = element_text(size = 18))
  
  x5 <- dplyr::mutate(x2,
                      x = as.numeric(stringr::str_remove(position, "^V")),
                      y = as.numeric(stringr::str_remove(node, "^t")),
                      text = ifelse(value %in% 0:3, x, NA)  # <- can use this below but cleaner without
  )
  x5 <- dplyr::filter(x5, node %in% phy$tip.label)
  
  q <- ggplot(x5, aes(x, y, fill = value, label = "")) +
    geom_tile(width = 1, height = 1, color = "black") +
    geom_text() +
    scale_fill_manual(values = coolers) + # Add colors for levels 0 to 3
    scale_y_continuous(limits = c(0.5, N + 0.5)) +
    ggtitle("Character alignment") +
    theme_void() +
    theme(legend.position = "none")
  
  r_data <- data.frame(x = 1, y = 1:length(phy$tip.label), az = phy$tip.label)
  r <- ggplot(r_data, aes(x, y, label = az)) +
    scale_y_continuous() + #limits = c(0.5, length(phy$tip.label) + 0.5)
    geom_text() +
    theme_void()
  
  plot_grid(p, r, q, ncol = 3, rel_widths = c(1, 0.1, 1), align = "hv")
}

plot.BM <- function(N){
  tree <- rtree(N)
  mapped<-make.era.map(tree,
                       limits=seq(0,max(node.depth.edgelength(tree)),length.out=nlev+1))
  tt<-map.to.singleton(mapped)
  x<-fastBM(tt,internal=TRUE)
  ## compute plot without plotting
  plotTree(tt,plot=F,ftype="off")
  pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
  ## set tolerance
  tol<-1e-6
  ## function to mix colors
  mix_colors<-function(cols) colorRampPalette(cols)(3)[2]
  ## all colors
  cols<-setNames(hcl.colors(n=Ntip(tree)),tree$tip.label)
  ## plotting loop
  for(i in 1:nlev){
    dev.hold()
    ## subdivide plotting area
    layout(matrix(c(1,2),2,1),heights=c(0.4,0.6))
    ## plot tree from 1 to i
    par(mar=c(1.1,3.1,2.1,1.1))
    plot(NA,xlim=c(0,max(node.depth.edgelength(tree))),ylim=c(range(pp$yy)),bty="n",
         axes=FALSE)
    ii<-intersect(which(pp$xx[pp$edge[,2]]>((i*max(node.depth.edgelength(tree))+tol-1)/nlev)),
                  which(pp$xx[pp$edge[,2]]<=((i*max(node.depth.edgelength(tree))+tol)/nlev)))
    
    
    for(j in 1:nrow(pp$edge)){
      if(pp$xx[pp$edge[j,2]]<=(i*max(node.depth.edgelength(tree))+tol)/nlev){
        lines(pp$xx[pp$edge[j,]],pp$yy[pp$edge[j,]],type="s")
      }
    }
    ## add points at tips
    for(j in 1:length(ii)){
      daughters<-getDescendants(tt,pp$edge[ii[j],2])
      COL<-mix_colors(cols[daughters[daughters<=Ntip(tree)]])
      points(pp$xx[pp$edge[ii[j],2]],pp$yy[pp$edge[ii[j],2]],
             pch=16,col=COL,cex=1.5)
    }
    ## plot random walk from 1 to i
    par(mar=c(3.1,3.1,1.1,1.1))
    plot(NA,xlim=c(0,max(node.depth.edgelength(tree))),ylim=range(x),bty="n",las=1)
    for(j in 1:nrow(pp$edge)){
      if(pp$xx[pp$edge[j,2]]<=(i*max(node.depth.edgelength(tree))+tol)/nlev){
        lines(pp$xx[pp$edge[j,]],x[pp$edge[j,]])
      }
    }
    ## add points at tip
    for(j in 1:length(ii)){
      daughters<-getDescendants(tt,pp$edge[ii[j],2])
      COL<-mix_colors(cols[daughters[daughters<=Ntip(tree)]])
      points(pp$xx[pp$edge[ii[j],2]],x[pp$edge[ii[j],2]],
             pch=16,col=COL,cex=1.5)
    }
    dev.flush()
    Sys.sleep(0.05) ## sleep R
  }
}
