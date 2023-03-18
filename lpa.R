library(igraph)
library(igraphdata)
source('data.R')
source('utils.R')
# samotna funkcia na prevolavanie algoritmu
# arg:g typu igraph
# arg:plot typu boolean - ci ma vyplotovat vysledok alebo nie, defaultne FALSE
# arg:byFixedLabels - ci ma algoritmus clustrovat podla typu  labelov a nie podla susednosti, napr. volicov nejakej strany do jedneho konkrenteho zhluku, defaultne FALSE
lpa <- function(g,plot=FALSE, byFixedLabels = FALSE){
  if(class(g)!='igraph'){
    return('Input must be type of igraph!')
  }
  # nastavi labele a names podla toho aky vstupny graf sme dostali 
  g <- initGraphSetter(g)
  j <- 0
  # cyklus sa opakuje az pokial nebude vyhodnoteny ako optimalny
  while(!isClusteringOptimal(g, checkLabels = byFixedLabels )){
    print(j)
    # uprava siete podla logiky algoritmu
    g <- algorithmLogic(g,byFixedLabels = byFixedLabels)
    j<- j +1
  }
  print(paste("Total num. of iteration:", j))
  clusteredReturn <- make_clusters(g,membership = shiftMembership(V(g)$label), algorithm= 'Vlastny label propagination algoritmus')
  if(plot){
    plot(clusteredReturn,g, layout = layout_nicely(g), vertex.label = V(g)$label,  vertex.label.cex = 0.75)
  }
  return(clusteredReturn)
}

