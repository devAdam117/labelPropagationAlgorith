jeSietOptimalna<- function(g){
  ids <- V(g)$name
  for(id in ids){
    idx <- which(id == ids)
    labels <- neighbors(g,idx)$label
    maxTblFrequency <- max(table(labels))
    optimalLabels <- as.vector(names(which(table(labels) == maxTblFrequency)))
    if(!(V(g)$label[idx]  %in% optimalLabels)   ) {
      return(FALSE)
    }
  }
  return(TRUE)
}

robAlgoritmus <- function(g){
  resampledNameVector <- sample(V(g)$name)
  numOfNodes <- length(resampledNameVector)
  for(i in 1:numOfNodes){
    currentNodeId <- resampledNameVector[i]
    currentNodeIdx <- which(currentNodeId == V(g)$name)
    neighborLabels <- neighbors(g,currentNodeIdx)$label
    maxFreqLabel <- neighborLabels[1]
    if(length(table(neighborLabels))!=1){
      maxFreqLabel <- names(which.max(sample((table(neighborLabels)))))
    }
    V(g)$label[currentNodeIdx] <- maxFreqLabel
  }
  return(g)  
}

inicializaciaSiete <-  function(g){
  networkLength <- length(V(g))
  V(g)$label <- 1:networkLength
  V(g)$name <- 1:networkLength
  return(g)
}

lpa <- function(g){
  g <- inicializaciaSiete(g)
  while(!jeSietOptimalna(g)){
    g <- robAlgoritmus(g)
  }
  clusteredReturn <- make_clusters(g,membership = shiftMembership(V(g)$label), algorithm= 'Vlastny label propagination algoritmus')
  return(clusteredReturn)
}

