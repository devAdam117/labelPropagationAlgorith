library(igraph)
#logika algoritmu
algorithmLogic <- function(g,byFixedLabels = FALSE){
  #poprehadzovanie ideciek siete
  resampledNameVector <- sample(V(g)$name)
  # ak su nejake fixnute nody, ktorych nody nechceme menit zadane explicitne + existuju, tak ich vyradime z nasemplovaneho vektoru
  if(byFixedLabels==TRUE  && length(g$nameOfFixedLabels)>0){
    resampledNameVector <- resampledNameVector[!(resampledNameVector %in% g$nameOfFixedLabels)]
  }
  numOfNodes <- length(resampledNameVector)
  # kazdy node si prezrie a zmeni mu label podla logiky algoritmu
  for(i in 1:numOfNodes){
    currentNodeId <- resampledNameVector[i]
    #index id v povodnej sieti
    currentNodeIdx <- which(currentNodeId == V(g)$name)
    #ked sa vyhladava podla int, tak hlada podla poradia V(g)$name, ked podla stringu tak hlada konkretne id
    neighborIds <- neighbors(g,currentNodeIdx)$name
    neighborIdxs <- sapply(neighborIds,function(i){
      which(i == V(g)$name)
    })
    neighborLabels <- V(g)$label[neighborIdxs]
    # kontig. tabulka -> presampluju sa vysledne stlpce -> vybere sa prvy s maximalnymm stlpcom -> tj. ze ak su dva 
    # uplne rovnake tak vrati spravodlivo nahodne jeden z nich
    maxFreqLabel <- neighborLabels[1]
    # ak je dlzka==1 tak bol problem so samplovanim tabulky sample(c(1,1,1)) -> 1, 2, 3, tak sa to takto osetrilo...
    if(length(table(neighborLabels))!=1){
      maxFreqLabel <- names(which.max(sample((table(neighborLabels)))))
    }
    V(g)$label[currentNodeIdx] <- maxFreqLabel
  }
  return(g)
}
# lubovolny vektor napr. c(1,100,100,2,6,49) presunie na -> c(1,5,5,2,3,4), teda usporiadava jeho elemtny od najnizsieho po najvacsi
# vyuzitelnost pri tom aby membership napr nebol 1, 100, 2, 6 ,49 ale 1:5, tym padom dostavame aj relevantnu informaciu o tok, kolko membershipov sa celkovo nachadza pre dany graf
shiftMembership <- function(vector){
  uniqueVector <- sort(unique(vector))
  newVector <- match(vector, uniqueVector)
  for (i in unique(vector)) {
    idx <- which(vector == i)
    if (length(idx) > 1) {
      newVector[idx[2:length(idx)]] <- newVector[idx[2:length(idx)]] 
    }
  }
  return(newVector)
  
}



isClusteringOptimal <- function(g,checkLabels = FALSE){
  ids <- V(g)$name
  for(id in ids){
    if(checkLabels==TRUE  && length(g$nameOfFixedLabels) >0 && id %in% g$nameOfFixedLabels) next
    currentNodeIdx <- which(id == ids)
    neighborIds <- neighbors(g,currentNodeIdx)$name
    neighborIdxs <- sapply(neighborIds,function(i){
      which(i == V(g)$name)
    })
    neighborLabels <- V(g)$label[neighborIdxs]
    maxTblFrequency <- max(table(neighborLabels))
    optimalLabels <- as.vector(names(which(table(neighborLabels) == maxTblFrequency)))
    if(!(V(g)$label[currentNodeIdx]  %in% optimalLabels)   ) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# handluje labele a name pre dany graf, ak nie su nastavy ich ak su iba nejake tak zvysne vygeneruje ak nie su ziadne tak nastavi uplne vsetky
initGraphSetter <- function(g){
  # najprv sa pozrie na id/names grafu a setne ten
  names <- V(g)$name
  if(is.null(names)){
    V(g)$name <- 1:length(g)
  }
  # ak su ale nejake id prazdne a ine nie tak prazdnym prideli hodnotu
  V(g)$name <- as.vector(sapply(V(g)$name, function(i){
    if(i == '' | i == '?' | i == ' '){
      return(uuid::UUIDgenerate())
    }
    return(i)
  }))
  # pozrieme sa na labels
  if(is.null(V(g)$label)){
    V(g)$label <- 1:length(g)
  }
  # ak nie je null, tak si uschovame idecka takych nodov, ktorych labele na zaciatku boli setnute
  g$nameOfFixedLabels <- V(g)$name[(V(g)$label!= '?' & V(g)$label != '' & V(g)$label != ' ')]
  # prazdne labele inicializujeme nahodnymi unikatnymi retazcami
  V(g)$label <- as.vector(sapply(V(g)$label, function(i){
    if(i == '' | i == '?' | i == ' '){
      return(uuid::UUIDgenerate())
    }
    return(i)
  }))
  return(g)
}

