#vytvorenie nejakych umelych dat...
getPreferencieNetwork <- function(labels= TRUE,num){
  g <- make_full_graph(2)
  g <- add_vertices(g,13)
  if(labels==TRUE) {
    labelNames <- c('SAS','SAS','?','?','?','PS','SMER','?','?','?','?','PS','?','?','?')
  }
  if(labels==FALSE){
    labelNames <- rep('?',15)
  }
  g <- add.edges(g,c(1,3,1,4,2,3,2,4,3,4,4,5,5,6,5,7,6,7,6,8,6,9,6,10,7,8,7,9,7,10,8,9,8,10,9,10,8,13,13,14,14,11,14,12,14,15,15,11,15,12))
  labelCol <- labelNames
  labelCol[labelCol=='SAS'] <- 'green'
  labelCol[labelCol=='PS'] <- '2'
  labelCol[labelCol=='SMER'] <- 'red'
  labelCol[labelCol=='?'] <- 0
  V(g)$label.cex <- 0.72
  V(g)$label <- labelNames
  if(!is.null(num)){
    set.seed(num)
  }
  plot(g,
       layout=layout_nicely(g),
       vertex.label=labelNames,
       vertex.size=18,
       vertex.color = labelCol,
       vertex.label.family="Times")
  
  return(g)
}

# pref <- getPreferencieNetwork(FALSE,3)
# set.seed(3)
# plot(pref, vertex.label = floor(runif(15)*16), vertex.color = 0, 
#     layout=layout_nicely(pref),
#     vertex.size=18,
#     vertex.label.family="Times")
