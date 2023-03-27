# TESTY
source('lpa.R')
# VLASTNE DATA - NOT KNOWN LABEL
preferencie <- getPreferencieNetwork(FALSE,3)
ret <- list()
for (i in 1:100){
  ret[i] <- list(cluster_label_prop(preferencie))
}
lapply(ret,function(optResult){
  par(new=TRUE)
  set.seed(2)
  plot(optResult,preferencie,layout=layout_nicely(preferencie))
  })

ret <- list()
for (i in 1:100){
  ret[i] <- list(lpa(preferencie,FALSE,byFixedLabels = FALSE))
}
lapply(ret,function(optResult){
  par(new=TRUE)
  set.seed(2)
  plot(optResult,preferencie,layout=layout_nicely(preferencie))
})

lpa(preferencie,TRUE,byFixedLabels = TRUE)


# KARATE DATA - KNOWN LABEL + CLPA
data(karate)

ret <- list()
for (i in 1:100){
  ret[i] <- list(cluster_label_prop(karate))
}
lapply(ret,function(optResult){
  par(new=TRUE)
  set.seed(2)
  plot(optResult,karate,layout=layout_nicely(karate))
})

ret <- list()
for (i in 1:100){
  ret[i] <- list(lpa(karate,FALSE,byFixedLabels = FALSE))
}
lapply(ret,function(optResult){
  par(new=TRUE)
  set.seed(2)
  plot(optResult,karate,layout=layout_nicely(karate))
})
# vsetci mali zadany label, takze bol fixny a teda nikomu sa nemal menit -> toto je optimalne
lpa(karate,TRUE,TRUE)

# DATA YEAST
data(yeast)
ret <- list()
for (i in 1:10){
  ret[i] <- list(cluster_label_prop(yeast))
}
lapply(ret,function(optResult){
  par(new=TRUE)
  set.seed(2)
  plot(optResult,yeast,layout=layout_nicely(yeast),vertex.size=1,edge.width=0.1,vertex.label=NA)
})

ret <- list()
for (i in 1:10){
  ret[i] <- list(lpa(yeast,FALSE,byFixedLabels = FALSE))
}
lapply(ret,function(optResult){
  par(new=TRUE)
  set.seed(2)
  plot(optResult,yeast,layout=layout_nicely(yeast),vertex.size=1,edge.width=0.1,vertex.label=NA)
})

plot(lpa(yeast,FALSE,TRUE),yeast,layout=layout_nicely(yeast),vertex.size=1,edge.width=0.1,vertex.label=NA)



# Clustrovanie podla labelov (napr. volici konketnej strany v sieti)
# ako je to ked vieme nejake labely siete ale nejake nie
# predpokladame, ze tych ktorych nepozname budu mat labely tych ktorych pozname / vzniknu este nejake nove
# tiez predpokladame ze tych ktorych pozname labely sa uz nezmenia
 
preferencie2 <- getPreferencieNetwork(TRUE,3)
ret <- list()
for (i in 1:100){
  ret[i] <- list(lpa(preferencie2,FALSE,byFixedLabels = TRUE))
}
lapply(ret,function(optResult){
  par(new=TRUE)
  set.seed(2)
  plot(optResult,preferencie2,layout=layout_nicely(preferencie2))
})

karateCopy <- karate
nezname <- 2:(length(V(karate)) - 1) 
V(karateCopy)$label[nezname] <- '?'
ret <- list()
for (i in 1:100){
  ret[i] <- list(lpa(karateCopy,FALSE,byFixedLabels = TRUE))
}
lapply(ret,function(optResult){
  par(new=TRUE)
  set.seed(2)
  plot(optResult,karateCopy,layout=layout_nicely(karateCopy))
})

V(karateCopy)$label[10] <- 'X'
V(karateCopy)$label[20] <- 'Y'
ret <- list()
for (i in 1:100){
  ret[i] <- list(lpa(karateCopy,FALSE,byFixedLabels = TRUE))
}
lapply(ret,function(optResult){
  par(new=TRUE)
  set.seed(2)
  plot(optResult,karateCopy,layout=layout_nicely(karateCopy))
})
