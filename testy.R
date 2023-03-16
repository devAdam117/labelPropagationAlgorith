# TESTY
source('lpa.R')

preferencie <- getPreferencieNetwork(FALSE,3)
plot(cluster_label_prop(preferencie),preferencie)
lpa(preferencie,TRUE,byFixedLabels = FALSE)
lpa(preferencie,TRUE,byFixedLabels = TRUE)


#karate
data(karate)
plot(cluster_label_prop(karate),karate)
lpa(karate,TRUE,FALSE)
# vsetci mali zadany label, takze bol fixny a teda nikomu sa nemal menit -> toto je optimalne
lpa(karate,TRUE,TRUE)

data(yeast)
## cca 223,234,226,239 groups
cluster_label_prop(yeast)
## cca 220,217,   -> ofc pojde to omnoho pomalsie lebo to nie je uplne zoptimalizovane + skompilovane
lpa(yeast,FALSE,FALSE)
# kazdemu cluster zvlast lebo kazdy ma label
lpa(yeast,FALSE,TRUE)

# Clustrovanie podla labelov (napr. volici konketnej strany v sieti)
# ako je to ked vieme nejake labely siete ale nejake nie
# predpokladame, ze tych ktorych nepozname budu mat labely tych ktorych pozname / vzniknu este nejake nove
# tiez predpokladame ze tych ktorych pozname labely sa uz nezmenia
preferencie2 <- getPreferencieNetwork(TRUE,3)
plot(cluster_label_prop(preferencie2),preferencie2)
#clustruje podla hodnot labelov nie podla 'grup'
lpa(preferencie2,TRUE,TRUE)

karateCopy <- karate
nezname <- sample(2:(length(karateCopy)-1),length(karateCopy)-4)
V(karateCopy)$label[nezname] <- '?'
#po starom
plot(cluster_label_prop(karateCopy),karateCopy)
lpa(karateCopy,TRUE,FALSE)
#po novom
lpa(karateCopy,TRUE,TRUE)
