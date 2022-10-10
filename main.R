library(dplyr)

##################################
# Importation du jeux de donnees
##################################
setwd("C:/Users/Delpl/Desktop/ADD-Project/")
EauxFM <- read.table("EauxFM.txt", header=T, na.string="NA", dec=".", sep='\t')

# Verification de l'importation du jeux de donnees
lignes <- nrow(EauxFM) # OK si = 95
colonnes <- ncol(EauxFM) # OK si = 12

##################################
# Nettoyage
##################################
# Lignes ayant des infos manquantes (NA)
LinesNACheck <- which(is.na(EauxFM),arr.ind=TRUE)[,1]
# Recuperation dans le jeux de donnees des individus ayant des donnees manquantes
LinesWithNA <- EauxFM[LinesNACheck,]
# Recuperation dans le jeux de donnees des individus ayant des donnees manquantes en France
LinesWithNAFrance <- subset(LinesWithNA, Pays == "France") 
# Jeux de donnees sans les individus ayant des infos manquantes
EauxFMWithoutNA <- EauxFM[-LinesNACheck,]

# Eaux de France
FranceWithNA <- subset(EauxFM, Pays == "France") 
# Eaux de France sans NA
FranceWithoutNA <- subset(EauxFMWithoutNA, Pays == "France") 
# Eaux du Maroc
MarocWithNA <- subset(EauxFM, Pays == "Maroc") 
# Eaux du Maroc sans NA
MarocWithoutNA <- subset(EauxFMWithoutNA, Pays == "Maroc") 

# Nombre de NA par variable
for (i in 1:colonnes) {
  vec <- is.na(LinesWithNAFrance[,i])
  count <- sum(vec)
  cat(names(LinesWithNAFrance)[i] , " " , count, "\n")
}

# Il faut retirer les variables NO3 et PH car il y a 
# +30 individus ayant un NA dans ces variables en France


##################################
# Version sans NO3 & PH
##################################
CleanedEauxFM <- select(EauxFM,-c(NO3,PH))

# Verification du jeux de donnees
lignes <- nrow(CleanedEauxFM) # OK si = 95
colonnes <- ncol(CleanedEauxFM) # OK si = 12

# Lignes ayant des infos manquantes
LinesNACheck <- which(is.na(CleanedEauxFM),arr.ind=TRUE)[,1]
# Recuperation dans le jeux de donnees des individus ayant des donnees manquantes
LinesWithNA <- CleanedEauxFM[LinesNACheck,]
# Recuperation dans le jeux de donnees des individus ayant des donnees manquantes en France
LinesWithNAFrance <- subset(LinesWithNA, Pays == "France") 
# Jeux de donnees sans les individus ayant des infos manquantes
EauxFMWithoutNA <- CleanedEauxFM[-LinesNACheck,]

# Eaux de France
FranceWithNA <- subset(CleanedEauxFM, Pays == "France") 
# Eaux de France sans NA
FranceWithoutNA <- subset(EauxFMWithoutNA, Pays == "France") 
# Eaux du Maroc
MarocWithNA <- subset(CleanedEauxFM, Pays == "Maroc") 
# Eaux du Maroc sans NA
MarocWithoutNA <- subset(EauxFMWithoutNA, Pays == "Maroc") 

# Nombre de NA par variable
for (i in 1:colonnes) {
  vec <- is.na(LinesWithNAFrance[,i])
  count <- sum(vec)
  cat(names(LinesWithNAFrance)[i] , " " , count, "\n")
}




# Description univariee et bivariee 
# (Nature, Ca, Mg, Na, K, Cl, NO3, SO4, HCO3, PH, Pays)
pie(summary(as.factor(EauxFM$Nature)))
pie(summary(as.factor(EauxFMWithoutNA$Nature)))
plot(EauxFM$Ca)
plot(EauxFMWithoutNA$Ca)
pie(summary(as.factor(EauxFM$Pays)))

# Description bivariée
