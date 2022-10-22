library(dplyr)

#######################################
# UTILS
#######################################
# Nombre de données manquantes par variable
missingDataPerVariable <- function(datatset){
  for (i in 1:ncol(datatset)) {
    vec <- is.na(datatset[,i])
    count <- sum(vec)
    cat(names(datatset)[i] , " " , count, "\n")
  }
}

#######################################
# Importation du jeu de donnees
#######################################
setwd("C:/Users/Delpl/Desktop/ADD-Project/")
EauxFM <- read.table("EauxFM.txt", header=T, na.string="NA", dec=".", sep='\t')

# Verification de la dimension du jeu de donnees
myDataset.dim = dim(EauxFM) # OK si 95 12

#######################################
# Nettoyage du jeu de donnees
#######################################
# Jeu de données principal (Eaux de France)
France <- subset(EauxFM, Pays == "France")
# Jeu de données supplementaire (Eaux du Maroc)
Maroc <- subset(EauxFM, Pays == "Maroc")

# Lignes ayant des donnees manquantes (NA) en France
FranceLinesWithNA <- which(is.na(France),arr.ind=TRUE)[,1]
FranceNA <- France[FranceLinesWithNA,]

# On affiche le nombre d'individus ayant des donnees manquantes par variable
missingDataPerVariable(FranceNA)

# Il faut retirer les variables NO3 et PH car il y a 30 individus ayant un NA dans ces variables en France

# Jeu de données principal sans NO3 et le PH (Eaux de France)
FranceWithoutNO3PH <- select(France,-c(NO3,PH))

# On affiche le nombre d'individus ayant des données manquantes par variable
missingDataPerVariable(FranceWithoutNO3PH)

# On a diminue considerablement le nombre d'individus ayant des donnees manquantes

# On retire les derniers individus ayant des donnees manquantes
FranceLinesWithNA <- which(is.na(FranceWithoutNO3PH),arr.ind=TRUE)[,1]
myDataset.CleanFrance <- FranceWithoutNO3PH[-FranceLinesWithNA,]

# On verifie qu'il n'y a plus d'individus ayant des donnees manquantes
missingDataPerVariable(myDataset.CleanFrance)


#######################################
# Description univariée et bivariée
#######################################


#######################################
# Description multivariée
#######################################

