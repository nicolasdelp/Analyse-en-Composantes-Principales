EauxFM <- read.table("EauxFM.txt", header=T, na.string="NA", dec=".", sep='\t') # Importation du fichier .txt

# Verification des dimensions du jeux de donnees
lignes <- nrow(EauxFM) # nombre de lignes (= 95)
colonnes <- ncol(EauxFM) # nombre de colonnes (= 12)

LinesNACheck <- which(is.na(EauxFM),arr.ind=TRUE)[,1] # Recuperation des lignes ayant des donnees manquantes
LinesWithNA <- EauxFM[LinesNACheck,] # Lignes ayant des infos manquantes
EauxFMWithoutNA <- EauxFM[-LinesNACheck,] # Donnees sans les lignes ayant des infos manquantes

# Differents jeux de donnees
FranceWithNA <- subset(EauxFM, Pays == "France") # Eaux de France
MarocWithNA <- subset(EauxFM, Pays == "Maroc") # Eaux du Maroc

# Description univariee et bivariee 
# (Nature, Ca, Mg, Na, K, Cl, NO3, SO4, HCO3, PH, Pays)
pie(summary(as.factor(EauxFM$Nature)))
pie(summary(as.factor(EauxFMWithoutNA$Nature)))
plot(EauxFM$Ca)
plot(EauxFMWithoutNA$Ca)
pie(summary(as.factor(EauxFM$Pays)))

# Description bivariée
