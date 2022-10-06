setwd("~/Desktop/")
EauxFM <- read.table("EauxFM.txt", header=T, na.string="NA", dec=".", sep='\t') # Importation du fichier .txt

lignes <- nrow(EauxFM) # nombre de lignes (= 95)
colonnes <- ncol(EauxFM) # nombre de colonnes (= 12)

# Description univariée
Nature <- subset(EauxFM, select=c('Nature'))
Ca <- subset(EauxFM, select=c('Ca'))
Mg <- subset(EauxFM, select=c('Mg'))
Na <- subset(EauxFM, select=c('Na'))
K <- subset(EauxFM, select=c('K'))
Cl <- subset(EauxFM, select=c('Cl'))
NO3 <- subset(EauxFM, select=c('NO3'))
SO4 <- subset(EauxFM, select=c('SO4'))
HCO3 <- subset(EauxFM, select=c('HCO3'))
PH <- subset(EauxFM, select=c('PH'))
Pays <- subset(EauxFM, select=c('Pays'))

pie(summary(as.factor(EauxFM$Nature)))
pie(summary(as.factor(EauxFM$Pays)))

# Description bivariée

