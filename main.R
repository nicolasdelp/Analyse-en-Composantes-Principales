library(dplyr)
library(GGally)
library(plotrix)

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

# Lignes ayant des donnees manquantes (NA)
FranceLinesWithNA <- which(is.na(France),arr.ind=TRUE)[,1]
FranceNA <- France[FranceLinesWithNA,]
MarocLinesWithNA <- which(is.na(Maroc),arr.ind=TRUE)[,1]
MarocNA <- Maroc[MarocLinesWithNA,]

# On affiche le nombre d'individus ayant des donnees manquantes par variable
missingDataPerVariable(FranceNA)
missingDataPerVariable(MarocNA)

# Il faut retirer les variables NO3 et PH car il y a 30 individus ayant un NA dans ces variables en France

# Jeu de données sans le NO3, le PH et le Pays
FranceWithoutNO3PH <- select(France,-c(NO3,PH,Pays))
MarocWithoutNO3PH <- select(Maroc,-c(NO3,PH,Pays))

# On affiche le nombre d'individus ayant des données manquantes par variable
missingDataPerVariable(FranceWithoutNO3PH)
missingDataPerVariable(MarocWithoutNO3PH)

# On a diminue considerablement le nombre d'individus ayant des donnees manquantes

# On retire les derniers individus ayant des donnees manquantes
FranceLinesWithNA <- which(is.na(FranceWithoutNO3PH),arr.ind=TRUE)[,1]
myDataset.CleanFrance <- FranceWithoutNO3PH[-FranceLinesWithNA,]
MarocLinesWithNA <- which(is.na(MarocWithoutNO3PH),arr.ind=TRUE)[,1]
myDataset.CleanMaroc <- MarocWithoutNO3PH[-MarocLinesWithNA,]

# On change la variable qualitative en variable quantitative (la Nature)
myDataset.CleanFrance$Nature <- as.numeric(factor(myDataset.CleanFrance$Nature))
myDataset.CleanMaroc$Nature <- as.numeric(factor(myDataset.CleanMaroc$Nature))

# On verifie qu'il n'y a plus d'individus ayant des donnees manquantes
missingDataPerVariable(myDataset.CleanFrance)
missingDataPerVariable(myDataset.CleanMaroc)


#######################################
# Description univariee
#######################################
# Analyse univariee avec la fonction "summary" proposee par R (pas tres interessant)
summary(myDataset.CleanFrance)
  
#######################################
# Description bivariee
#######################################
# Premiere analyse bivariee du dataste avec la fonction "pairs"
pairs(myDataset.CleanFrance[,2:ncol(myDataset.CleanFrance)]) # On retire la première colonne (Nom) elle ne sert à rien ici

# On fait une analyse bivariee du dataset avec "ggpairs" pour avoir plus d'informations qu'avec la fonction "pairs"
ggpairs(myDataset.CleanFrance[,2:ncol(myDataset.CleanFrance)]) # On retire la première colonne (Nom) elle ne sert à rien ici

#######################################
# Description multivariee (ACP)
#######################################
# Retourne la matrice centree-reduite XCR
get_XCR <- function(X){
  return(scale(X, scale=TRUE))
}

# Retourne la matrice diagonale D
get_D <- function(X){
  n <- nrow(X)
  return(1/(n-1))
}

# Retourne la matrice de variances-covariances S
get_S <- function(X){
  XCR <- get_XCR(X)
  return(get_D(X) * (t(XCR) %*% XCR))
}

# Retourne les valeurs propres
get_LambdasAndEigenVectors <- function(X){
  eig = eigen(get_S(X))
  
  roundedLambdas = round(eig$values, 10)
  roundedLambdas = roundedLambdas[roundedLambdas != 0]
  
  vectors = eig$vectors[,1:length(roundedLambdas)]
  
  return(list("lambdas" = roundedLambdas, "vectors" = vectors))
}

# Retourne la somme des lambdas
get_SumLambdasValues <- function(X){
  lambdas <- get_LambdasAndEigenVectors(X)$lambdas
  return(sum(lambdas))
}

# Calcul du pourcentage d'inertie total
get_InertiaAxes <- function(X){
  lambdas <- get_LambdasAndEigenVectors(X)$lambdas
  lambdaSum <- get_SumLambdasValues(X)
  lambdaPercentage <- lambdas/lambdaSum
  totalInertia <- 0
  i <- 1
  
  while (totalInertia <= 0.75) {
    totalInertia = totalInertia + lambdaPercentage[i]
    i = i+1
  }
  
  barplot(lambdaPercentage*100, ylim=c(0,100))
  
  return(list("dims" = i, "totalInertia" = totalInertia*100))
}

# Calcul des composantes principales
get_F <- function(X){
  XCR <- get_XCR(X)
  dims <- get_InertiaAxes(X)$dims
  vectors <- get_LambdasAndEigenVectors(X)$vectors
  mainComponents <- list()
  
  for (i in 1:dims) {
    mainComponents[[i]] <- XCR %*% vectors[,i]
  }
  return(mainComponents)
}

# Affichage sous forme de graphique selon F1 et F2
set_graph <- function(X){
  myData <- get_F(X)
  x <- vector()
  y <- vector()
  for (i in 1:length(myData[[1]])) {
    x <- append(x, myData[[1]][i,])
    y <- append(y, myData[[2]][i,])
  }
  
  # Avec labels
  df <- data.frame(x, y, z=myDataset.CleanFrance[,1])
  plot(df$x, df$y, xlab = "F1", ylab = "F2", xlim = c(-10, 10), ylim = c(-6, 6), col = "red", pch = 20)
  text(df$x, df$y-1, labels=df$z)
  draw.radial.line(-10, 10, center=c(0,0))
  draw.radial.line(-6, 6, center=c(0,0), angle=pi/2)
  
  # Sans labels
  plot(x,y, xlab = "F1", ylab = "F2", xlim = c(-10, 10), ylim = c(-6, 6), col = "red", pch = 20)
  draw.radial.line(-10, 10, center=c(0,0))
  draw.radial.line(-6, 6, center=c(0,0), angle=pi/2)
}

# Calcul des coordonnées sur le cercle des corrélations
get_G <- function(X){
  lambdas <- get_LambdasAndEigenVectors(X)$lambdas
  vectors <- get_LambdasAndEigenVectors(X)$vectors
  dims <- get_InertiaAxes(X)$dims
  coorVar <- list()
  
  for (i in 1:dims) {
    coorVar[[i]] <- sqrt(lambdas[i]) * vectors[,i]
  }
  return(coorVar)
}

# Affichage sur le cercle de corrélations des variables
set_CorCircle <- function(X){
  myData <- get_G(X)
  x <- vector()
  y <- vector()
  for (i in 1:length(myData[[1]])) {
    x <- append(x, myData[[1]][i])
    y <- append(y, myData[[2]][i])
  }
  
  # Avec labels
  df <- data.frame(x, y, z=names(myDataset.CleanFrance[,2:ncol(myDataset.CleanFrance)]))
  plot(df$x, df$y, xlim = c(-2, 2), ylim = c(-2, 2), col = "blue", pch = 20)
  text(df$x, df$y-1, labels=df$z)
  draw.circle(0,0,1)
  draw.radial.line(-1, 1, center=c(0,0))
  draw.radial.line(-1, 1, center=c(0,0), angle=pi/2)
  
  # Sans labels
  plot(x, y, xlim = c(-2, 2), ylim = c(-2, 2), col = "blue", pch = 20)
  draw.circle(0,0,1)
  draw.radial.line(-1, 1, center=c(0,0))
  draw.radial.line(-1, 1, center=c(0,0), angle=pi/2)
}



dataSet <- myDataset.CleanFrance[,2:ncol(myDataset.CleanFrance)]
get_XCR(dataSet)
get_D(dataSet)
get_S(dataSet)
get_LambdasAndEigenVectors(dataSet)
get_SumLambdasValues(dataSet)
get_InertiaAxes(dataSet)
get_F(dataSet)
set_graph(dataSet)
get_G(dataSet)
set_CorCircle(dataSet)