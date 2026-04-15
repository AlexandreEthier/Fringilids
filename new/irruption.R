# Chargements des bibliothèques -------------------------------------------

library(readxl)



# Chargement des répertoires de travail -----------------------------------

setwd("C:/Users/alexe/Fringilids/Data") # Alex



# Chargement données ------------------------------------------------------

DUSA <- read.csv("DUSA.csv", header = TRUE)
JABO <- read.csv("JABO.csv", header = TRUE)
SIFL <- read.csv("SIFL.csv", header = TRUE)
TAPI <- read.csv("TAPI.csv", header = TRUE)

# Filtrer tout avant 2007

DUSA <- DUSA[DUSA$Annee >= "2007",]
JABO <- JABO[JABO$Annee >= "2007",]
SIFL <- SIFL[SIFL$Annee >= "2007",]
TAPI <- TAPI[TAPI$Annee >= "2007",]

str(DUSA)


# Calcul des irruptions ---------------------------------------------------

# Article de Widck et al. (2023)

# We defined irruption years using the standardized deviate (Di,j) method, following LaMontagne and Boutin (2009). Standardized deviates were defined by

# Ni,j,t is the mean count of species i in cell j in year t, 
# Pi,j,t is the value predicted by the long-term trend in bird count for species i in cell j in year t, and 
# σi,j is the standard deviation for all detrended years for species i in cell j. The numerator in this expression detrends the time series and the denominator scales to unit standard deviation. Following LaMontagne and Boutin (2009), years with positive standardized deviates greater than the absolute value of the minimum deviate were considered anomalously high and indicative of irruption years.

# N: c'est la moyenne pour une année
# P: c'est la valeur prédite à long terme d'une espèce à une année précise. Notre valeur prédite correspond au valeur prédite du modèle linéaire
# Sigma: c'est l'ecart-type du numérateur

# On prend la valeur la plus petite de D (exemple -2.1) en valeur absolue comme seuil (2.1). Si D d'une année dépasse ce seuil (ex: D = 3.4 en 2016), alors 2016 est une année irruptive. 
















# Faire du VECTEUR IFELSE_IRR (1,0) un objet.R
# Load object.R in data_clean.R
# cbind(IFELSE_IRR)
# write.csv in wd










