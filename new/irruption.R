# Chargements des bibliothèques -------------------------------------------

library(readxl)
library(ggplot2)


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

DUSA$irruption <- as.factor(DUSA$irruption)
JABO$irruption <- as.factor(JABO$irruption)
SIFL$irruption <- as.factor(SIFL$irruption)
TAPI$irruption <- as.factor(TAPI$irruption)


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


##### DUSA

# Extraction param. équation

DUSA_Ni <- DUSA$abond_std
DUSA_P <- mean(DUSA$abond_std)

DUSA_Nipi <- DUSA_Ni - DUSA_P # Numérateur

DUSA_sigma <- sd(DUSA_Nipi) # Sigma

DUSA_Di <- DUSA_Nipi/DUSA_sigma # Standard deviate

DUSA_seuil <- abs(min(DUSA_Di)) # Seuil fixé pour irruption

DUSA_irr <- DUSA_Di > DUSA_seuil # Vecteur logique TRUE or FALSE pour irruption

# Save object to load in data_clean.R
# save(DUSA_irr, file = "DUSA_irr.R")

# Représentation graphique

DUSA_plot <- ggplot(DUSA, aes(x = Annee, y = abond_std, color = irruption))+
  scale_color_manual(values = c( "1" = "orange", "0" = "black"))+
  geom_point(size = 4)+
  geom_path(linewidth = 1, col = "#a7a7a7")+
  labs(title = "Abondance standardisée annuelle du Durbec des sapins pour les années irruptives VS non-irruptives",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  geom_hline(yintercept = mean(DUSA$abond_std), col = "red", lty = 2)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
DUSA_plot


# DEVIATE BARPLOTS

range(DUSA_Di)

cols <- c("black", "orange")
DUSA_col <- ifelse(DUSA_Di > 0.965, cols[2],
                                    cols[1])

DUSA_Diplot <- barplot(DUSA_Di, DUSA$Annee,
                       xlab = "Année",
                       ylab = "Déviation standard",
                       col = DUSA_col,
                       main = "Déviation standard du Durbec des sapins")
abline(h = 0.965, col = "red", lty = 2)



##### JABO

JABO_Ni <- JABO$abond_std
JABO_P <- mean(JABO$abond_std)

JABO_Nipi <- JABO_Ni - JABO_P # Numérateur

JABO_sigma <- sd(JABO_Nipi) # Sigma

JABO_Di <- JABO_Nipi/JABO_sigma # Standard deviate

JABO_seuil <- abs(min(JABO_Di)) # Seuil fixé pour irruption

JABO_irr <- JABO_Di > JABO_seuil # Vecteur logique TRUE or FALSE pour irruption

# Save object to load in data_clean.R
# save(JABO_irr, file = "JABO_irr.R")

# Représentation graphique

JABO_plot <- ggplot(JABO, aes(x = Annee, y = abond_std, color = irruption))+
  scale_color_manual(values = c("1" = "#009929", "0" = "black"))+
  geom_point(size = 4)+
  geom_path(linewidth = 1, col = "#a7a7a7")+
  labs(title = "Abondance standardisée annuelle du Jaseur boréal pour les années irruptives VS non-irruptives",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  geom_hline(yintercept = mean(JABO$abond_std), col = "red", lty = 2)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
JABO_plot


# DEVIATE BARPLOTS

range(JABO_Di)

cols <- c("black", "#009929")
JABO_col <- ifelse(JABO_Di > 1.053, cols[2],
                   cols[1])

JABO_Diplot <- barplot(JABO_Di, JABO$Annee,
                       xlab = "Année",
                       ylab = "Déviation standard",
                       col = JABO_col,
                       main = "Déviation standard du Durbec des sapins")
abline(h = 1.053, col = "red", lty = 2)


##### SIFL

SIFL_Ni <- SIFL$abond_std
SIFL_P <- mean(SIFL$abond_std)

SIFL_Nipi <- SIFL_Ni - SIFL_P # Numérateur

SIFL_sigma <- sd(SIFL_Nipi) # Sigma

SIFL_Di <- SIFL_Nipi/SIFL_sigma # Standard deviate

SIFL_seuil <- abs(min(SIFL_Di)) # Seuil fixé pour irruption

SIFL_irr <- SIFL_Di > SIFL_seuil # Vecteur logique TRUE or FALSE pour irruption

# Save object to load in data_clean.R
# save(SIFL_irr, file = "SIFL_irr.R")

# Représentation graphique

SIFL_plot <- ggplot(SIFL, aes(x = Annee, y = abond_std, color = irruption))+
  scale_color_manual(values = c("1" = "turquoise", "0" = "black"))+
  geom_point(size = 4)+
  geom_path(linewidth = 1, col = "#a7a7a7")+
  labs(title = "Abondance standardisée annuelle du Sizerin flammé pour les années irruptives VS non-irruptives",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  geom_hline(yintercept = mean(SIFL$abond_std), col = "red", lty = 2)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
SIFL_plot

# DEVIATE BARPLOT

range(SIFL_Di)

cols <- c("black", "turquoise")
SIFL_col <- ifelse(SIFL_Di > 0.495, cols[2],
                   cols[1])

SIFL_Diplot <- barplot(SIFL_Di, SIFL$Annee,
                       xlab = "Année",
                       ylab = "Déviation standard",
                       col = SIFL_col,
                       main = "Déviation standard du Durbec des sapins")
abline(h = 0.495, col = "red", lty = 2)


##### TAPI

TAPI_Ni <- TAPI$abond_std
TAPI_P <- mean(TAPI$abond_std)

TAPI_Nipi <- TAPI_Ni - TAPI_P # Numérateur

TAPI_sigma <- sd(TAPI_Nipi) # Sigma

TAPI_Di <- TAPI_Nipi/TAPI_sigma # Standard deviate

TAPI_seuil <- abs(min(TAPI_Di)) # Seuil fixé pour irruption

TAPI_irr <- TAPI_Di > TAPI_seuil # Vecteur logique TRUE or FALSE pour irruption

# Save object to load in data_clean.R
# save(TAPI_irr, file = "TAPI_irr.R")

# Représentation graphique

TAPI_plot <- ggplot(TAPI, aes(x = Annee, y = abond_std, color = irruption))+
  scale_color_manual(values = c("1" = "violetred", "0" = "black"))+
  geom_point(size = 4)+
  geom_path(linewidth = 1, col = "#a7a7a7")+
  labs(title = "Abondance standardisée annuelle du Tarin des pins pour les années irruptives VS non-irruptives",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  geom_hline(yintercept = mean(TAPI$abond_std), col = "red", lty = 2)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
TAPI_plot


# DEVIATE BARPLOT

range(TAPI_Di)

cols <- c("black", "violetred")
TAPI_col <- ifelse(TAPI_Di > 0.525, cols[2],
                   cols[1])

TAPI_Diplot <- barplot(TAPI_Di, TAPI$Annee,
                       xlab = "Année",
                       ylab = "Déviation standard",
                       col = TAPI_col,
                       main = "Déviation standard du Durbec des sapins")
abline(h = 0.525, col = "red", lty = 2)








