
# Chargement des packages -------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Chargement des données ---------------------------------------------------

# IMPORTANT - Spécifier votre propre chemin qui mène aux documents
# IMPORTANT - SVP changez les virgules (,) en points (.) à même les deux fichiers Excel

# Maxence:/Users/maxencepoirier-joanette/Rstudio/FOR7046/Abondance.xlsx
#         /Users/maxencepoirier-joanette/Rstudio/FOR7046/Baguage.xlsx

# Alex: C:/Users/alexe/Fringilids/Data/Abondance.xlsx
#       C:/Users/alexe/Fringilids/Data/Baguage.xlsx
setwd("C:/Users/alexe/Fringilids")

# Bérince:
#

# Adrien:
#


# Formatage - Abondance ---------------------------------------------------------------

abond <- read_excel("") %>% # Ajoutez votre propre chemin
  rename(Annee = "Année", 
         DUSA = "Durbec des sapins", 
         JABO = "Jaseur boréal", 
         SIFL = "Sizerin flammé",
         TAPI = "Tarin des pins")

abond <- abond %>% 
  gather(abond, key = "Espece", DUSA:TAPI) %>%   # Dataframe en une seule colonne
  mutate(abond_std = abond/Effort)               # Ajout de l'abondance standardisée
                                                 
abond$Annee <- as.integer(abond$Annee)
abond$Effort <- as.integer(abond$Effort)
abond$abond_std <- as.numeric(abond$abond_std)
abond$Espece <- as.factor(abond$Espece)

str(abond)


# Formatage - Baguage -----------------------------------------------------------------

bague <- read_excel("") %>% # Ajoutez votre propre chemin
  rename(Espece = "Espèce",
         Abrv = "Espèce (abréviation)",
         Age = "Âge",
         Annee = "Année")

head(bague)

str(bague)

bague$Age <- as.factor(bague$Age)
bague$Sexe <- as.factor(bague$Sexe)
bague$Aile <- as.numeric(bague$Aile)
bague$Gras <- as.factor(bague$Gras)
bague$Queue <- as.numeric(bague$Queue)
bague$Masse <- as.numeric(bague$Masse)
bague$Annee <- as.integer(bague$Annee)

# Vérification des colonnes de la bd

# Noms d'espèce
xtabs(~ Espece, data = bague) # Standardiser les noms

bague$Espece <- replace(bague$Espece, bague$Espece %in% "DURBEC DES SAPINS", "Durbec des sapins")
bague$Espece <- replace(bague$Espece, bague$Espece %in% "TARIN DES PINS", "Tarin des pins")
bague$Espece <- replace(bague$Espece,  bague$Espece %in% "SIZERIN FLAMMÉ", "Sizerin flammé")


# Abréviation
xtabs(~ Abrv, data = bague) # Standardiser TAPI

bague$Abrv <- replace(bague$Abrv, bague$Abrv %in% "tapi", "TAPI")


# Âge
xtabs(~ Age, data = bague) # Pooler les âges (HY vs AHY)

bague$Age <- replace(bague$Age, bague$Age %in% c("Local", "S"), "U")  # Unknown
bague$Age <- replace(bague$Age, bague$Age %in% "hy", "HY") # Juv (Hatch year)
bague$Age <- replace(bague$Age, bague$Age %in% c("SY", "ASY", "ahy"), "AHY") # Non-juv (After-Hatch Year)


# Sexe
xtabs(~ Sexe, data = bague) # Standardiser sexe

bague$Sexe <- replace(bague$Sexe, bague$Sexe %in% "u", "U") # Unknown
bague$Sexe <- replace(bague$Sexe, bague$Sexe %in% "f", "F") # Femelle
bague$Sexe <- replace(bague$Sexe, bague$Sexe %in% "m", "M") # Mâle


# Site
xtabs(~ Site, data = bague) # Exclusion des sites != "Dunes"

bague <- bague %>% 
  filter(Site == "Dunes") # Sélection des sites de capture aux Dunes de Tadoussac


# Manipulation df

bague <- bague %>% 
  select(-Préfixe, -Suffixe, -Site, - Municipalité) %>%  # Retrait des colonnes non nécessaires
  filter(Aile != "NA") %>%                               # Retrait des données manquantes pour "Aile"
  filter(Masse != "NA") %>%                              # Retrait des données manquantes pour "Masse"
  mutate(Condition = (Aile/Masse))                       # Indice de condition standardisé

str(bague) # Tout propre


# Exploration des données -------------------------------------------------

# ABONDANCE

# Visualisation graphique de l'abondance des 4 espèces en fonction des années

plot_ab <- ggplot(abond, aes(x = Annee, y = abond, group = Espece, color = Espece))+
  geom_point()+
  geom_line()+
  labs(title = "Abondance des espèces cibles par année",
       x = "Année",
       y = "N. d'oiseaux",
       color = "Espèce")+
  theme_classic()
plot_ab


# Visualisation graphique de l'abondance STANDARDISÉE des 4 espèces en fonction des années

plot_std <- ggplot(abond, aes(x = Annee, y = abond_std, group = Espece, color = Espece))+
  geom_point(size = 3)+
  geom_line(linewidth = 1.5)+
  scale_y_continuous(limits = c(0, 600), n.breaks = 15)+
  labs(title = "Abondance standardisée des espèces cibles par année",
       x = "Année",
       y = "N. d'oiseaux/h",
       color = "Espèce")+
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_std


# DUSA (Adrien) -----------------------------------------------------------







# JABO (Bérince) ----------------------------------------------------------








# SIFL (Alex) -------------------------------------------------------------


SIFL <- abond[abond$Espece == "SIFL",]
SIFL

plot_sifl_tot <- ggplot(SIFL, aes(x = Annee, y = abond_std, group = 1))+
  geom_point()+
  geom_line()+
  labs(title = "Abondance standardisée du SIFL par année",
       x = "Année",
       y = "N. individus recensés * heure-1")+
  theme_classic()
plot_sifl_tot



# TAPI (Maxence) ----------------------------------------------------------

# Créer le dataframe NAO complet
# Variable explicative
# Créer le dataframe
nao_data <- data.frame(
  Annee = c(rep(1950, 12), rep(1951, 12), rep(1952, 12), rep(1953, 12), rep(1954, 12),
            rep(1955, 12), rep(1956, 12), rep(1957, 12), rep(1958, 12), rep(1959, 12),
            rep(1960, 12), rep(1961, 12), rep(1962, 12), rep(1963, 12), rep(1964, 12),
            rep(1965, 12), rep(1966, 12), rep(1967, 12), rep(1968, 12), rep(1969, 12),
            rep(1970, 12), rep(1971, 12), rep(1972, 12), rep(1973, 12), rep(1974, 12),
            rep(1975, 12), rep(1976, 12), rep(1977, 12), rep(1978, 12), rep(1979, 12),
            rep(1980, 12), rep(1981, 12), rep(1982, 12), rep(1983, 12), rep(1984, 12),
            rep(1985, 12), rep(1986, 12), rep(1987, 12), rep(1988, 12), rep(1989, 12),
            rep(1990, 12), rep(1991, 12), rep(1992, 12), rep(1993, 12), rep(1994, 12),
            rep(1995, 12), rep(1996, 12), rep(1997, 12), rep(1998, 12), rep(1999, 12),
            rep(2000, 12), rep(2001, 12), rep(2002, 12), rep(2003, 12), rep(2004, 12),
            rep(2005, 12), rep(2006, 12), rep(2007, 12), rep(2008, 12), rep(2009, 12),
            rep(2010, 12), rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12),
            rep(2015, 12), rep(2016, 12), rep(2017, 12), rep(2018, 12), rep(2019, 12),
            rep(2020, 12), rep(2021, 12), rep(2022, 12), rep(2023, 12), rep(2024, 12),
            rep(2025, 12), 2026),
  
  Mois = c(rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 76), "Jan"),
  
  NAO = c(
    # 1950
    0.92, 0.40, -0.36, 0.73, -0.59, -0.06, -1.26, -0.05, 0.25, 0.85, -1.26, -1.02,
    # 1951
    0.08, 0.70, -1.02, -0.22, -0.59, -1.64, 1.37, -0.22, -1.36, 1.87, -0.39, 1.32,
    # 1952
    0.93, -0.83, -1.49, 1.01, -1.12, -0.40, -0.09, -0.28, -0.54, -0.73, -1.13, -0.43,
    # 1953
    0.33, -0.49, -0.04, -1.67, -0.66, 1.09, 0.40, -0.71, -0.35, 1.32, 1.04, -0.47,
    # 1954
    0.37, 0.74, -0.83, 1.34, -0.09, -0.25, -0.60, -1.90, -0.44, 0.60, 0.40, 0.69,
    # 1955
    -1.84, -1.12, -0.53, -0.42, -0.34, -1.10, 1.76, 1.07, 0.32, -1.47, -1.29, 0.17,
    # 1956
    -0.22, -1.12, -0.05, -1.06, 2.21, 0.10, -0.75, -1.37, 0.24, 0.88, 0.51, 0.10,
    # 1957
    1.05, 0.11, -1.26, 0.49, -0.79, -0.72, -1.19, -0.55, -1.66, 1.32, 0.73, 0.12,
    # 1958
    -0.54, -1.06, -1.96, 0.37, -0.24, -1.38, -1.73, -1.56, -0.07, 0.16, 1.64, -0.70,
    # 1959
    -0.87, 0.68, -0.15, 0.36, 0.39, 0.40, 0.74, 0.06, 0.88, 0.89, 0.41, 0.44,
    # 1960
    -1.29, -1.89, -0.50, 1.36, 0.45, -0.21, 0.35, -1.40, 0.39, -1.73, -0.51, 0.06,
    # 1961
    0.41, 0.45, 0.55, -1.55, -0.36, 0.86, -0.39, 0.90, 1.24, 0.51, -0.62, -1.48,
    # 1962
    0.61, 0.55, -2.47, 0.99, -0.10, 0.16, -2.47, 0.14, -0.37, 0.41, -0.23, -1.32,
    # 1963
    -2.12, -0.96, -0.43, -1.35, 2.16, -0.43, -0.77, -0.64, 1.79, 0.94, -1.27, -1.92,
    # 1964
    -0.95, -1.43, -1.20, 0.36, 0.52, 1.29, 1.90, -1.77, 0.20, 0.74, -0.01, -0.15,
    # 1965
    -0.12, -1.55, -1.51, 0.72, -0.62, 0.29, 0.32, 0.45, 0.37, 0.38, -1.66, 1.37,
    # 1966
    -1.74, -1.39, 0.56, -0.75, 0.22, 1.05, 0.32, -1.76, -0.45, -0.68, -0.04, 0.72,
    # 1967
    -0.89, 0.19, 1.51, 0.18, -0.99, 1.40, 0.41, 1.44, 0.93, 0.07, 0.60, -0.45,
    # 1968
    0.13, -1.29, 0.40, -1.08, -1.76, 0.33, -0.80, -0.66, -1.92, -2.30, -0.93, -1.40,
    # 1969
    -0.83, -1.55, -1.56, 1.53, 0.55, 0.55, 0.57, -1.45, 2.07, 0.66, -0.96, -0.28,
    # 1970
    -1.50, 0.64, -0.96, -1.30, 1.14, 1.55, 0.10, 0.10, -0.09, -0.92, -0.60, -1.20,
    # 1971
    -1.13, 0.24, -0.84, -0.24, 0.50, -1.57, 0.24, 1.55, 0.39, 0.58, -0.20, 0.60,
    # 1972
    0.27, 0.32, 0.72, -0.22, 0.95, 0.88, 0.18, 1.32, -0.12, 1.09, 0.54, 0.19,
    # 1973
    0.04, 0.85, 0.30, -0.54, -0.44, 0.39, 0.57, -0.06, -0.30, -1.24, -0.93, 0.32,
    # 1974
    1.34, -0.14, -0.03, 0.51, -0.24, -0.14, -0.76, -0.64, 0.82, 0.49, -0.54, 1.50,
    # 1975
    0.58, -0.62, -0.61, -1.60, -0.52, -0.84, 1.55, -0.26, 1.56, -0.54, 0.41, 0.00,
    # 1976
    -0.25, 0.93, 0.75, 0.26, 0.96, 0.80, -0.32, 1.92, -1.29, -0.08, 0.17, -1.60,
    # 1977
    -1.04, -0.49, -0.81, 0.65, -0.86, -0.57, -0.45, -0.28, 0.37, 0.52, -0.07, -1.00,
    # 1978
    0.66, -2.20, 0.70, -1.17, 1.08, 1.38, -1.14, 0.64, 0.46, 1.93, 3.04, -1.57,
    # 1979
    -1.38, -0.67, 0.78, -1.71, -1.03, 1.60, 0.83, 0.96, 1.01, -0.30, 0.53, 1.00,
    # 1980
    -0.75, 0.05, -0.31, 1.29, -1.50, -0.37, -0.42, -2.24, 0.66, -1.77, -0.37, 0.78,
    # 1981
    0.37, 0.92, -1.19, 0.36, 0.20, -0.45, 0.05, 0.39, -1.45, -1.35, -0.38, -0.02,
    # 1982
    -0.89, 1.15, 1.15, 0.10, -0.53, -1.63, 1.15, 0.26, 1.76, -0.74, 1.60, 1.78,
    # 1983
    1.59, -0.53, 0.95, -0.85, -0.07, 0.99, 1.19, 1.61, -1.12, 0.65, -0.98, 0.29,
    # 1984
    1.66, 0.72, -0.37, -0.28, 0.54, -0.42, -0.07, 1.15, 0.17, -0.07, -0.06, 0.00,
    # 1985
    -1.61, -0.49, 0.20, 0.32, -0.49, -0.80, 1.22, -0.48, -0.52, 0.90, -0.67, 0.22,
    # 1986
    1.11, -1.00, 1.71, -0.59, 0.85, 1.22, 0.12, -1.09, -1.12, 1.55, 2.29, 0.99,
    # 1987
    -1.15, -0.73, 0.14, 2.00, 0.98, -1.82, 0.52, -0.83, -1.22, 0.14, 0.18, 0.32,
    # 1988
    1.02, 0.76, -0.17, -1.17, 0.63, 0.88, -0.35, 0.04, -0.99, -1.08, -0.34, 0.61,
    # 1989
    1.17, 2.00, 1.85, 0.28, 1.38, -0.27, 0.97, 0.01, 2.05, -0.03, 0.16, -1.15,
    # 1990
    1.04, 1.41, 1.46, 2.00, -1.53, -0.02, 0.53, 0.97, 1.06, 0.23, -0.24, 0.22,
    # 1991
    0.86, 1.04, -0.20, 0.29, 0.08, -0.82, -0.49, 1.23, 0.48, -0.19, 0.48, 0.46,
    # 1992
    -0.13, 1.07, 0.87, 1.86, 2.63, 0.20, 0.16, 0.85, -0.44, -1.76, 1.19, 0.47,
    # 1993
    1.60, 0.50, 0.67, 0.97, -0.78, -0.59, -3.18, 0.12, -0.57, -0.71, 2.56, 1.56,
    # 1994
    1.04, 0.46, 1.26, 1.14, -0.57, 1.52, 1.31, 0.38, -1.32, -0.97, 0.64, 2.02,
    # 1995
    0.93, 1.14, 1.25, -0.85, -1.49, 0.13, -0.22, 0.69, 0.31, 0.19, -1.38, -1.67,
    # 1996
    -0.12, -0.07, -0.24, -0.17, -1.06, 0.56, 0.67, 1.02, -0.86, -0.33, -0.56, -1.41,
    # 1997
    -0.49, 1.70, 1.46, -1.02, -0.28, -1.47, 0.34, 0.83, 0.61, -1.70, -0.90, -0.96,
    # 1998
    0.39, -0.11, 0.87, -0.68, -1.32, -2.72, -0.48, -0.02, -2.00, -0.29, -0.28, 0.87,
    # 1999
    0.77, 0.29, 0.23, -0.95, 0.92, 1.12, -0.90, 0.39, 0.36, 0.20, 0.65, 1.61,
    # 2000
    0.60, 1.70, 0.77, -0.03, 1.58, -0.03, -1.03, -0.29, -0.21, 0.92, -0.92, -0.58,
    # 2001
    0.25, 0.45, -1.26, 0.00, -0.02, -0.20, -0.25, -0.07, -0.65, -0.24, 0.63, -0.83,
    # 2002
    0.44, 1.10, 0.69, 1.18, -0.22, 0.38, 0.62, 0.38, -0.70, -2.28, -0.18, -0.94,
    # 2003
    0.16, 0.62, 0.32, -0.18, 0.01, -0.07, 0.13, -0.07, 0.01, -1.26, 0.86, 0.64,
    # 2004
    -0.29, -0.14, 1.02, 1.15, 0.19, -0.89, 1.13, -0.48, 0.38, -1.10, 0.73, 1.21,
    # 2005
    1.52, -0.06, -1.83, -0.30, -1.25, -0.05, -0.51, 0.37, 0.63, -0.98, -0.31, -0.44,
    # 2006
    1.27, -0.51, -1.28, 1.24, -1.14, 0.84, 0.90, -1.73, -1.62, -2.24, 0.44, 1.34,
    # 2007
    0.22, -0.47, 1.44, 0.17, 0.66, -1.31, -0.58, -0.14, 0.72, 0.45, 0.58, 0.34,
    # 2008
    0.89, 0.73, 0.08, -1.07, -1.73, -1.39, -1.27, -1.16, 1.02, -0.04, -0.32, -0.28,
    # 2009
    -0.01, 0.06, 0.57, -0.20, 1.68, -1.21, -2.15, -0.19, 1.51, -1.03, -0.02, -1.93,
    # 2010
    -1.11, -1.98, -0.88, -0.72, -1.49, -0.82, -0.42, -1.22, -0.79, -0.93, -1.62, -1.85,
    # 2011
    -0.88, 0.70, 0.61, 2.48, -0.06, -1.28, -1.51, -1.35, 0.54, 0.39, 1.36, 2.52,
    # 2012
    1.17, 0.42, 1.27, 0.47, -0.91, -2.53, -1.32, -0.98, -0.59, -2.06, -0.58, 0.17,
    # 2013
    0.35, -0.45, -1.61, 0.69, 0.57, 0.52, 0.67, 0.97, 0.24, -1.28, 0.90, 0.95,
    # 2014
    0.29, 1.34, 0.80, 0.31, -0.92, -0.97, 0.18, -1.68, 1.62, -1.27, 0.68, 1.86,
    # 2015
    1.79, 1.32, 1.45, 0.73, 0.15, -0.07, -3.18, -0.76, -0.65, 0.44, 1.74, 2.24,
    # 2016
    0.12, 1.58, 0.73, 0.38, -0.77, -0.43, -1.76, -1.65, 0.61, 0.41, -0.16, 0.48,
    # 2017
    0.48, 1.00, 0.74, 1.73, -1.91, 0.05, 1.26, -1.10, -0.61, 0.19, -0.00, 0.88,
    # 2018
    1.44, 1.58, -0.93, 1.24, 2.12, 1.09, 1.39, 1.97, 1.67, 0.93, -0.11, 0.61,
    # 2019
    0.59, 0.29, 1.23, 0.47, -2.62, -1.09, -1.43, -1.17, -0.16, -1.41, 0.28, 1.20,
    # 2020
    1.34, 1.26, 1.01, -1.02, -0.41, -0.15, -1.23, 0.12, 0.98, -0.65, 2.54, -0.30,
    # 2021
    -1.11, 0.14, 0.73, -1.43, -1.24, 0.77, 0.03, -0.28, -0.21, -2.29, -0.18, 0.29,
    # 2022
    1.08, 1.68, 0.77, -0.36, 0.71, -0.12, -0.09, 1.47, -1.61, -0.72, 0.69, -0.15,
    # 2023
    1.25, 0.92, -1.11, -0.63, 0.39, -0.58, -2.17, -1.16, -0.44, -2.03, -0.32, 1.94,
    # 2024
    0.21, 1.09, -0.21, -0.78, -0.44, -0.09, 1.46, 0.63, -1.43, -0.38, -0.23, 1.21,
    # 2025
    -0.52, 1.60, 0.30, 0.18, 0.49, 0.70, 0.48, 0.26, -0.80, -0.96, -0.78, -0.65,
    # 2026
    -0.36
  )
)


# Tâche #3: Voir si la condition physique des différentes classes est associée à l'abondance standardisé

# Avoir les données d'abondance avec condition
jointure <- left_join(
  bague_modifie, 
  abondance, 
  by = c("Année" = "Année") )

# D'abord filtrer pour garder seulement les Tarin
jointure_tarin <- jointure[jointure$Espèce == "Tarin des pins", ]

# Renommer la colonne
names(jointure_tarin)[names(jointure_tarin) == "Tarin des pins_std"] <- "Tarin_std"

#Conserver les colonnes utiles
jointure_tarin <- jointure_tarin[, c("Année", "Espèce", "Condition", "groupe", "Effort", "Tarin des pins_std")]

# Ensuite faire le modèle
mod <- lm(Condition ~ groupe + Année + Tarin_std, data=jointure_tarin)
summary(mod)

par(mfrow = c(2, 2))
plot(mod)

# Visuellement les conditions semblent respectés

# Tâche #4: Voir pour chaque année la proportion des classes selon les années





















