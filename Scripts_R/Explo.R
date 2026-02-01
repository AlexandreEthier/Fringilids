setwd("C:/Users/alexe/Fringilids")

# Chargement des packages ####

library(readxl)
library(dplyr)
library(ggplot2)




################################################################################
# EXPLORATION DES DONNÉES
################################################################################

# ABONDANCE #####

# Chargement des données

abond <- read_excel("C:/Users/alexe/Fringilids/Data/Abondance.xlsx") %>% # Standardisation des en-têtes -> Sans espace, sans accents
  rename(Annee = "Année", 
         DUSA = "Durbec des sapins", 
         JABO = "Jaseur boréal", 
         SIFL = "Sizerin flammé",
         TAPI = "Tarin des pins")

str(abond)

abond$Annee <- as.factor(abond$Annee)

# Visualisation graphique

abond <- abond %>% 
  group_by(Annee) %>% 
  mutate(Ntot = sum(c(DUSA, JABO, SIFL, TAPI))) %>% 
  ungroup()

ggplot(abond, aes(x = Annee, y = Ntot))+
  geom_col(fill = "black")+
  geom_text(aes(label = Effort),
            vjust = -1,
            colour = "black")+
  labs(title = "Abondance totale des espèces cibles par année",
       x = "Année",
       y = "N. individus recensés")+
  theme_classic()





# BAGUAGE ####

# Chargement des données

bag <- read_excel("C:/Users/alexe/Fringilids/Data/Baguage.xlsx") %>% 
  rename(Prefixe = "Préfixe",
         Espece = "Espèce",
         Abreviation = "Espèce (abréviation)",
         Age = "Âge",
         Annee = "Année",
         Municipalite = "Municipalité")





















