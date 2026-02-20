setwd("C:/Users/alexe/Fringilids")

# Chargement des packages ####

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


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

# Si on veut "tidyr" le dataframe (C'est-à-dire le mettre en une seule colonne)

abond <- abond %>% 
  gather(abond, key = "Espece", DUSA:TAPI)  # Fct. du package "tidyr"

abond # Dataframe en une seule colonne
      # 120 observations (1 abondance/année x 30 ans x 4 espèces)

abond <- abond %>% 
  mutate(abond_std = abond/Effort) # Ajout de l'effort standardisé

abond$abond_std <- as.numeric(abond$abond_std)
abond$Espece <- as.factor(abond$Espece)

str(abond)

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


# Graphique avec l'effort standardisé donne exactement la même chose


# DUSA

plot_dusa_tot <- ggplot(abond, aes(x = Annee, y = DUSA))+
  geom_point()+
  geom_line()+
  labs(title = "Abondance du DUSA par année",
       x = "Année",
       y = "N. individus recensés")+
  theme_classic()
plot_dusa_tot

xtabs(Effort ~ Annee, data = abond)






















