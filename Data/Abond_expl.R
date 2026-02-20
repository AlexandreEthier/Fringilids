#Librarie
library(readxl)
library(dplyr)
library(ggplot2)
install.packages("tidyr")
library(tidyr)
# Importe le jeu de données
abond <- read_excel("Abondance.xlsx")
summary(abond)

# Créer colonne abondance
abond2 <- abond %>%
  pivot_longer(
    cols = c(`Jaseur boréal`, `Sizerin flammé`, 
             `Tarin des pins`, `Durbec des sapins`),
    names_to = "Espece",
    values_to = "Abondance"
  )
summary(abond2)

# Visualisation condition selon les années
ggplot(abond2, aes(x = Année, y = `Abondance`, color= Espece)) +
  geom_point(alpha = 0.6) + # ajoute les points
  theme_minimal() +
  labs(
    title = "Abondance fringilidés en fonction des années",
    y = "Abondance", 
    color = "Espece"
  )  
