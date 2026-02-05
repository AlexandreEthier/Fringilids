#Librarie
library(readxl)
library(dplyr)
library(ggplot2)
# Importe le jeu de données
bague <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Baguage.xlsx")
bague <- read_excel("C:/Users/alexe/Fringilids/Data/Baguage.xlsx")

#Créer une colonne "Condition" pour avoir le ratio (masse/aile)
bague_modifie <- bague %>%
  mutate('Condition' = (Masse / Aile))
str(bague_modifie)
unique(bague_modifie$Espèce)

# Modifier les noms
bague_modifie$Espèce[tolower(bague_modifie$Espèce) == "durbec des sapins"] <- "Durbec des sapins"
bague_modifie$Espèce[tolower(bague_modifie$Espèce) == "tarin des pins"] <- "Tarin des pins"
bague_modifie$Espèce[tolower(bague_modifie$Espèce) == "sizerin flammé"] <- "Sizerin flammé"

# Visualisation condition selon les années
ggplot(bague_modifie, aes(x = Année, y = Condition, color = Espèce)) +
  geom_point(alpha = 0.6) + # ajoute les points
  geom_smooth(se = FALSE) + #ajoute ligne pour suivre tendance
  theme_minimal() +
  labs(
    title = "Condition en fonction des années",
    y = "Condition (Masse / Aile)",
    color = "Espèce"
  )

# Moyenne de masse corporelle

bague_modifie |>
  group_by(Espèce) |>
  summarise(
    masse_moyenne = mean(Masse, na.rm = TRUE),
    ecart_type = sd(Masse, na.rm = TRUE))


