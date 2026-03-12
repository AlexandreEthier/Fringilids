
# Chargement des packages -------------------------------------------------

#install.packages("cowplot")

library(cowplot)
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

abond <- read_excel("C:/Users/alexe/Fringilids/Data/Abondance.xlsx") %>% # Ajoutez votre propre chemin
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


# Création d'un dataframe pour chaque espèce

# DUSA
DUSA <- abond %>% 
  filter(Espece == "DUSA")

hist(DUSA$abond_std)

# TAPI
TAPI <- abond %>% 
  filter(Espece == "TAPI")

hist(TAPI$abond_std)

# SIFL
SIFL <- abond %>% 
  filter(Espece == "SIFL")

hist(SIFL$abond_std)

# JABO
JABO <- abond %>% 
  filter(Espece == "JABO")

hist(JABO$abond_std)


# Formatage - Baguage -----------------------------------------------------------------

bague <- read_excel("C:/Users/alexe/Fringilids/Data/Baguage.xlsx") %>% # Ajoutez votre propre chemin
  rename(Espece = "Espèce",
         Abrv = "Espèce (abréviation)",
         Age = "Âge",
         Annee = "Année")

head(bague)

str(bague)

xtabs(~ bague$Age)

xtabs(~Sexe, data = bague)

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

levels(bague$Age) # Les catégories existent encore
bague$Age <- droplevels(bague$Age) # Clean-up des levels

# Sexe
xtabs(~ Sexe, data = bague) # Standardiser sexe

bague$Sexe <- replace(bague$Sexe, bague$Sexe %in% "u", "U") # Unknown
bague$Sexe <- replace(bague$Sexe, bague$Sexe %in% "f", "F") # Femelle
bague$Sexe <- replace(bague$Sexe, bague$Sexe %in% "m", "M") # Mâle

levels(bague$Sexe)
bague$Sexe <- droplevels(bague$Sexe)


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



plot_log <- ggplot(abond, aes(x = Annee, y = log(abond_std), group = Espece, color = Espece))+
  geom_point(size = 3)+
  geom_line(linewidth = 1.5)+
  scale_y_continuous(limits = c(0, 7), n.breaks = 15)+
  labs(title = "Abondance standardisée des espèces cibles par année",
       x = "Année",
       y = "N. d'oiseaux/h",
       color = "Espèce")+
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_log


DUSA <- abond$abond_std[abond$Espece == "DUSA"]
DUSA$Annee <- cbind(abond$Annee)






lm_DUSA <- lm(abond$abond_std[abond$Espece == "DUSA"] ~ Annee, data = abond)




# Graphique condition/espèce/année

plot_condition <- ggplot(bague, aes(x = Annee, y = Condition, group = Espece, color = Espece))+
  geom_point(size = 3)+
  labs(title = "Abondance standardisée des espèces cibles par année",
       x = "Année",
       y = "N. d'oiseaux/h",
       color = "Espèce")+
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_condition

# Calcule la moyenne de la condition par espèce et par année
bague_moyenne <- bague_modifie %>%
  group_by(Espèce, Année) %>%          # Regroupe par espèce et année
  summarise(Condition_moyenne = mean(Condition, na.rm = TRUE),
            Condition_sd = sd(Condition, na.rm = TRUE),  
            Condition_se = Condition_sd / sqrt(n()))  


plot_condition <- ggplot(bague_moyenne, aes(x = Année, y = Condition_moyenne, group = Espèce, color = Espèce)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +  
  labs(title = "Condition moyenne par espèce et par année",
       x = "Année",
       y = "Condition moyenne",
       color = "Espèce") +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    panel.background = element_blank()
  )+
  geom_errorbar(aes(ymin = Condition_moyenne - Condition_se, ymax = Condition_moyenne + Condition_se), width = 0.2)

print(plot_condition) # ne parait pas beaucoup évoluer dans le temps


# DUSA (Adrien) -----------------------------------------------------------

plot_dusa_tot <- ggplot(abond[abond$Espece=="DUSA",], aes(x = Annee, y = abond_std, group =1))+
  geom_point(size = 3, col = "darkorange1")+
  geom_path(linewidth = 1, col = "orange")+
  labs(title = "Abondance du DUSA par heure d'observation",
       x = "Année",
       y = "N. individus recensés")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_dusa_tot


plot_condition_DUSA <- ggplot(bague_moyenne[bague_moyenne$Espèce=="DURBEC DES SAPINS",], aes(x = Année, y = Condition_moyenne, group=1)) +
  geom_point(size = 3, col = "darkorange1")+
  geom_path(linewidth = 1, col = "orange")+  
  labs(title = "Condition moyenne du durbec des sapins par année",
       x = "Année",
       y = "Condition moyenne") +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    panel.background = element_blank()
  )+
  geom_errorbar(aes(ymin = Condition_moyenne - Condition_se, ymax = Condition_moyenne + Condition_se), width = 0.2, col="orange")
print(plot_condition_DUSA) # il y a un trou entre 97-99, 99-01 et 2001-2007

plot_dusa_tot <- ggplot(abond[abond$Espece=="DUSA",], aes(x = Annee, y = abond_std, group =1))+
  geom_point(size = 3, col = "darkorange1")+
  geom_path(linewidth = 1, col = "orange")+
  labs(title = "Abondance du DUSA par heure d'observation",
       x = "Année",
       y = "N. individus recensés")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_dusa_tot

# combine les deux graphique :
plot_grid(plot_dusa_tot, plot_condition_DUSA, ncol = 1) #graphique bof



# JABO (Bérince) ----------------------------------------------------------

plot_jabo_tot <- ggplot(abond[abond$Espece=="JABO",], aes(x = Annee, y = abond_std, group =1))+
  geom_point(size = 3, col = "darkgreen")+
  geom_path(linewidth = 1, col = "chartreuse4")+
  labs(title = "Abondance du JABO par heure d'observation",
       x = "Année",
       y = "N. individus recensés")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_jabo_tot



# SIFL (Alex) -------------------------------------------------------------

SIFL <- abond[abond$Espece == "SIFL",]
SIFL

SIFL <- SIFL %>% 
  group_by(Annee) %>% 
  filter(between(Annee, 2007, 2025))

bague_SIFL <- bague[bague$Espece == "Sizerin flammé",] # Ne garde que SIFL
bague_SIFL <- bague_SIFL[bague_SIFL$Age != "U",] # Ne garde que les individus âgés

bague_SIFL <- bague_SIFL %>% # Regroupement des oiseaux bagués/année
  group_by(Annee) %>% 
  filter(between(Annee, 2007, 2025)) # Baguage plus régulier à partir de 2006 (ou 2007, à vérifier)

plot_sifl_tot <- ggplot(SIFL, aes(x = Annee, y = abond_std, group = 1))+
  geom_point(size = 3, col = "turquoise4")+
  geom_path(linewidth = 1, col = "turquoise3")+
  labs(title = "Abondance du SIFL par heure d'observation",
       x = "Année",
       y = "N. individus recensés * heure-1")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_sifl_tot

# Proportion d'individus bagués par année 

bague_annuel_SIFL <- bague_SIFL %>% # Nb. de SIFL bagués / année
  summarise(n())

ann <- seq(2007, 2025, 1) # Création d'un vecteur peuplé de 0

n_bague_ann_SIFL <- as.data.frame(ann) %>% 
  rename(Annee = "ann") %>% 
  left_join(bague_annuel_SIFL, by = "Annee") %>% 
  cbind(n_bague_ann_SIFL$`n()`/SIFL$abond) %>%            # Calcul la proportion de SIFL bagué par année
  rename(prop_bague = "n_bague_ann_SIFL$`n()`/SIFL$abond")


# Condition jeunes vs adultes par année

ggplot(bague_SIFL, aes(x = Annee, y = Condition, group = interaction(Annee, Age)))+
  geom_boxplot(aes(fill = Age))+
  theme_classic()



# TAPI (Maxence) ----------------------------------------------------------

plot_tapi_tot <- ggplot(abond[abond$Espece=="TAPI",], aes(x = Annee, y = abond_std, group =1))+
  geom_point(size = 3, col = "violetred4")+
  geom_path(linewidth = 1, col = "violetred3")+
  labs(title = "Abondance du TAPI par heure d'observation",
       x = "Année",
       y = "N. individus recensés")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_tapi_tot


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



# Analyse -----------------------------------------------------------------

# Combine les 4 graphiques en 2x2
plot_grid(plot_dusa_tot, plot_jabo_tot, plot_sifl_tot, plot_tapi_tot, ncol = 2)




# TENDANCE TEMPORELLE -----------------------------------------------------

abond

# Tendance temporelle depuis 1996 - 2025





# SIFL


# TAPI


# DUSA



# JABO













