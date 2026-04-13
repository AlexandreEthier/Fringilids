
# Chargement des packages -------------------------------------------------

library(cowplot)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(nlme)
library(emmeans)
library(writexl)
library(AICcmodavg)

# Chargement des données ---------------------------------------------------

# IMPORTANT - Spécifier votre propre chemin qui mène aux documents
# IMPORTANT - SVP changez les virgules (,) en points (.) à même les deux fichiers Excel

# Maxence
abond <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Abondance.xlsx")
bague <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Baguage.xlsx")

#C:/Users/alexe/Fringilids/Data/Abondance.xlsx
bague <- read_excel("C:/Users/alexe/Fringilids/Data/Baguage.xlsx")

summary(bague)



bague <- bague %>%  
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


xtabs(~ Espece, data = bague)  #Standardiser les noms

bague$Espece <- replace(bague$Espece, bague$Espece %in% "DURBEC DES SAPINS", "Durbec des sapins")
bague$Espece <- replace(bague$Espece, bague$Espece %in% "TARIN DES PINS", "Tarin des pins")
bague$Espece <- replace(bague$Espece,  bague$Espece %in% "SIZERIN FLAMMÉ", "Sizerin flammé")


# Abréviation
xtabs(~ Abrv, data = bague)  #Standardiser TAPI

bague$Abrv <- replace(bague$Abrv, bague$Abrv %in% "tapi", "TAPI")


# Âge
xtabs(~ Age, data = bague)  #Pooler les âges (HY vs AHY)

bague$Age <- replace(bague$Age, bague$Age %in% c("Local", "S"), "U")   #Unknown
bague$Age <- replace(bague$Age, bague$Age %in% "hy", "HY")  #Juv (Hatch year)
bague$Age <- replace(bague$Age, bague$Age %in% c("SY", "ASY", "ahy"), "AHY")  #Non-juv (After-Hatch Year)

bague <- bague %>% 
  filter(Age != "U")

# Sexe
xtabs(~ Sexe, data = bague)  #Standardiser sexe

bague$Sexe <- replace(bague$Sexe, bague$Sexe %in% "u", "U")  #Unknown
bague$Sexe <- replace(bague$Sexe, bague$Sexe %in% "f", "F")  #Femelle
bague$Sexe <- replace(bague$Sexe, bague$Sexe %in% "m", "M")  #Mâle

# Site
xtabs(~ Site, data = bague)  #Exclusion des sites != "Dunes"

bague <- bague %>% 
  filter(Site == "Dunes")  #Sélection des sites de capture aux Dunes de Tadoussac

 #Manipulation df

bague <- bague %>% 
  select(-Préfixe, -Suffixe, -Site, - Municipalité, -Queue, - Gras, -Date, -Espece, -Sexe) %>%  # Retrait des colonnes non nécessaires
  filter(!Annee %in% c(1997,1998, 1999, 2000, 2006))%>%   #Retrait des années avant 2007
  mutate(Condition = (Aile/Masse))                        #Indice de condition standardisé

bague <- bague %>% 
  filter(Age != "U") %>% 
  rename(Espece = "Abrv")

#write_xlsx(bague, "bague_clean.xlsx")



# Jeu de données propre ---------------------------------------------------

# Importer Excel

#Adrien:

abond <- read_excel("abond_clean.xlsx")
bague <- read_excel("bague_clean.xlsx")

#Alex:
abond <- read_excel("C:/Users/alexe/Fringilids/Data/abond_clean.xlsx")
bague <- read_excel("C:/Users/alexe/Fringilids/Data/bague_clean.xlsx")

#Bérince:

#Maxence:
abond <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Fringilids/Data/abond_clean.xlsx")
bague <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Fringilids/Data/bague_clean.xlsx")


# Création d'un dataframe pour chaque espèce pour abondance ------------------------------

# DUSA (Durbec des sapins)
DUSA <- abond %>% 
  filter(Espece == "DUSA")

DUSA$Annee <- as.factor(as.numeric(DUSA$Annee))
DUSA$Annee <- as.integer(DUSA$Annee)

#hist(DUSA$abond_std)

# TAPI (Tarin des Pins)
TAPI <- abond %>% 
  filter(Espece == "TAPI")

TAPI$Annee <- as.factor(as.numeric(TAPI$Annee))
TAPI$Annee <- as.integer(TAPI$Annee)

#hist(TAPI$abond_std)

# SIFL (Sizerin flammé)
SIFL <- abond %>% 
  filter(Espece == "SIFL")

SIFL$Annee <- as.factor(as.numeric(SIFL$Annee))
SIFL$Annee <- as.integer(SIFL$Annee)

#hist(SIFL$abond_std)

# JABO (Jaseur Boréal)
JABO <- abond %>% 
  filter(Espece == "JABO")

JABO$Annee <- as.factor(as.numeric(JABO$Annee))
JABO$Annee <- as.integer(JABO$Annee)

#hist(JABO$abond_std)


# Calcul des proportions pour chaque espèce -------------------------------

props_all <- bague %>%
  group_by(Espece, Annee) %>% # par Abrv et Annee
  summarise(
    nb_HY = sum(Age == "HY"), # somme des jeunes
    nb_AHY = sum(Age == "AHY"), # somme des adultes
    prop_jeunes = nb_HY / (nb_HY + nb_AHY), #proportion de jeunes
    .groups = "drop"
  )

props_all

# Jointure avec abond
abond_joint <- abond %>%
  left_join(props_all, by = c("Espece" = "Espece", "Annee" = "Annee")) %>% 
  mutate(nb_total = nb_HY + nb_AHY) %>% 
  filter(!Annee %in% c(1996,1997,1998, 1999, 2000,2001,2002,2003,2004,2005, 2006))

# Calcul la moyenne de la condition par espèce et par année ---------------


bague <- bague %>% 
  group_by(Espece, Annee) %>% 
  mutate(Condition_moyenne = mean(Condition, na.rm = TRUE),
         Condition_sd = sd(Condition, na.rm = TRUE),  
         Condition_se = Condition_sd / sqrt(n())) %>% 
  ungroup()

bague_moyenne <- bague %>%
  group_by(Espece, Annee) %>%          # Regroupe par espèce et année
  summarise(Condition_moyenne = mean(Condition, na.rm = TRUE),
            Condition_sd = sd(Condition, na.rm = TRUE),  
            Condition_se = Condition_sd / sqrt(n())) %>% 
  filter(!Annee %in% c(1996,1997,1998, 1999, 2000,2001,2002,2003,2004,2005, 2006)) %>%   # Retrait des années avant 2007
ungroup()

bague_moyenne$Espece <- replace(bague_moyenne$Espece, bague_moyenne$Espece %in% "Durbec des sapins", "DUSA")
bague_moyenne$Espece <- replace(bague_moyenne$Espece, bague_moyenne$Espece %in% "Sizerin flammé", "SIFL")
bague_moyenne$Espece <- replace(bague_moyenne$Espece, bague_moyenne$Espece %in% "Tarin des pins", "TAPI")
bague_moyenne$Espece <- replace(bague_moyenne$Espece, bague_moyenne$Espece %in% "Jaseur boréal", "JABO")

abond_joint <- abond_joint %>% 
  left_join(bague_moyenne, by = c("Espece", "Annee"))


# Exploration des données pour l'abondance -------------------------------------------------

# ABONDANCE

# Visualisation graphique de l'abondance des 4 espèces en fonction des années

plot_ab <- ggplot(abond, aes(x = Annee, y = abond, group = Espece, color = Espece))+
  geom_point(size = 3)+
  geom_line(linewidth = 1.5)+
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

#Visualisation de l'abondance STANDARDISÉE en LOG des 4 espèces en fonction des années

plot_log <- ggplot(abond, aes(x = Annee, y = log(abond_std), group = Espece, color = Espece))+
  geom_point(size = 3)+
  geom_line(linewidth = 1.5)+
  scale_y_continuous(limits = c(0, 7), n.breaks = 15)+
  labs(title = "Logarythme de l'abondance standardisée des espèces cibles par année",
       x = "Année",
       y = "log N. d'oiseaux/h",
       color = "Espèce")+
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_log


# Graphique de la condition de chaque espèce en fonction des années

plot_condition <- ggplot(bague, aes(x = Annee, y = Condition, group = Espece, color = Espece))+
  geom_point(size = 3)+
  labs(title = "Condition des espèces cibles par année",
       x = "Année",
       y = "Condition",
       color = "Espèce")+
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_condition

# Exploration des données pour bague moyenne ------------------------------

plot_condition <- ggplot(bague_moyenne, aes(x = Annee, y = Condition_moyenne, group = Espece, color = Espece)) +
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



# Exploration des données par espèce --------------------------------------

# DUSA (Adrien)

# abondance
plot_dusa_tot <- ggplot(abond[abond$Espece=="DUSA",], aes(x = Annee, y = abond_std, group =1))+
  geom_point(size = 3, col = "darkorange1")+
  geom_path(linewidth = 1, col = "orange")+
  labs(title = "Abondance du Durbec des Sapins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_dusa_tot


#log abondance
DUSA_log <- ggplot(DUSA, aes(x = Annee, y = log(abond_std), group =1))+
  geom_point(size = 3, col = "darkorange1")+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  labs(title = "Abondance du Durbec des Sapins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
DUSA_log


#lm_DUSA <- lm(abond$abond_std[abond$Espece == "DUSA"] ~ Annee, data = abond) ne fonctionne pas ce code

#Filtre espèce
abond_DUSA <- abond[abond$Espece == "DUSA", ]

#Modèle linéaire
lm_DUSA <- lm(abond_std ~ Annee, data = abond_DUSA)

# Condition
bague_DUSA <- bague[bague$Espece == "DUSA",] # Ne garde que DUSA
bague_DUSA <- bague_DUSA[bague_DUSA$Age != "U",] # Ne garde que les individus âgés

bague_DUSA <- bague_DUSA %>% # Regroupement des oiseaux bagués/année
  group_by(Annee) %>% 
  filter(between(Annee, 2007, 2025)) # Baguage plus régulier à partir de 2006 (ou 2007, à vérifier)
bague_annuel_DUSA <- bague_DUSA %>% # Nb. de DUSA bagués / année
  summarise(n())
ann <- seq(2007, 2025, 1) # Création d'un vecteur peuplé de 0

summary(bague_DUSA)

# Condition population
plot_condition_DUSA <- ggplot(bague_DUSA, aes(x = Annee, y = Condition, group = Annee)) +
    geom_boxplot(fill = "darkorange1") +
    labs(title = "Condition moyenne du durbec des sapins par année",
         x = "Année",
         y = "Condition moyenne") +
    theme(
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      panel.background = element_blank()
    )
  print(plot_condition_DUSA)
  
# combine les deux graphique :
  plot_grid(plot_dusa_tot, plot_condition_DUSA, ncol = 1) #graphique bof
# Condition jeunes vs adultes par année

plot_condition_age_DUSA<-ggplot(bague_DUSA, aes(x = Annee, y = Condition, group = interaction(Annee, Age)))+
  geom_boxplot(aes(fill = Age)) +
  scale_fill_manual(
    values = c(
      "AHY" = "orange",
      "HY" = "darkorange3", "U" = "red" # il n'y en a plus
    ))+
  labs(title = "Condition moyenne des classes de durbec des sapins par année",
       x = "Année",
       y = "Condition moyenne") +
  theme_classic() 

print(plot_condition_age_DUSA)

# différence entre les condition des jeunes et des adultes
t.test(bague_DUSA$Condition[bague_DUSA$Age=="HY"], bague_DUSA$Condition[bague_DUSA$Age=="AHY"], alternative = "two.sided")


# Liste des années uniques
annees <- unique(bague_DUSA$Annee)

# Boucle sur chaque année
for (annee in annees) {
  # Sous-ensemble des données pour l'année en cours
  data_annee <- bague_DUSA[bague_DUSA$Annee == annee, ]
  
  # Sous-ensembles HY et AHY
  condition_HY <- data_annee$Condition[data_annee$Age == "HY"]
  condition_AHY <- data_annee$Condition[data_annee$Age == "AHY"]
  
  # Test t si les deux groupes ont au moins un individu
  if (length(condition_HY) > 0 && length(condition_AHY) > 0) {
    result <- t.test(condition_HY, condition_AHY, alternative = "two.sided")
    cat("\nAnnée :", annee, "\n")
    print(result)
  } 
}
## 2016 ; 2018 ; 2020 : années où les adultes avaient une condition meilleures que les jeunes

# JABO (Bérince)

plot_jabo_tot <- ggplot(abond[abond$Espece=="JABO",], aes(x = Annee, y = abond_std, group =1))+
  geom_point(size = 3, col = "darkgreen")+
  geom_path(linewidth = 1, col = "chartreuse4")+
  labs(title = "Abondance du Jaseur Boréal par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_jabo_tot


JABO$Annee <- as.factor(as.numeric(JABO$Annee))
JABO$Annee <- as.integer(JABO$Annee)

JABO_log <- ggplot(JABO, aes(x = Annee, y = log(abond_std), group =1))+
  geom_point(size = 3, col = "darkgreen")+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  labs(title = "Abondance du Jaseur Boréal par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
JABO_log

# Condition
bague_JABO <- bague[bague$Espece == "JABO",] # Ne garde que JABO
bague_JABO <- bague_JABO[bague_JABO$Age != "U",] # Ne garde que les individus âgés

bague_JABO <- bague_JABO %>% # Regroupement des oiseaux bagués/année
  group_by(Annee) %>% 
  filter(between(Annee, 2007, 2025)) # Baguage plus régulier à partir de 2006 (ou 2007, à vérifier)
bague_annuel_JABO <- bague_JABO %>% # Nb. de JABO bagués / année
  summarise(n())
ann <- seq(2007, 2025, 1) # Création d'un vecteur peuplé de 0

summary(bague_JABO)

# Condition population
plot_condition_JABO <- ggplot(bague_JABO, aes(x = Annee, y = Condition, group = Annee)) +
  geom_boxplot(fill = "darkgreen") +
  labs(title = "Condition moyenne du jaseur boréal par année",
       x = "Année",
       y = "Condition moyenne") +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    panel.background = element_blank()
  )
print(plot_condition_JABO)

# Condition jeunes vs adultes par année

plot_condition_age_JABO<-ggplot(bague_JABO, aes(x = Annee, y = Condition, group = interaction(Annee, Age)))+
  geom_boxplot(aes(fill = Age))+
  scale_fill_manual(
    values = c(
      "AHY" = "green",
      "HY" = "darkgreen", "U" = "red" # il n'y en a plus
    ))+
  labs(title = "Condition moyenne des classes de jaseur boréal par année",
       x = "Année",
       y = "Condition moyenne") +
  theme_classic() 
print(plot_condition_age_JABO)


# pas différence entre les condition des jeunes et des adultes
t.test(bague_JABO$Condition[bague_JABO$Age=="HY"], bague_JABO$Condition[bague_JABO$Age=="AHY"], alternative = "two.sided")


# Liste des années uniques
annees <- unique(bague_JABO$Annee)

# Boucle sur chaque année
for (annee in annees) {
  # Sous-ensemble des données pour l'année en cours
  data_annee <- bague_JABO[bague_JABO$Annee == annee, ]
  
  # Sous-ensembles HY et AHY
  condition_HY <- data_annee$Condition[data_annee$Age == "HY"]
  condition_AHY <- data_annee$Condition[data_annee$Age == "AHY"]
  
  # Test t si les deux groupes ont au moins un individu
  if (length(condition_HY) > 0 && length(condition_AHY) > 0) {
    result <- t.test(condition_HY, condition_AHY, alternative = "two.sided")
    cat("\nAnnée :", annee, "\n")
    print(result)
  } 
}
## 2016 ; 2023 : années où les adultes avaient une condition meilleures que les jeunes

# SIFL

SIFL <- abond[abond$Espece == "SIFL",]
SIFL

SIFL <- SIFL %>% 
  group_by(Annee) %>% 
  filter(between(Annee, 2007, 2025))

bague_SIFL <- bague[bague$Espece == "SIFL",] # Ne garde que SIFL
bague_SIFL <- bague_SIFL[bague_SIFL$Age != "U",] # Ne garde que les individus âgés

bague_SIFL <- bague_SIFL %>% # Regroupement des oiseaux bagués/année
  group_by(Annee) %>% 
  filter(between(Annee, 2007, 2025)) # Baguage plus régulier à partir de 2006 (ou 2007, à vérifier)

plot_sifl_tot <- ggplot(SIFL, aes(x = Annee, y = abond_std, group = 1))+
  geom_point(size = 3, col = "turquoise4")+
  geom_path(linewidth = 1, col = "turquoise3")+
  labs(title = "Abondance du Sizerin Flammé par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_sifl_tot

# Proportion d'individus bagués par année 

bague_annuel_SIFL <- bague_SIFL %>% # Nb. de SIFL bagués / année
  summarise(n())

ann <- seq(2007, 2025, 1) # Création d'un vecteur peuplé de 0

#n_bague_ann_SIFL <- as.data.frame(ann) %>%  Problème avec ce code ici
#  rename(Annee = "ann") %>% 
#  left_join(bague_annuel_SIFL, by = "Annee") %>% 
#  cbind(n_bague_ann_SIFL$`n()`/SIFL$abond) %>%            # Calcul la proportion de SIFL bagué par année
#  rename(prop_bague = "n_bague_ann_SIFL$`n()`/SIFL$abond")

# Condition
plot_condition_SIFL <- ggplot(bague_SIFL, aes(x = Annee, y = Condition, group = Annee)) +
  geom_boxplot(fill = "turquoise") +
  labs(title = "Condition moyenne du sizerin flammé par année",
       x = "Année",
       y = "Condition moyenne") +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    panel.background = element_blank()
  )
print(plot_condition_SIFL)

# Condition jeunes vs adultes par année

plot_condition_age_SIFL<-ggplot(bague_SIFL, aes(x = Annee, y = Condition, group = interaction(Annee, Age)))+
  geom_boxplot(aes(fill = Age))+
  scale_fill_manual(
    values = c(
      "AHY" = "turquoise",
      "HY" = "darkblue", "U" = "red" # il n'y en a plus
    ))+
  labs(title = "Condition moyenne des classes de sizerin flammé par année",
       x = "Année",
       y = "Condition moyenne") +
  theme_classic() 
print(plot_condition_age_SIFL)

# différence entre les condition des jeunes et des adultes
t.test(bague_SIFL$Condition[bague_SIFL$Age=="HY"], bague_SIFL$Condition[bague_SIFL$Age=="AHY"], alternative = "two.sided")


# Liste des années uniques
annees <- unique(bague_SIFL$Annee)

# Boucle sur chaque année
for (annee in annees) {
  # Sous-ensemble des données pour l'année en cours
  data_annee <- bague_SIFL[bague_SIFL$Annee == annee, ]
  
  # Sous-ensembles HY et AHY
  condition_HY <- data_annee$Condition[data_annee$Age == "HY"]
  condition_AHY <- data_annee$Condition[data_annee$Age == "AHY"]
  
  # Test t si les deux groupes ont au moins un individu
  if (length(condition_HY) > 0 && length(condition_AHY) > 0) {
    result <- t.test(condition_HY, condition_AHY, alternative = "two.sided")
    cat("\nAnnée :", annee, "\n")
    print(result)
  } 
}
## 2014 ; 2016 ; 2018 : années où les adultes avaient une condition meilleures que les jeunes


# TAPI (Maxence) 

plot_tapi_tot <- ggplot(abond[abond$Espece=="TAPI",], aes(x = Annee, y = abond_std, group =1))+
  geom_point(size = 3, col = "violetred4")+
  geom_path(linewidth = 1, col = "violetred3")+
  labs(title = "Abondance du Tarin des Pins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_tapi_tot


# CHANGER AXE DES X POUR AVOUR LES ANNÉES
#Prend le lm pour le graphique
lm_TAPI <- lm(log(abond_std) ~ Annee, data = TAPI)

TAPI_log <- ggplot(TAPI, aes(x = Annee, y = log(abond_std), group =1))+
  geom_point(size = 3, col = "violetred4")+
  geom_line(col="violetred3")+
  geom_abline(intercept = coef(lm_TAPI)[1], slope = coef(lm_TAPI)[2], color = "red")+
  labs(title = "Abondance du Tarin des Pins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
TAPI_log+
  annotate(geom ="text",x = 30, y = 5, label ="y = 1.66 + 0.067x")

summary(lm_TAPI)

#stat_cor(method = "pearson")+
  
# Condition
bague_TAPI <- bague[bague$Espece == "TAPI",] # Ne garde que TAPI
bague_TAPI <- bague_TAPI[bague_TAPI$Age != "U",] # Ne garde que les individus âgés

bague_TAPI <- bague_TAPI %>% # Regroupement des oiseaux bagués/année
  group_by(Annee) %>% 
  filter(between(Annee, 2007, 2025)) # Baguage plus régulier à partir de 2006 (ou 2007, à vérifier)
bague_annuel_TAPI <- bague_TAPI %>% # Nb. de TAPI bagués / année
  summarise(n())
ann <- seq(2007, 2025, 1) # Création d'un vecteur peuplé de 0


summary(bague_TAPI)

# Condition population
plot_condition_TAPI <- ggplot(bague_TAPI, aes(x = Annee, y = Condition, group = Annee)) +
  geom_boxplot(fill = "violetred4") +
  labs(title = "Condition moyenne du tarin des pins par année",
       x = "Année",
       y = "Condition moyenne") +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    panel.background = element_blank()
  )
print(plot_condition_TAPI)

# Condition jeunes vs adultes par année

plot_condition_age_TAPI<-ggplot(bague_TAPI, aes(x = Annee, y = Condition, group = interaction(Annee, Age)))+
  geom_boxplot(aes(fill = Age))+
  scale_fill_manual(
    values = c(
      "AHY" = "violet",
      "HY" = "violetred4", "U" = "red" # il n'y en a plus
    ))+
  labs(title = "Condition moyenne des classes de tarin des pins par année",
       x = "Année",
       y = "Condition moyenne") +
  theme_classic() 
print(plot_condition_age_TAPI)

# différence entre les condition des jeunes et des adultes
t.test(bague_TAPI$Condition[bague_TAPI$Age=="HY"], bague_TAPI$Condition[bague_TAPI$Age=="AHY"], alternative = "two.sided")


# Liste des années uniques
annees <- unique(bague_TAPI$Annee)

# Boucle sur chaque année
for (annee in annees) {
  # Sous-ensemble des données pour l'année en cours
  data_annee <- bague_TAPI[bague_TAPI$Annee == annee, ]
  
  # Sous-ensembles HY et AHY
  condition_HY <- data_annee$Condition[data_annee$Age == "HY"]
  condition_AHY <- data_annee$Condition[data_annee$Age == "AHY"]
  
  # Test t si les deux groupes ont au moins un individu
  if (length(condition_HY) > 0 && length(condition_AHY) > 0) {
    result <- t.test(condition_HY, condition_AHY, alternative = "two.sided")
    cat("\nAnnée :", annee, "\n")
    print(result)
  } 
}
## 2020 ; 2021 : années où les adultes avaient une condition meilleures que les jeunes

#Voir si la condition physique des différentes classes est associée à l'abondance standardisé

# Avoir les données d'abondance avec condition
#jointure <- left_join(
#  bague_modifie, 
#  abondance, 
#  by = c("Année" = "Année") )

# D'abord filtrer pour garder seulement les Tarin
#jointure_tarin <- jointure[jointure$Espèce == "Tarin des pins", ]

# Renommer la colonne
#names(jointure_tarin)[names(jointure_tarin) == "Tarin des pins_std"] <- "Tarin_std"

#Conserver les colonnes utiles
#jointure_tarin <- jointure_tarin[, c("Année", "Espèce", "Condition", "groupe", "Effort", "Tarin des pins_std")]

# Ensuite faire le modèle
#mod <- lm(Condition ~ groupe + Année + Tarin_std, data=jointure_tarin)
#summary(mod)

#par(mfrow = c(2, 2))
#plot(mod)

# Visuellement les conditions semblent respectés


# TENDANCE TEMPORELLE -----------------------------------------------------

# Tendance temporelle depuis 1996 - 2025

# SIFL

abond_modif_SIFL <- log(SIFL$abond_std + 1)
abond_modif_SIFL
SIFL
lm_SIFL <- lm(log(abond_std) ~ Annee, data = SIFL)

summary(lm_SIFL)

par(mfrow = c(2,2))
plot(lm_SIFL)

SIFL_int <- lm_SIFL$coefficients["(Intercept)"]
SIFL_annee <- lm_SIFL$coefficients["Annee"]
SIFL_annee
SIFL_sd <- coef(summary(lm_SIFL))[, "Std. Error"]
SIFL_sd

summary(lm_SIFL)


# Intercepte
upper_ic_SIFL <- SIFL_int + (1.96 * SIFL_sd[1])
lower_ic_SIFL <- SIFL_int - (1.96 * SIFL_sd[1])
lower_ic_SIFL

upper_ic_annee_SIFL <- SIFL_annee + (1.96 * SIFL_sd[2])
exp(upper_ic_annee_SIFL)
lower_ic_annee_SIFL <- SIFL_annee - (1.96 * SIFL_sd[2])
exp(lower_ic_annee_SIFL)

plot(exp(SIFL_annee),
     ylim = c(0.80, 1.25))
abline(h = 1, lty = 2, col = "red")
segments(x0 = 1, y0 = exp(lower_ic_annee_SIFL),
         x1 = 1, y1 = exp(upper_ic_annee_SIFL))


n_sim <- 5000
SIFL_output <- matrix(data = NA, nrow = n_sim, ncol = 2)
SIFL_sim <- SIFL[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  SIFL_sim$abond_std <- sample(x = SIFL$abond_std, replace = FALSE)
  
  lm_SIFL_rand <- lm(log(abond_std) ~ Annee, data = SIFL_sim)
  
  SIFL_output[i,1] <- coef(lm_SIFL_rand)[1] # Beta intercepte (abond_std)
  SIFL_output[i,2] <- coef(lm_SIFL_rand)[2] # Beta année
  
}

SIFL_output

hist(SIFL_output[,1])
hist(SIFL_output[,2])

pbeta_annee <- sum(SIFL_output[, 2] >= coef(lm_SIFL)[2])/n_sim
pbeta_annee

summary(lm_SIFL)

exp(SIFL_annee) # 1,002 oiseau de plus/heure/année 
exp(SIFL_int) 


# GRAPHIQUE

SIFL_log <- ggplot(SIFL, aes(x = Annee, y = log(abond_std), group = 1))+
  geom_point(size = 3, col = "turquoise4")+
  geom_line()+
  geom_abline(intercept = coef(lm_SIFL)[1], slope = coef(lm_SIFL)[2], color = "red")+
  labs(title = "Abondance du Sizerin Flammé par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
SIFL_log <- SIFL_log+
  annotate(geom ="text",x = 2009, y = 6, label ="y = 2.35 + 0.025x")

SIFL_log 

# TAPI --------------------------------------------------------------------

abond_modif_TAPI <- log(TAPI$abond_std + 1)
abond_modif_TAPI

lm_TAPI <- lm(log(abond_std) ~ Annee, data = TAPI)

summary(lm_TAPI)

par(mfrow = c(2,2))
plot(lm_TAPI)
dev.off()

TAPI_int <- lm_TAPI$coefficients["(Intercept)"]
TAPI_int
TAPI_annee <- lm_TAPI$coefficients["Annee"]
TAPI_annee
TAPI_sd <- coef(summary(lm_TAPI))[, "Std. Error"]
TAPI_sd

summary(lm_TAPI)


# Intercepte
upper_ic_TAPI <- TAPI_int + (1.96 * TAPI_sd[1])
lower_ic_TAPI <- TAPI_int - (1.96 * TAPI_sd[1])
lower_ic_TAPI

upper_ic_annee_TAPI <- TAPI_annee + (1.96 * TAPI_sd[2])
upper_ic_annee_TAPI
lower_ic_annee_TAPI <- TAPI_annee - (1.96 * TAPI_sd[2])
lower_ic_annee_TAPI



plot(exp(TAPI_annee),
     ylim = c(0.80, 1.25))
abline(h = 1, lty = 2, col = "red")
segments(x0 = 1, y0 = exp(lower_ic_annee_TAPI),
         x1 = 1, y1 = exp(upper_ic_annee_TAPI))

n_sim <- 5000
TAPI_output <- matrix(data = NA, nrow = n_sim, ncol = 2)
TAPI_sim <- TAPI[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  TAPI_sim$abond_std <- sample(x = TAPI$abond_std, replace = FALSE)
  
  lm_TAPI_rand <- lm(log(abond_std) ~ Annee, data = TAPI_sim)
  
  TAPI_output[i,1] <- coef(lm_TAPI_rand)[1] # Beta intercepte (abond_std)
  TAPI_output[i,2] <- coef(lm_TAPI_rand)[2] # Beta année
  
}

TAPI_output

hist(TAPI_output[,1])
hist(TAPI_output[,2])

pbeta_annee <- sum(TAPI_output[, 2] >= coef(lm_TAPI)[2])/n_sim
pbeta_annee

summary(lm_TAPI)

exp(TAPI_annee) # 1,07 oiseau de plus/heure/année 
exp(TAPI_int) 

# INSERTION GRAPHIQUE

TAPI_log <- ggplot(TAPI, aes(x = Annee, y = log(abond_std), group =1))+
  geom_point(size = 3, col = "violetred4")+
  geom_line()+
  geom_abline(intercept = coef(lm_TAPI)[1], slope = coef(lm_TAPI)[2], color = "red")+
  labs(title = "Abondance du Tarin des Pins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
TAPI_log <- TAPI_log+
  annotate(geom ="text",x = 5, y = 5, label ="y = 1.66 + 0.067x")

print(TAPI_log)

# DUSA --------------------------------------------------------------------


abond_modif_DUSA <- log(DUSA$abond_std + 1)
abond_modif_DUSA

lm_DUSA <- lm(log(abond_std) ~ Annee, data = DUSA)

summary(lm_DUSA)

par(mfrow = c(2,2))
plot(lm_DUSA)
dev.off()

DUSA_int <- lm_DUSA$coefficients["(Intercept)"]
DUSA_annee <- lm_DUSA$coefficients["Annee"]
DUSA_annee
DUSA_sd <- coef(summary(lm_DUSA))[, "Std. Error"]
DUSA_sd

summary(lm_DUSA)


# Intercepte
upper_ic_DUSA <- DUSA_int + (1.96 * DUSA_sd[1])
lower_ic_DUSA <- DUSA_int - (1.96 * DUSA_sd[1])
lower_ic_DUSA

upper_ic_annee_DUSA <- DUSA_annee + (1.96 * DUSA_sd[2])
upper_ic_annee_DUSA
lower_ic_annee_DUSA <- DUSA_annee - (1.96 * DUSA_sd[2])
lower_ic_annee_DUSA

plot(exp(DUSA_annee),
     ylim = c(0.80, 1.25))
abline(h = 1, lty = 2, col = "red")
segments(x0 = 1, y0 = exp(lower_ic_annee_DUSA),
         x1 = 1, y1 = exp(upper_ic_annee_DUSA))


n_sim <- 5000
DUSA_output <- matrix(data = NA, nrow = n_sim, ncol = 2)
DUSA_sim <- DUSA[, c("Annee")]

set.seed(0088)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  DUSA_sim$abond_std <- sample(x = DUSA$abond_std, replace = FALSE)
  
  lm_DUSA_rand <- lm(log(abond_std) ~ Annee, data = DUSA_sim)
  
  DUSA_output[i,1] <- coef(lm_DUSA_rand)[1] # Beta intercepte (abond_std)
  DUSA_output[i,2] <- coef(lm_DUSA_rand)[2] # Beta année
  
}

DUSA_output

hist(DUSA_output[,1])
hist(DUSA_output[,2])

pbeta_annee <- sum(DUSA_output[, 2] >= coef(lm_DUSA)[2])/n_sim
pbeta_annee

exp(DUSA_annee) # 1,05 oiseau de plus/heure/année 
exp(DUSA_int) 

val_pred_DUSA <- predict.lm(lm_DUSA)
val_pred_DUSA


coefficients(lm_DUSA)

sigma(lm_DUSA)

# Indice annuelle
coef_irrupt_DUSA <- (DUSA$abond_std - mean(DUSA$abond_std)/sigma(lm_DUSA))
coef_irrupt_DUSA # Lamontagne and Boutin, 2009

# Indice avec valeurs prédites
ind_irrupt_DUSA <- (DUSA$abond_std - val_pred_DUSA)/sigma(lm_DUSA)
ind_irrupt_DUSA # Widick et al., 2023

DUSA$abond_std - mean(DUSA$abond_std)



# GRAPHIQUE

DUSA_log <- ggplot(DUSA, aes(x = Annee, y = log(abond_std), group =1))+
  geom_point(size = 3, col = "darkorange1")+
  geom_line()+
  geom_abline(intercept = coef(lm_DUSA)[1], slope = coef(lm_DUSA)[2], color = "red")+
  labs(title = "Abondance du Durbec des Sapins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
DUSA_log <- DUSA_log+
  annotate(geom ="text",x = 5, y = 5, label ="y = 1.11 + 0.05x")

print(DUSA_log)

# JABO

# Puisqu'on a une abondance ~ 0 (log(8)) -> +1 sur toutes les données pour éviter 0

abond_modif_JABO <- log(JABO$abond_std + 1)
abond_modif_JABO

lm_JABO <- lm(abond_modif_JABO ~ Annee, data = JABO)

summary(lm_JABO)

par(mfrow = c(2,2))
plot(lm_JABO) # MIEUX!!
dev.off()

JABO_int <- lm_JABO$coefficients["(Intercept)"]
JABO_annee <- lm_JABO$coefficients["Annee"]
JABO_annee
JABO_sd <- coef(summary(lm_JABO))[, "Std. Error"]
JABO_sd


# Intercepte
upper_ic_JABO <- JABO_int + (1.96 * JABO_sd[1])
lower_ic_JABO <- JABO_int - (1.96 * JABO_sd[1])
lower_ic_JABO

upper_ic_annee_JABO <- JABO_annee + (1.96 * JABO_sd[2])
upper_ic_annee_JABO
lower_ic_annee_JABO <- JABO_annee - (1.96 * JABO_sd[2])
lower_ic_annee_JABO

plot(exp(JABO_annee),
     ylim = c(0.80, 1.25))
abline(h = 1, lty = 2, col = "red")
segments(x0 = 1, y0 = exp(lower_ic_annee_JABO),
         x1 = 1, y1 = exp(upper_ic_annee_JABO))

summary(lm_JABO)


n_sim <- 5000
JABO_output <- matrix(data = NA, nrow = n_sim, ncol = 2)
JABO_sim <- JABO[, c("Annee")]

set.seed(5555)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  JABO_sim$abond_std <- sample(x = JABO$abond_std, replace = FALSE)
  
  lm_JABO_rand <- lm(log(abond_std) ~ Annee, data = JABO_sim)
  
  JABO_output[i,1] <- coef(lm_JABO_rand)[1] # Beta intercepte (abond_std)
  JABO_output[i,2] <- coef(lm_JABO_rand)[2] # Beta année
  
}

JABO_output

hist(JABO_output[,1])
hist(JABO_output[,2])

pbeta_annee <- sum(JABO_output[, 2] >= coef(lm_JABO)[2])/n_sim
pbeta_annee

summary(lm_JABO)

exp(JABO_annee) # 1,015 oiseau de plus/heure/année 
exp(JABO_int) 


JABO_log <- ggplot(JABO, aes(x = Annee, y = log(abond_std), group =1))+
  geom_point(size = 3, col = "forestgreen")+
  geom_line()+
  geom_abline(intercept = coef(lm_JABO)[1], slope = coef(lm_JABO)[2], color = "red")+
  labs(title = "Abondance du Durbec des Sapins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
JABO_log <- JABO_log+
  annotate(geom ="text",x = 5, y = 3.5, label ="y = 1.18 + 0.015x")

print(JABO_log)

# Analyse -----------------------------------------------------------------

# Combine les 4 graphiques en 2x2
# abondance log
plot_grid(DUSA_log,
          TAPI_log,
          SIFL_log,
          JABO_log, ncol = 2)

# condition
plot_grid(plot_condition_DUSA, 
          plot_condition_TAPI, 
          plot_condition_SIFL, 
          plot_condition_JABO, ncol=2)

# condition par age
plot_grid(plot_condition_age_DUSA,
          plot_condition_age_TAPI,
          plot_condition_age_SIFL, 
          plot_condition_age_JABO, ncol = 2)




# Mettre les "prédictions" en un seul graphique

# Combine les 4 graphiques en 2x2
plot_grid(DUSA_log,
          TAPI_log,
          SIFL_log,
          JABO_log, ncol = 2)

tab_comp <- data.frame(c(DUSA_int, JABO_int, TAPI_int, SIFL_int),
                       c(DUSA_annee, JABO_annee, TAPI_annee, SIFL_annee),
                       c(lower_ic_annee_DUSA, lower_ic_annee_JABO, lower_ic_annee_TAPI, lower_ic_annee_SIFL),
                       c(upper_ic_annee_DUSA, upper_ic_annee_JABO, upper_ic_annee_TAPI, upper_ic_annee_SIFL))


rownames(tab_comp) <- c("DUSA", "JABO", "TAPI", "SIFL")
colnames(tab_comp) <- c("Intercepte", "annee", "ic_inf", "ic_sup")
tab_comp

axex <- 1:4

plot(tab_comp$annee,
     xaxt = "n",
     ylim = c(-0.2, 0.25),
     ylab = "log estimé de beta",
     xlab = "Espèces",
     main = "Estimés de log beta avec IC 95% : abondance")
abline(h = 0, lty = 2, col = "red")
axis(side = 1, at = axex, labels = c("DUSA", "JABO", "TAPI", "SIFL"))
segments(x0 = 1, y0 = lower_ic_annee_DUSA,
         x1 = 1, y1 = upper_ic_annee_DUSA)
segments(x0 = 2, y0 = lower_ic_annee_JABO,
         x1 = 2, y1 = upper_ic_annee_JABO)
segments(x0 = 3, y0 = lower_ic_annee_TAPI,
         x1 = 3, y1 = upper_ic_annee_TAPI)
segments(x0 = 4, y0 = lower_ic_annee_SIFL,
         x1 = 4, y1 = upper_ic_annee_SIFL)



summary(lm_DUSA)
summary(lm_SIFL)
summary(lm_JABO)
summary(lm_TAPI)

summary(lm_DUSA_modif)

#### tendance long terme condition ####

bague_moyenne$Espece<-as.factor(bague_moyenne$Espece)
## JABO
bague_moyenne_JABO <-bague_moyenne[bague_moyenne$Espece== "JABO",]
lm_JABO_cond <- lm(Condition_moyenne ~ Annee, data = bague_moyenne_JABO) 
summary(lm_JABO_cond)
# la régression n'explique rien = simulation peu pertinente

par(mfrow = c(2,2))
plot(lm_JABO_cond)
dev.off()

# GRAPHIQUE
JABO_cond <- ggplot(bague_moyenne_JABO, aes(x = Annee, y = Condition_moyenne, group = 1))+
  geom_point(size = 3, col = "forestgreen")+
  geom_line(col= "forestgreen")+
  geom_abline(intercept = coef(lm_JABO_cond)[1], slope = coef(lm_JABO_cond)[2], color = "red")+
  labs(title = "Condition moyenne du Jaseur boréal par année",
       x = "Année",
       y = "Condition")+
  theme_classic()
JABO_cond <- JABO_cond+
  annotate(geom ="text",x = 2010, y = 2.35, label ="p = 0.83  R² = -0.08")
JABO_cond ### la condition change de 0.02 par année


# simulation
n_sim <- 5000
JABO_output_cond <- matrix(data = NA, nrow = n_sim, ncol = 2)
JABO_sim_cond <- bague_moyenne_JABO[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  JABO_sim_cond$condition <- sample(x = bague_moyenne_JABO$Condition_moyenne, replace = FALSE)
  
  lm_JABO_rand_cond <- lm(condition ~ Annee, data = JABO_sim_cond)
  
  JABO_output_cond[i,1] <- coef(lm_JABO_rand_cond)[1] # Beta intercepte (abond_std)
  JABO_output_cond[i,2] <- coef(lm_JABO_rand_cond)[2] # Beta année
  
}
JABO_output_cond
summary(lm_JABO_rand_cond)
summary(lm_JABO_cond)
hist(JABO_output_cond[,1])
abline(v =(coef(lm_JABO_cond)[1]), lty = 2, col= "red")

hist(JABO_output_cond[,2])
abline(v =(coef(lm_JABO_cond)[2]), lty = 2, col= "red")

pbeta_annee <- sum(JABO_output_cond[, 2] >= coef(lm_JABO_cond)[2])/n_sim
pbeta_annee # 0.574

# graph coef
JABO_int_cond <- lm_JABO_cond$coefficients["(Intercept)"]
JABO_annee_cond <- lm_JABO_cond$coefficients["Annee"]
JABO_annee_cond
JABO_sd_cond <- coef(summary(lm_JABO_cond))[, "Std. Error"]
JABO_sd_cond

# Intercepte
upper_ic_annee_JABO_cond <- JABO_annee_cond + (1.96 * JABO_sd_cond[2])
upper_ic_annee_JABO_cond
lower_ic_annee_JABO_cond <- JABO_annee_cond - (1.96 * JABO_sd_cond[2])
lower_ic_annee_JABO_cond

plot((JABO_annee_cond),
     ylim = c(0.015,-0.015))
abline(h = 0, lty = 2, col = "red")
segments(x0 = 1, y0 = (lower_ic_annee_JABO_cond),
         x1 = 1, y1 = (upper_ic_annee_JABO_cond))


## DUSA
bague_moyenne_DUSA <-bague_moyenne[bague_moyenne$Espece== "DUSA",]
lm_DUSA_cond <- lm(Condition_moyenne ~ Annee, data = bague_moyenne_DUSA) 
summary(lm_DUSA_cond)
# la régression n'explique rien = simulation peu pertinente

par(mfrow = c(2,2))
plot(lm_DUSA_cond)
dev.off()

# GRAPHIQUE
DUSA_cond <- ggplot(bague_moyenne_DUSA, aes(x = Annee, y = Condition_moyenne, group = 1))+
  geom_point(size = 3, col = "orange")+
  geom_line(col= "orange")+
  geom_abline(intercept = coef(lm_DUSA_cond)[1], slope = coef(lm_DUSA_cond)[2], color = "red")+
  labs(title = "Condition moyenne du Durbec des sapins par année",
       x = "Année",
       y = "Condition")+
  theme_classic()
DUSA_cond <- DUSA_cond+
  annotate(geom ="text",x = 2010, y = 2.15, label ="p = 0.26  R² = 0.02")
DUSA_cond ### la condition change de 0.02 par année


# simulation
n_sim <- 5000
DUSA_output_cond <- matrix(data = NA, nrow = n_sim, ncol = 2)
DUSA_sim_cond <- bague_moyenne_DUSA[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  DUSA_sim_cond$condition <- sample(x = bague_moyenne_DUSA$Condition_moyenne, replace = FALSE)
  
  lm_DUSA_rand_cond <- lm(condition ~ Annee, data = DUSA_sim_cond)
  
  DUSA_output_cond[i,1] <- coef(lm_DUSA_rand_cond)[1] # Beta intercepte (abond_std)
  DUSA_output_cond[i,2] <- coef(lm_DUSA_rand_cond)[2] # Beta année
  
}
DUSA_output_cond
summary(lm_DUSA_rand_cond)
summary(lm_DUSA_cond)
hist(DUSA_output_cond[,1])
hist(DUSA_output_cond[,2])
abline(v =(coef(lm_DUSA_cond)[2]), lty = 2, col= "red")

pbeta_annee <- sum(DUSA_output_cond[, 2] >= coef(lm_DUSA_cond)[2])/n_sim
pbeta_annee # 0.133

# graph coef
DUSA_int_cond <- lm_DUSA_cond$coefficients["(Intercept)"]
DUSA_annee_cond <- lm_DUSA_cond$coefficients["Annee"]
DUSA_annee_cond
DUSA_sd_cond <- coef(summary(lm_DUSA_cond))[, "Std. Error"]
DUSA_sd_cond

# Intercepte
upper_ic_annee_DUSA_cond <- DUSA_annee_cond + (1.96 * DUSA_sd_cond[2])
upper_ic_annee_DUSA_cond
lower_ic_annee_DUSA_cond <- DUSA_annee_cond - (1.96 * DUSA_sd_cond[2])
lower_ic_annee_DUSA_cond

plot((DUSA_annee_cond),
     ylim = c(0.015,-0.015))
abline(h = 0, lty = 2, col = "red")
segments(x0 = 1, y0 = (lower_ic_annee_DUSA_cond),
         x1 = 1, y1 = (upper_ic_annee_DUSA_cond))


## SIFL
bague_moyenne_SIFL <-bague_moyenne[bague_moyenne$Espece== "SIFL",]
lm_SIFL_cond <- lm(Condition_moyenne ~ Annee, data = bague_moyenne_SIFL) 
summary(lm_SIFL_cond)
# la régression n'explique rien = simulation peu pertinente

par(mfrow = c(2,2))
plot(lm_SIFL_cond)
dev.off()

# GRAPHIQUE
SIFL_cond <- ggplot(bague_moyenne_SIFL, aes(x = Annee, y = Condition_moyenne, group = 1))+
  geom_point(size = 3, col = "turquoise")+
  geom_line(col= "turquoise")+
  geom_abline(intercept = coef(lm_SIFL_cond)[1], slope = coef(lm_SIFL_cond)[2], color = "red")+
  labs(title = "Moyenne de la condition du Sizerin flammé par année",
       x = "Année",
       y = "Condition")+
  theme_classic()
SIFL_cond <- SIFL_cond+
  annotate(geom ="text",x = 2010, y = 6.0, label ="p = 0.50  R² = -0.03")
SIFL_cond ### la condition change de 0.02 par année


# simulation
n_sim <- 5000
SIFL_output_cond <- matrix(data = NA, nrow = n_sim, ncol = 2)
SIFL_sim_cond <- bague_moyenne_SIFL[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  SIFL_sim_cond$condition <- sample(x = bague_moyenne_SIFL$Condition_moyenne, replace = FALSE)
  
  lm_SIFL_rand_cond <- lm(condition ~ Annee, data = SIFL_sim_cond)
  
  SIFL_output_cond[i,1] <- coef(lm_SIFL_rand_cond)[1] # Beta intercepte (abond_std)
  SIFL_output_cond[i,2] <- coef(lm_SIFL_rand_cond)[2] # Beta année
  
}
SIFL_output_cond
summary(lm_SIFL_rand_cond)
summary(lm_SIFL_cond)
hist(SIFL_output_cond[,1])
abline(v =(coef(lm_SIFL_cond)[1]), lty = 2, col= "red")

hist(SIFL_output_cond[,2])
abline(v =(coef(lm_SIFL_cond)[2]), lty = 2, col= "red")

pbeta_annee <- sum(SIFL_output_cond[, 2] >= coef(lm_SIFL_cond)[2])/n_sim
pbeta_annee # 0.252

# graph coef
SIFL_int_cond <- lm_SIFL_cond$coefficients["(Intercept)"]
SIFL_annee_cond <- lm_SIFL_cond$coefficients["Annee"]
SIFL_annee_cond
SIFL_sd_cond <- coef(summary(lm_SIFL_cond))[, "Std. Error"]
SIFL_sd_cond

# Intercepte
upper_ic_annee_SIFL_cond <- SIFL_annee_cond + (1.96 * SIFL_sd_cond[2])
upper_ic_annee_SIFL_cond
lower_ic_annee_SIFL_cond <- SIFL_annee_cond - (1.96 * SIFL_sd_cond[2])
lower_ic_annee_SIFL_cond

plot((SIFL_annee_cond),
     ylim = c(-0.02,0.02))
abline(h = 0, lty = 2, col = "red")
segments(x0 = 1, y0 = (lower_ic_annee_SIFL_cond),
         x1 = 1, y1 = (upper_ic_annee_SIFL_cond))

## TAPI
bague_moyenne_TAPI <-bague_moyenne[bague_moyenne$Espece== "TAPI",]
lm_TAPI_cond <- lm(Condition_moyenne ~ Annee, data = bague_moyenne_TAPI) 
summary(lm_TAPI_cond)
# la régression n'explique rien = simulation peu pertinente

par(mfrow = c(2,2))
plot(lm_TAPI_cond)
dev.off()

# GRAPHIQUE
TAPI_cond <- ggplot(bague_moyenne_TAPI, aes(x = Annee, y = Condition_moyenne, group = 1))+
  geom_point(size = 3, col = "violetred")+
  geom_line(col= "violetred")+
  geom_abline(intercept = coef(lm_TAPI_cond)[1], slope = coef(lm_TAPI_cond)[2], color = "red")+
  labs(title = "Condition moyenne du Tarin des pins par année",
       x = "Année",
       y = "Condition")+
  theme_classic()
TAPI_cond <- TAPI_cond+
  annotate(geom ="text",x = 2010, y = 5.8, label ="p = 0.23  R² = 0.03")
TAPI_cond ### la condition change de 0.02 par année

# simulation
n_sim <- 5000
TAPI_output_cond <- matrix(data = NA, nrow = n_sim, ncol = 2)
TAPI_sim_cond <- bague_moyenne_TAPI[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  TAPI_sim_cond$condition <- sample(x = bague_moyenne_TAPI$Condition_moyenne, replace = FALSE)
  
  lm_TAPI_rand_cond <- lm(condition ~ Annee, data = TAPI_sim_cond)
  
  TAPI_output_cond[i,1] <- coef(lm_TAPI_rand_cond)[1] # Beta intercepte (abond_std)
  TAPI_output_cond[i,2] <- coef(lm_TAPI_rand_cond)[2] # Beta année
  
}
TAPI_output_cond
summary(lm_TAPI_rand_cond)
summary(lm_TAPI_cond)
hist(TAPI_output_cond[,1])
abline(v =(coef(lm_TAPI_cond)[1]), lty = 2, col= "red")

hist(TAPI_output_cond[,2])
abline(v =(coef(lm_TAPI_cond)[2]), lty = 2, col= "red")

pbeta_annee <- sum(TAPI_output_cond[, 2] >= coef(lm_TAPI_cond)[2])/n_sim
pbeta_annee # 0.12

# graph coef
TAPI_int_cond <- lm_TAPI_cond$coefficients["(Intercept)"]
TAPI_annee_cond <- lm_TAPI_cond$coefficients["Annee"]
TAPI_annee_cond
TAPI_sd_cond <- coef(summary(lm_TAPI_cond))[, "Std. Error"]
TAPI_sd_cond

# Intercepte
upper_ic_annee_TAPI_cond <- TAPI_annee_cond + (1.96 * TAPI_sd_cond[2])
upper_ic_annee_TAPI_cond
lower_ic_annee_TAPI_cond <- TAPI_annee_cond - (1.96 * TAPI_sd_cond[2])
lower_ic_annee_TAPI_cond

plot((TAPI_annee_cond),
     ylim = c(-0.02,0.02))
abline(h = 0, lty = 2, col = "red")
segments(x0 = 1, y0 = (lower_ic_annee_TAPI_cond),
         x1 = 1, y1 = (upper_ic_annee_TAPI_cond))

### regrouper les graphique
plot_grid(DUSA_cond, JABO_cond, SIFL_cond, TAPI_cond )

tab_comp_cond <- data.frame(c(DUSA_int_cond, JABO_int_cond, TAPI_int_cond, SIFL_int_cond),
                       c(DUSA_annee_cond, JABO_annee_cond, TAPI_annee_cond, SIFL_annee_cond),
                       c(lower_ic_annee_DUSA_cond, lower_ic_annee_JABO_cond, lower_ic_annee_TAPI_cond, lower_ic_annee_SIFL_cond),
                       c(upper_ic_annee_DUSA_cond, upper_ic_annee_JABO_cond, upper_ic_annee_TAPI_cond, upper_ic_annee_SIFL_cond))

axex<- 1:4
rownames(tab_comp_cond) <- c("DUSA", "JABO", "TAPI", "SIFL")
colnames(tab_comp_cond) <- c("Intercepte", "annee", "ic_inf", "ic_sup")
tab_comp_cond
plot(tab_comp_cond$annee,
     xaxt = "n",
     ylim = c(-0.02, 0.02),
     ylab = "log estimé de beta",
     xlab = "Espèces",
     main = "Estimés de log beta avec IC 95% : condition")
abline(h = 0, lty = 2, col = "red")
axis(side = 1, at = axex, labels = c("DUSA", "JABO", "TAPI", "SIFL"))
segments(x0 = 1, y0 = lower_ic_annee_DUSA_cond,
         x1 = 1, y1 = upper_ic_annee_DUSA_cond)
segments(x0 = 2, y0 = lower_ic_annee_JABO_cond,
         x1 = 2, y1 = upper_ic_annee_JABO_cond)
segments(x0 = 3, y0 = lower_ic_annee_TAPI_cond,
         x1 = 3, y1 = upper_ic_annee_TAPI_cond)
segments(x0 = 4, y0 = lower_ic_annee_SIFL_cond,
         x1 = 4, y1 = upper_ic_annee_SIFL_cond)


#### proportion long terme #### 

## JABO
prop_JABO <-abond_joint[abond_joint$Espece== "JABO",c(1,3,8,9)]
#prop_JABO<- prop_JABO[prop_JABO$nb_total>10] # ex pour filter avec seuil
lm_JABO_prop <- lm(prop_jeunes ~ Annee, weight = nb_total, data = prop_JABO) 
summary(lm_JABO_prop)
# la régression n'explique rien 

par(mfrow = c(2,2))
plot(lm_JABO_prop)
dev.off()

# GRAPHIQUE
JABO_prop <- ggplot(prop_JABO, aes(x = Annee, y = prop_jeunes, group = 1))+
  geom_point(size = 3, col = "forestgreen")+
  geom_line(col= "forestgreen")+
  geom_abline(intercept = coef(lm_JABO_prop)[1], slope = coef(lm_JABO_prop)[2], color = "red")+
  labs(title = "Propition moyenne de jeunes du Jaseur boréal par année",
       x = "Année",
       y = "Proportion jeunes")+
  theme_classic()
JABO_prop <- JABO_prop+
  annotate(geom ="text",x = 2010, y = 1, label ="p = 0.34  R² = -0.003")
JABO_prop ### la propition change de 0.02 par année


# simulation
n_sim <- 5000
JABO_output_prop <- matrix(data = NA, nrow = n_sim, ncol = 2)
JABO_sim_prop <- prop_JABO[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  JABO_sim_prop$propition <- sample(x = prop_JABO$prop_jeunes, replace = FALSE)
  
  lm_JABO_rand_prop <- lm(propition ~ Annee, data = JABO_sim_prop)
  
  JABO_output_prop[i,1] <- coef(lm_JABO_rand_prop)[1] # Beta intercepte (abond_std)
  JABO_output_prop[i,2] <- coef(lm_JABO_rand_prop)[2] # Beta année
  
}
JABO_output_prop
summary(lm_JABO_rand_prop)
summary(lm_JABO_prop)
hist(JABO_output_prop[,1])
abline(v =(coef(lm_JABO_prop)[1]), lty = 2, col= "red")

hist(JABO_output_prop[,2])
abline(v =(coef(lm_JABO_prop)[2]), lty = 2, col= "red")

pbeta_annee <- sum(JABO_output_prop[, 2] >= coef(lm_JABO_prop)[2])/n_sim
pbeta_annee # 0.82

# graph coef
JABO_int_prop <- lm_JABO_prop$coefficients["(Intercept)"]
JABO_annee_prop <- lm_JABO_prop$coefficients["Annee"]
JABO_annee_prop
JABO_sd_prop <- coef(summary(lm_JABO_prop))[, "Std. Error"]
JABO_sd_prop

# Intercepte
upper_ic_annee_JABO_prop <- JABO_annee_prop + (1.96 * JABO_sd_prop[2])
upper_ic_annee_JABO_prop
lower_ic_annee_JABO_prop <- JABO_annee_prop - (1.96 * JABO_sd_prop[2])
lower_ic_annee_JABO_prop

plot((JABO_annee_prop),
     ylim = c(-0.040,0.040))
abline(h = 0, lty = 2, col = "red")
segments(x0 = 1, y0 = (lower_ic_annee_JABO_prop),
         x1 = 1, y1 = (upper_ic_annee_JABO_prop))


## DUSA
prop_DUSA <-abond_joint[abond_joint$Espece== "DUSA",c(1,3,8,9)]
prop_DUSA<- prop_DUSA[prop_DUSA$nb_total>10,] # ex pour filter avec seuil
lm_DUSA_prop <- lm(prop_jeunes ~ Annee, weight = nb_total, data = prop_DUSA) 
summary(lm_DUSA_prop)
# la régression n'explique rien 

par(mfrow = c(2,2))
plot(lm_DUSA_prop)
dev.off()

# GRAPHIQUE
DUSA_prop <- ggplot(prop_DUSA, aes(x = Annee, y = prop_jeunes, group = 1))+
  geom_point(size = 3, col = "orange")+
  geom_line(col= "orange")+
  geom_abline(intercept = coef(lm_DUSA_prop)[1], slope = coef(lm_DUSA_prop)[2], color = "red")+
  labs(title = "Proportion moyenne de jeunes du Durbec des sapins par année",
       x = "Année",
       y = "Proportion jeunes")+
  theme_classic()
DUSA_prop <- DUSA_prop+
  annotate(geom ="text",x = 2010, y = 1, label ="p = 0.58  R² = -0.05")
DUSA_prop ### la propition change de 0.02 par année


# simulation
n_sim <- 5000
DUSA_output_prop <- matrix(data = NA, nrow = n_sim, ncol = 2)
DUSA_sim_prop <- prop_DUSA[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  DUSA_sim_prop$propition <- sample(x = prop_DUSA$prop_jeunes, replace = FALSE)
  
  lm_DUSA_rand_prop <- lm(propition ~ Annee, data = DUSA_sim_prop)
  
  DUSA_output_prop[i,1] <- coef(lm_DUSA_rand_prop)[1] # Beta intercepte (abond_std)
  DUSA_output_prop[i,2] <- coef(lm_DUSA_rand_prop)[2] # Beta année
  
}
DUSA_output_prop
summary(lm_DUSA_rand_prop)
summary(lm_DUSA_prop)
hist(DUSA_output_prop[,1])
abline(v =(coef(lm_DUSA_prop)[1]), lty = 2, col= "red")

hist(DUSA_output_prop[,2])
abline(v =(coef(lm_DUSA_prop)[2]), lty = 2, col= "red")

pbeta_annee <- sum(DUSA_output_prop[, 2] >= coef(lm_DUSA_prop)[2])/n_sim
pbeta_annee # 0.70

# graph coef
DUSA_int_prop <- lm_DUSA_prop$coefficients["(Intercept)"]
DUSA_annee_prop <- lm_DUSA_prop$coefficients["Annee"]
DUSA_annee_prop
DUSA_sd_prop <- coef(summary(lm_DUSA_prop))[, "Std. Error"]
DUSA_sd_prop

# Intercepte
upper_ic_annee_DUSA_prop <- DUSA_annee_prop + (1.96 * DUSA_sd_prop[2])
upper_ic_annee_DUSA_prop
lower_ic_annee_DUSA_prop <- DUSA_annee_prop - (1.96 * DUSA_sd_prop[2])
lower_ic_annee_DUSA_prop

plot((DUSA_annee_prop),
     ylim = c(-0.040,0.040))
abline(h = 0, lty = 2, col = "red")
segments(x0 = 1, y0 = (lower_ic_annee_DUSA_prop),
         x1 = 1, y1 = (upper_ic_annee_DUSA_prop))


## SIFL # significatif !!
prop_SIFL <-abond_joint[abond_joint$Espece== "SIFL",c(1,3,8,9)]
#prop_SIFL<- prop_SIFL[prop_SIFL$nb_total>10,] # ex pour filter avec seuil
lm_SIFL_prop <- lm(prop_jeunes ~ Annee, weight = nb_total, data = prop_SIFL) 
summary(lm_SIFL_prop)
# la régression n'explique rien 

par(mfrow = c(2,2))
plot(lm_SIFL_prop)
dev.off()

# GRAPHIQUE
SIFL_prop <- ggplot(prop_SIFL, aes(x = Annee, y = prop_jeunes, group = 1))+
  geom_point(size = 3, col = "turquoise")+
  geom_line(col= "turquoise")+
  geom_abline(intercept = coef(lm_SIFL_prop)[1], slope = coef(lm_SIFL_prop)[2], color = "red")+
  labs(title = "Proportion moyenne de jeunes du Sizerin flammé par année",
       x = "Année",
       y = "Proportion jeunes")+
  theme_classic()
SIFL_prop <- SIFL_prop+
  annotate(geom ="text",x = 2010, y = 1, label ="p = 0.03  R² = 0.23")
SIFL_prop ### la propition change de 0.02 par année


# simulation
n_sim <- 5000
SIFL_output_prop <- matrix(data = NA, nrow = n_sim, ncol = 2)
SIFL_sim_prop <- prop_SIFL[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  SIFL_sim_prop$propition <- sample(x = prop_SIFL$prop_jeunes, replace = FALSE)
  
  lm_SIFL_rand_prop <- lm(propition ~ Annee, data = SIFL_sim_prop)
  
  SIFL_output_prop[i,1] <- coef(lm_SIFL_rand_prop)[1] # Beta intercepte (abond_std)
  SIFL_output_prop[i,2] <- coef(lm_SIFL_rand_prop)[2] # Beta année
  
}
SIFL_output_prop
summary(lm_SIFL_rand_prop)
summary(lm_SIFL_prop)
hist(SIFL_output_prop[,1])
abline(v =(coef(lm_SIFL_prop)[1]), lty = 2, col= "red")

hist(SIFL_output_prop[,2])
abline(v =(coef(lm_SIFL_prop)[2]), lty = 2, col= "red")

pbeta_annee <- sum(SIFL_output_prop[, 2] >= coef(lm_SIFL_prop)[2])/n_sim
pbeta_annee # 0.27

# graph coef
SIFL_int_prop <- lm_SIFL_prop$coefficients["(Intercept)"]
SIFL_annee_prop <- lm_SIFL_prop$coefficients["Annee"]
SIFL_annee_prop
SIFL_sd_prop <- coef(summary(lm_SIFL_prop))[, "Std. Error"]
SIFL_sd_prop

# Intercepte
upper_ic_annee_SIFL_prop <- SIFL_annee_prop + (1.96 * SIFL_sd_prop[2])
upper_ic_annee_SIFL_prop
lower_ic_annee_SIFL_prop <- SIFL_annee_prop - (1.96 * SIFL_sd_prop[2])
lower_ic_annee_SIFL_prop

plot((SIFL_annee_prop),
     ylim = c(-0.040,0.040))
abline(h = 0, lty = 2, col = "red")
segments(x0 = 1, y0 = (lower_ic_annee_SIFL_prop),
         x1 = 1, y1 = (upper_ic_annee_SIFL_prop))



## TAPI
prop_TAPI <-abond_joint[abond_joint$Espece== "TAPI",c(1,3,8,9)]
#prop_TAPI<- prop_TAPI[prop_TAPI$nb_total>10,] # ex pour filter avec seuil
lm_TAPI_prop <- lm(prop_jeunes ~ Annee, weight= nb_total, data = prop_TAPI) 
summary(lm_TAPI_prop)
# la régression n'explique rien 

par(mfrow = c(2,2))
plot(lm_TAPI_prop)
dev.off()

# GRAPHIQUE
TAPI_prop <- ggplot(prop_TAPI, aes(x = Annee, y = prop_jeunes, group = 1))+
  geom_point(size = 3, col = "violetred")+
  geom_line(col= "violetred")+
  geom_abline(intercept = coef(lm_TAPI_prop)[1], slope = coef(lm_TAPI_prop)[2], color = "red")+
  labs(title = "Proportion moyenne de jeunes du Tarin des pins par année",
       x = "Année",
       y = "Proportion jeunes")+
  theme_classic()
TAPI_prop <- TAPI_prop+
  annotate(geom ="text",x = 2010, y = 1, label ="p = 0.86  R² = -0.06")
TAPI_prop 


# simulation
n_sim <- 5000
TAPI_output_prop <- matrix(data = NA, nrow = n_sim, ncol = 2)
TAPI_sim_prop <- prop_TAPI[, c("Annee")]

set.seed(1234)

for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  TAPI_sim_prop$propition <- sample(x = prop_TAPI$prop_jeunes, replace = FALSE)
  
  lm_TAPI_rand_prop <- lm(propition ~ Annee, data = TAPI_sim_prop)
  
  TAPI_output_prop[i,1] <- coef(lm_TAPI_rand_prop)[1] # Beta intercepte (abond_std)
  TAPI_output_prop[i,2] <- coef(lm_TAPI_rand_prop)[2] # Beta année
  
}
TAPI_output_prop
summary(lm_TAPI_rand_prop)
summary(lm_TAPI_prop)
hist(TAPI_output_prop[,1])
abline(v =(coef(lm_TAPI_prop)[1]), lty = 2, col= "red")

hist(TAPI_output_prop[,2])
abline(v =(coef(lm_TAPI_prop)[2]), lty = 2, col= "red")

pbeta_annee <- sum(TAPI_output_prop[, 2] >= coef(lm_TAPI_prop)[2])/n_sim
pbeta_annee # 0.70

# graph coef
TAPI_int_prop <- lm_TAPI_prop$coefficients["(Intercept)"]
TAPI_annee_prop <- lm_TAPI_prop$coefficients["Annee"]
TAPI_annee_prop
TAPI_sd_prop <- coef(summary(lm_TAPI_prop))[, "Std. Error"]
TAPI_sd_prop

# Intercepte
upper_ic_annee_TAPI_prop <- TAPI_annee_prop + (1.96 * TAPI_sd_prop[2])
upper_ic_annee_TAPI_prop
lower_ic_annee_TAPI_prop <- TAPI_annee_prop - (1.96 * TAPI_sd_prop[2])
lower_ic_annee_TAPI_prop

plot((TAPI_annee_prop),
     ylim = c(-0.040,0.040))
abline(h = 0, lty = 2, col = "red")
segments(x0 = 1, y0 = (lower_ic_annee_TAPI_prop),
         x1 = 1, y1 = (upper_ic_annee_TAPI_prop))


### regrouper les graphique
plot_grid(DUSA_prop, JABO_prop, SIFL_prop, TAPI_prop )

tab_comp_prop <- data.frame(c(DUSA_int_prop, JABO_int_prop, TAPI_int_prop, SIFL_int_prop),
                            c(DUSA_annee_prop, JABO_annee_prop, TAPI_annee_prop, SIFL_annee_prop),
                            c(lower_ic_annee_DUSA_prop, lower_ic_annee_JABO_prop, lower_ic_annee_TAPI_prop, lower_ic_annee_SIFL_prop),
                            c(upper_ic_annee_DUSA_prop, upper_ic_annee_JABO_prop, upper_ic_annee_TAPI_prop, upper_ic_annee_SIFL_prop))

axex<- 1:4
rownames(tab_comp_prop) <- c("DUSA", "JABO", "TAPI", "SIFL")
colnames(tab_comp_prop) <- c("Intercepte", "annee", "ic_inf", "ic_sup")
tab_comp_prop
plot(tab_comp_prop$annee,
     xaxt = "n",
     ylim = c(-0.03, 0.03),
     ylab = "log estimé de beta",
     xlab = "Espèces",
     main = "Estimés de log beta avec IC 95% : proportion jeunes")
abline(h = 0, lty = 2, col = "red")
axis(side = 1, at = axex, labels = c("DUSA", "JABO", "TAPI", "SIFL"))
segments(x0 = 1, y0 = lower_ic_annee_DUSA_prop,
         x1 = 1, y1 = upper_ic_annee_DUSA_prop)
segments(x0 = 2, y0 = lower_ic_annee_JABO_prop,
         x1 = 2, y1 = upper_ic_annee_JABO_prop)
segments(x0 = 3, y0 = lower_ic_annee_TAPI_prop,
         x1 = 3, y1 = upper_ic_annee_TAPI_prop)
segments(x0 = 4, y0 = lower_ic_annee_SIFL_prop,
         x1 = 4, y1 = upper_ic_annee_SIFL_prop)



#### irruption (dernière ouverture : 26/03/2026) ####

# formule : D = N-P / sigma(détrendés = ecart type de la différence N-P)
# DUSA 
#range(log(DUSA$abond_std +1))
# doit-on utiliser le log +1 ? Si oui aucune irruption pour DUSA

#lm_DUSA <- lm(log(abond_std + 1) ~ Annee, data = DUSA)
#summary(lm_DUSA) # lm OK !

#prédiction modèle
#pred_DUSA <- predict(lm_DUSA, DUSA)
#pred_DUSA
ecart_DUSA<- DUSA$abond_std - mean(DUSA$abond_std) #numerateur N-P
ecart_DUSA


# ecart_DUSA<- (DUSA$abond_std) - mean(DUSA$abond_std) # avec la moyenne

sd(ecart_DUSA) # denominateur

sd(DUSA$abond_std)/mean(DUSA$abond_std)

deviation_DUSA<- ecart_DUSA/sd(ecart_DUSA) # D
deviation_DUSA
threshold_DUSA<-abs(min(deviation_DUSA))
threshold_DUSA

DUSA$irruption <- deviation_DUSA> threshold_DUSA
DUSA$irruption
print(DUSA[,c( "Annee", "irruption")], n=30)

# si log pas d'année à cause de la valeur 24 anormalement faible :(
# 3 valeurs si pas log

# DUSA modifié (on ne garde que 2007 et après)

DUSA_modif <- DUSA %>% 
  filter(Annee >= 12)

#range(log(DUSA_modif$abond_std +1))
# doit-on utiliser le log +1 ? Si oui aucune irruption pour DUSA

#lm_DUSA_modif <- lm(log(abond_std+1)~Annee, data = DUSA_modif)
#summary(lm_DUSA_modif) # lm OK !

#prédiction modèle
#pred_DUSA_modif <- predict(lm_DUSA_modif, DUSA_modif)

#ecart_DUSA_modif <- log(DUSA_modif$abond_std+1) - pred_DUSA_modif #numerateur N-P


ecart_DUSA_modif <- (DUSA_modif$abond_std) - mean(DUSA_modif$abond_std)
ecart_DUSA_modif

sd(ecart_DUSA_modif) # denominateur
deviation_DUSA_modif <- ecart_DUSA_modif/sd(ecart_DUSA_modif) # D


threshold_DUSA_modif <-abs(min(deviation_DUSA_modif))
threshold_DUSA_modif
DUSA_modif$irruption <- deviation_DUSA_modif > threshold_DUSA_modif
DUSA_modif$irruption
print(DUSA_modif[,c( "Annee", "irruption")], n=30)


# SIFL 
range(log(SIFL$abond_std +1))

# année 29 trop basse si log utilisé
lm_SIFL <- lm(log(abond_std + 1) ~ Annee, data = SIFL)


summary(lm_SIFL) # pas significatif donc on ne peut pas utiliser lm comme prédicteur
#pred_SIFL<- predict(lm_SIFL, SIFL)
#pred_SIFL

#ecart_SIFL<- (SIFL$abond_std) - pred_SIFL
ecart_SIFL<- (SIFL$abond_std) - mean(SIFL$abond_std)
ecart_SIFL
sd(ecart_SIFL)
deviation_SIFL<- ecart_SIFL/sd(ecart_SIFL)
deviation_SIFL
threshold_SIFL<-abs(min(deviation_SIFL))
SIFL$irruption <- deviation_SIFL> threshold_SIFL
SIFL$irruption
print(SIFL[,c( "Annee", "irruption")], n=30)

sd(SIFL$abond_std)/mean(SIFL$abond_std)

# JABO 
#vérifier le + à utiliser

range(log(JABO$abond_std+1))

#lm
lm_JABO <- lm(log(abond_std + 1) ~ Annee, data = JABO)
summary(lm_JABO)# pas significatif donc on ne peut pas utiliser lm comme prédicteur

#valeurs prédites
pred_JABO<- predict(lm_JABO, JABO)

# pred_JABO
# ecart_JABO<- (JABO$abond_std) - pred_JABO
ecart_JABO<- (JABO$abond_std) - mean(JABO$abond_std)
ecart_JABO
sd(ecart_JABO)
deviation_JABO<- ecart_JABO/sd(ecart_JABO)
deviation_JABO
threshold_JABO<-abs(min(deviation_JABO))
threshold_JABO
JABO$irruption <- deviation_JABO> threshold_JABO
JABO
print(JABO[,c( "Annee", "irruption")], n=30)
# 1 valeurs log +2 ; 4 sans log 

sd(JABO$abond_std)/mean(JABO$abond_std)

# TAPI 
range(log(TAPI$abond_std))

lm_TAPI <- lm(log(abond_std)~Annee, data = TAPI)
summary(lm_TAPI) # significatif : on peut utiliser lm"
pred_TAPI<- predict(lm_TAPI, TAPI)
pred_TAPI
ecart_TAPI<- (log(TAPI$abond_std)) - pred_TAPI
ecart_TAPI<- (TAPI$abond_std) - mean(TAPI$abond_std)

ecart_TAPI
sd(ecart_TAPI)
deviation_TAPI<- ecart_TAPI/sd(ecart_TAPI)
deviation_TAPI
threshold_TAPI<-abs(min(deviation_TAPI))
TAPI$irruption <- deviation_TAPI> threshold_TAPI
TAPI$irruption
print(TAPI[,c( "Annee", "irruption")], n=30)

sd(TAPI$abond_std)/mean(TAPI$abond_std)

# une seule année parce que sd est très élevé sinon utiliser log 













# Calcul des irruptions ---------------------------------------------------

#Article de Widck et al. (2023)

#We defined irruption years using the standardized deviate (Di,j) method, following LaMontagne and Boutin (2009). Standardized deviates were defined by

# Ni,j,t is the mean count of species i in cell j in year t, 
# Pi,j,t is the value predicted by the long-term trend in bird count for species i in cell j in year t, and 
# σi,j is the standard deviation for all detrended years for species i in cell j. The numerator in this expression detrends the time series and the denominator scales to unit standard deviation. Following LaMontagne and Boutin (2009), years with positive standardized deviates greater than the absolute value of the minimum deviate were considered anomalously high and indicative of irruption years.

#Qu'est-ce que ça veut dire ce paragraphe? 

# N: c'est la moyenne pour une année (nous on a une valeur donc c'est ça)
# P: c'est la valeur prédite à long terme d'une espèce à une année précise. Notre valeur prédite correspond au valeur prédite du modèle linéaire
# Sigma: c'est l'ecart-type du numérateur

# On prend la valeur la plus petite de D (exemple -2.1) en valeur absolue comme seuil (2.1). Si D d'une année dépasse ce seuil (ex: D = 3.4 en 2016), alors 2016 est une année irruptive. 

abond_irruption <- abond %>%
  group_by(Espece) %>% # Par espèce
  mutate( #On créé 6 nouvelles colonnes
    N = abond_std, #valeur de N (mean count)
    P  = mean(abond_std), # valeur de P (valeur prédite)
    numerateur   = N-P, 
    sigma       = sd(numerateur),
    standardized_deviate           = numerateur / sigma,
    seuil = abs(min(standardized_deviate)),
    irruption   = standardized_deviate > seuil
  ) %>%
  ungroup()
# BON CODE, IRRUPTION
unique(abond_irruption$seuil) # Les 4 valeurs sont 1.4724033 pour DUSA 1.2248917 pour JABO 1.0243263 pour SIFL et 0.8864361 pour TAPI

abond_irruption %>%
  filter(irruption == TRUE) %>%
  select(Espece, Annee, abond_std, standardized_deviate)



str(DUSA)

DUSA$irruption <- abond_irruption$irruption[abond_irruption$Espece == "DUSA"]
abond_irruption$irruption[abond_irruption$Espece == "DUSA"]
DUSA$irruption <- as.factor(DUSA$irruption)

plot_dusa_tot <- ggplot(DUSA, aes(x = Annee, y = abond_std, color = irruption))+
  scale_color_manual(values = c("TRUE" = "orange", "FALSE" = "black"))+
  geom_point(size = 4)+
  geom_path(linewidth = 1, col = "#a7a7a7")+
  labs(title = "Abondance du Durbec des Sapins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  geom_hline(yintercept = mean(DUSA$abond_std), col = "red", lty = 2)+
  scale_x_continuous(labels=c('1995', '2005', '2015', '2025'))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_dusa_tot
  
JABO$irruption <- abond_irruption$irruption[abond_irruption$Espece == "JABO"]
abond_irruption$irruption[abond_irruption$Espece == "JABO"]
JABO$irruption <- as.factor(JABO$irruption)

plot_JABO_tot <- ggplot(JABO, aes(x = Annee, y = abond_std, color = irruption))+
  scale_color_manual(values = c("TRUE" = "#009929", "FALSE" = "black"))+
  geom_point(size = 4)+
  geom_path(linewidth = 1, col = "#a7a7a7")+
  labs(title = "Abondance du Jaseur boréal par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  geom_hline(yintercept = mean(JABO$abond_std), col = "red", lty = 2)+
  scale_x_continuous(labels=c('1995', '2005', '2015', '2025'))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_JABO_tot

SIFL$irruption <- abond_irruption$irruption[abond_irruption$Espece == "SIFL"]
abond_irruption$irruption[abond_irruption$Espece == "SIFL"]
SIFL$irruption <- as.factor(SIFL$irruption)

plot_SIFL_tot <- ggplot(SIFL, aes(x = Annee, y = abond_std, color = irruption))+
  scale_color_manual(values = c("TRUE" = "turquoise", "FALSE" = "black"))+
  geom_point(size = 4)+
  geom_path(linewidth = 1, col = "#a7a7a7")+
  labs(title = "Abondance du Sizerin flammé par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  geom_hline(yintercept = mean(SIFL$abond_std), col = "red", lty = 2)+
  scale_x_continuous(labels=c('1995', '2005', '2015', '2025'))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_SIFL_tot


TAPI$irruption <- abond_irruption$irruption[abond_irruption$Espece == "TAPI"]
abond_irruption$irruption[abond_irruption$Espece == "TAPI"]
TAPI$irruption <- as.factor(TAPI$irruption)

plot_TAPI_tot <- ggplot(TAPI, aes(x = Annee, y = abond_std, color = irruption))+
  scale_color_manual(values = c("TRUE" = "violetred", "FALSE" = "black"))+
  geom_point(size = 4)+
  geom_path(linewidth = 1, col = "#a7a7a7")+
  labs(title = "Abondance du Tarin des pins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  geom_hline(yintercept = mean(TAPI$abond_std), col = "red", lty = 2)+
  scale_x_continuous(labels=c('1995', '2005', '2015', '2025'))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_TAPI_tot

plot_grid(plot_dusa_tot, plot_JABO_tot, plot_SIFL_tot, plot_TAPI_tot)


## graph déviation standard
par(mfrow=c(2,2))

range(abond_irruption$standardized_deviate[abond_irruption$Espece=="DUSA"])
cols=c("black", "orange")

col <- ifelse(abond_irruption$standardized_deviate[abond_irruption$Espece == "DUSA"] > 0.965,
              cols[2],  # vrai ="orange"
              cols[1]   # faux = "black"
)

deviate_dusa<-barplot( abond_irruption$standardized_deviate[abond_irruption$Espece=="DUSA"],
        abond_irruption$Annee[abond_irruption$Espece=="DUSA"],
        xlab= "Année", ylab= "Standard deviate", col= col, 
        main= "Déviation standard du durbec des sapins")
deviate_dusa<-deviate_dusa +abline(h= 0.96, col="red", lty=2)
deviate_dusa
#plot(abond_irruption$Annee[abond_irruption$Espece=="DUSA"], 
#    abond_irruption$standardized_deviate[abond_irruption$Espece=="DUSA"],
#   xlab= "Année", ylab= "Standard deviate", col= col, pch=20, cex=2 )
#abline(h=0, col="red", lty=2)
#abline(h= 0.96, col="#ff9800")

range(abond_irruption$standardized_deviate[abond_irruption$Espece=="JABO"])
cols=c("black", "darkgreen")

col <- ifelse(abond_irruption$standardized_deviate[abond_irruption$Espece == "JABO"] > 1.053,
              cols[2],  # vrai ="orange"
              cols[1]   # faux = "black"
)

deviate_JABO<-barplot( abond_irruption$standardized_deviate[abond_irruption$Espece=="JABO"],
                       abond_irruption$Annee[abond_irruption$Espece=="JABO"],
                       xlab= "Année", ylab= "Standard deviate", col= col,
                       main= "Déviation standard du jaseur boréal")
deviate_JABO<-deviate_JABO +abline(h= 1.053, col="red", lty=2)
deviate_JABO

range(abond_irruption$standardized_deviate[abond_irruption$Espece=="SIFL"])
cols=c("black", "turquoise")

col <- ifelse(abond_irruption$standardized_deviate[abond_irruption$Espece == "SIFL"] > 0.495,
              cols[2],  # vrai ="orange"
              cols[1]   # faux = "black"
)

deviate_SIFL<-barplot( abond_irruption$standardized_deviate[abond_irruption$Espece=="SIFL"],
                       abond_irruption$Annee[abond_irruption$Espece=="SIFL"],
                       xlab= "Année", ylab= "Standard deviate", col= col,
                       main= "Déviation standard du sizerin flammé")
deviate_SIFL<-deviate_SIFL +abline(h= 0.495, col="red", lty=2)

range(abond_irruption$standardized_deviate[abond_irruption$Espece=="TAPI"])
cols=c("black", "violet")

col <- ifelse(abond_irruption$standardized_deviate[abond_irruption$Espece == "TAPI"] > 0.525,
              cols[2],  # vrai ="orange"
              cols[1]   # faux = "black"
)

deviate_TAPI<-barplot( abond_irruption$standardized_deviate[abond_irruption$Espece=="TAPI"],
                       abond_irruption$Annee[abond_irruption$Espece=="TAPI"],
                       xlab= "Année", ylab= "Standard deviate", col= col,
                       main= "Déviation standard du tarin des pins")
deviate_TAPI<-deviate_TAPI +abline(h= 0.525, col="red", lty=2)

dev.off()





# Condition et proportion -------------------------------------------------

abond_irruption <- abond_irruption %>%
  select(-c(N, P, sigma, numerateur, seuil, standardized_deviate))

abond_irruption <- abond_irruption %>%
  left_join(abond_joint, by = c("Espece", "Annee")) %>% 
  filter(!Annee %in% c(1996,1997,1998, 1999, 2000,2001,2002,2003,2004,2005, 2006))


# jointure de la proportion de jeunes

abond_irruption <- abond_irruption %>%
  left_join(props_all, by = c("Espece" = "Espece", "Annee" = "Annee")) %>% 
  mutate(nb_total = nb_HY + nb_AHY) %>% 
  filter(!Annee %in% c(1996,1997,1998, 1999, 2000,2001,2002,2003,2004,2005, 2006))

# jointure avec abond_joint
abond_irruption <- abond_irruption %>%
  left_join(bague_joint, by = c("Espece" = "Abrv", "Annee" = "Annee")) %>% 
  filter(!Annee %in% c(1996,1997,1998, 1999, 2000,2001,2002,2003,2004,2005, 2006))

par(mfrow=c(2,2))
cols=c("black", "orange")

col <- ifelse(abond_irruption$irruption=="TRUE",
              cols[2],  # vrai ="orange"
              cols[1]   # faux = "black"
)

plot(abond_irruption$abond_std[abond_irruption$Espece=="TAPI"], 
     abond_irruption$nb_total[abond_irruption$Espece=="TAPI"], 
     ylab ="n bagué", xlab = "abond std.",main= "TAPI",
     col=col)

plot(abond_irruption$prop_jeunes[abond_irruption$Espece=="TAPI"], 
     abond_irruption$moyenne_condition[abond_irruption$Espece=="TAPI"], 
     ylab ="condition moyenne", xlab = "Proportion jeunes",main= "TAPI",
     col=col)


abond_irruption$prop_bague <- abond_irruption$nb_total/abond_irruption$abond


plot(abond_irruption$prop_jeunes[abond_irruption$Espece=="SIFL"], 
     abond_irruption$moyenne_condition[abond_irruption$Espece=="SIFL"], 
     ylab ="condition moyenne", xlab = "Proportion jeunes",main= "SIFL",
     col=col)

plot(abond_irruption$prop_jeunes[abond_irruption$Espece=="TAPI"], 
     abond_irruption$moyenne_condition[abond_irruption$Espece=="TAPI"], 
     ylab ="condition moyenne", xlab = "Proportion jeunes",main= "TAPI",
     col=col)

plot(abond_irruption$prop_jeunes[abond_irruption$Espece=="TAPI"], 
     abond_irruption$moyenne_condition[abond_irruption$Espece=="TAPI"], 
     ylab ="condition moyenne", xlab = "Proportion jeunes",main= "TAPI",
     col=col)
plot_condition_proportion_TAPI


range(abond_irruption$nb_total, na.omit = TRUE)

xtabs(nb_total ~ Espece + Annee, data = abond_irruption)

xtabs(irruption ~ Espece + Annee ,data = abond_irruption)

prop_tab <- xtabs(prop_bague ~ Espece + Annee ,data = abond_irruption)

round(prop_tab, 3)




# DUSA

irr <- abond_irruption[, c(1,3, 12)]
irr

abond_joint <- abond_joint %>% 
  left_join(irr, by = c("Annee", "Espece"))

DUSA <- abond_joint[1:19,]
irr_DUSA <- DUSA[, c(1,8,9, 10)]

# remove na

irr_DUSA <- na.omit(irr_DUSA)


boxplot(prop_jeunes ~ irruption.x, data = irr_DUSA)

range(irr_DUSA$prop_jeunes)


glm(prop_jeunes ~ irruption.x, data = irr_DUSA)

plot(prop_jeunes ~ nb_total, data = irr_DUSA)


# JABO

irr_jabo <- abond_joint[20:38,]
irr_jabo <- irr_jabo[, c(1,5,8,9,10)]

mod_jabo <- lm(abond_std ~ prop_jeunes, data = irr_jabo)

summary(mod_jabo)

plot(abond_std ~ prop_jeunes, data = irr_jabo)

plot(prop_jeunes ~ nb_total, data = irr_jabo)

dusanonirr


?weighted.mean

dusairr <- irr_DUSA[irr_DUSA$irruption.x == "TRUE",]
dusanonirr <- irr_DUSA[irr_DUSA$irruption.x == "FALSE",]

wirr <- weighted.mean(dusairr$prop_jeunes, dusairr$nb_total)
wnonirr <- weighted.mean(dusanonirr$prop_jeunes, dusanonirr$nb_total)

t.test(dusairr, dusanonirr)
 

mod_dusa <- glm(prop_jeunes ~ Annee + irruption.x, data = irr_DUSA, family = "binomial", weight = nb_std)

summary(mod_dusa)

par(mfrow = c(2,2))
plot(mod_dusa)

moy_ndusa <- mean(irr_DUSA$nb_total)
moy_ndusa
sd_ndusa <- sd(irr_DUSA$nb_total)

irr_DUSA$nb_std <- (irr_DUSA$nb_total - moy_ndusa)/sd_ndusa

irr_DUSA$nb_std <- inverse(irr_DUSA$nb_total)










