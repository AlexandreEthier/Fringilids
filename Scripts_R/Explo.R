
# Chargement des packages -------------------------------------------------

install.packages("emmeans")

library(cowplot)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(nlme)
library(emmeans)
library(writexl)

# Chargement des données ---------------------------------------------------

# IMPORTANT - Spécifier votre propre chemin qui mène aux documents
# IMPORTANT - SVP changez les virgules (,) en points (.) à même les deux fichiers Excel

# Maxence
abond <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Abondance.xlsx")
bague <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Baguage.xlsx")

# Alex: C:/Users/alexe/Fringilids/Data/Abondance.xlsx
#       C:/Users/alexe/Fringilids/Data/Baguage.xlsx
setwd("C:/Users/alexe/Fringilids")

# Bérince:
#

# Adrien:
abond <- read_excel("Abondance.xlsx") %>% 
rename(Annee = "Année", 
       DUSA = "Durbec des sapins", 
       JABO = "Jaseur boréal", 
       SIFL = "Sizerin flammé",
       TAPI = "Tarin des pins")
bague <- read_excel("Baguage.xlsx") %>% 
  rename(Espece = "Espèce",
         Abrv = "Espèce (abréviation)",
         Age = "Âge",
         Annee = "Année")

# Formatage - Abondance ---------------------------------------------------------------

abond <- abond %>% # Ajoutez votre propre chemin
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

# Créer Excel avec nouvelles modifications
#chemin1 <- "/Users/maxencepoirier-joanette/Rstudio/FOR7046/Fringilids/Data/abond_clean.xlsx"
# Exporter (my_data_expanded=jeu de données dans envrionnement)
#write_xlsx(abond, path = chemin1)


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

bague <- bague %>%  # Ajoutez votre propre chemin
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

# Exporter le dossier bague tout nettoyé dans un Excel (au lieu de rouler le code à chaque fois)

#chemin <- "/Users/maxencepoirier-joanette/Rstudio/FOR7046/Fringilids/Data/bague_clean.xlsx"
# Exporter (my_data_expanded=jeu de données dans envrionnement)
#write_xlsx(bague, path = chemin)

## Ajouter la proportion de jeunes dans la population pour chaque espèce
unique(bague$Espece)

jeunes_par_an <- bague %>%
  filter(Age != "U") %>%    # retire U du jeu de données
  group_by(Abrv, Annee) %>%
  summarise(
    nb_HY = sum(Age == "HY"),
    nb_AHY = sum(Age == "AHY"),
    prop_jeunes = nb_HY / (nb_HY + nb_AHY),
    .groups = "drop"
  )

unique(bague$Abrv)
tables_par_espece <- split(jeunes_par_an, jeunes_par_an$Abrv)
prop_Dubrec <- tables_par_espece[["DUSA"]]
prop_Sizerin <- tables_par_espece[["TAPI"]]
prop_Jaseur <- tables_par_espece[["SIFL"]]
prop_Tarin <- tables_par_espece[["JABO"]]


# Combiner tous les tableaux de proportions en un seul
props_all <- bind_rows(
  prop_Dubrec_sel,
  prop_Sizerin_sel,
  prop_Jaseur_sel,
  prop_Tarin_sel
)

# Jointure avec abond
abond_joint <- abond %>%
  left_join(props_all, by = c("Espece" = "Abrv", "Annee" = "Annee"))

# Vérification
head(abond_joint)



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
  labs(title = "Abondance du Durbec des Sapins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_dusa_tot

DUSA_log <- ggplot(DUSA, aes(x = Annee, y = log(abond_std), group =1))+
  geom_point(size = 3, col = "darkorange1")+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  labs(title = "Abondance du Durbec des Sapins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
DUSA_log

lm_DUSA <- lm(abond$abond_std[abond$Espece == "DUSA"] ~ Annee, data = abond)


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
       y = "N. individus recensés * heure-1")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_dusa_tot

# combine les deux graphique :
plot_grid(plot_dusa_tot, plot_condition_DUSA, ncol = 1) #graphique bof

# Condition
bague_DUSA <- bague[bague$Espece == "Durbec des sapins",] # Ne garde que DUSA
bague_DUSA <- bague_DUSA[bague_SIFL$Age != "U",] # Ne garde que les individus âgés

bague_DUSA <- bague_DUSA %>% # Regroupement des oiseaux bagués/année
  group_by(Annee) %>% 
  filter(between(Annee, 2007, 2025)) # Baguage plus régulier à partir de 2006 (ou 2007, à vérifier)
bague_annuel_DUSA <- bague_DUSA %>% # Nb. de SIFL bagués / année
  summarise(n())

ann <- seq(2007, 2025, 1) # Création d'un vecteur peuplé de 0

#n_bague_ann_DUSA <- as.data.frame(ann) %>% 
  rename(Annee = "ann") %>% 
  left_join(bague_annuel_DUSA, by = "Annee") %>% 
  cbind(bague_annuel_DUSA$`n()`/DUSA$abond) %>%            # Calcul la proportion de SIFL bagué par année
  rename(prop_bague = "n_bague_ann_DUSA$`n()`/DUSA$abond")


# Condition jeunes vs adultes par année

ggplot(bague_DUSA, aes(x = Annee, y = Condition, group = interaction(Annee, Age)))+
  geom_boxplot(aes(fill = Age))+
  theme_classic() #encore U






# JABO (Bérince) ----------------------------------------------------------

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
  labs(title = "Abondance du Sizerin Flammé par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.8))
plot_sifl_tot

SIFL_log <- ggplot(SIFL, aes(x = Annee, y = log(abond_std), group = 1))+
  geom_point(size = 3, col = "turquoise4")+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  labs(title = "Abondance du Sizerin Flammé par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
SIFL_log


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

str(TAPI)


# TAPI (Maxence) ----------------------------------------------------------

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

TAPI_log <- ggplot(TAPI, aes(x = Annee, y = log(abond_std), group =1))+
  geom_point(size = 3, col = "violetred4")+
  geom_line()+
  geom_abline(intercept = coef(lm_TAPI)[1], slope = coef(lm_TAPI)[2], color = "red")+
  labs(title = "Abondance du Tarin des Pins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
TAPI_log+
  annotate(geom ="text",x = 5, y = 5, label ="y = 1.66 + 0.067x")

summary(lm_TAPI)

#stat_cor(method = "pearson")+
  



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
plot_grid(DUSA_log,
          TAPI_log,
          SIFL_log,
          JABO_log, ncol = 2)




# TENDANCE TEMPORELLE -----------------------------------------------------

# Tendance temporelle depuis 1996 - 2025

# SIFL

abond_modif_SIFL <- log(SIFL$abond_std + 1)
abond_modif_SIFL

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
upper_ic_annee_SIFL
lower_ic_annee_SIFL <- SIFL_annee - (1.96 * SIFL_sd[2])
lower_ic_annee_SIFL

plot(exp(SIFL_annee),
     ylim = c(0.95, 1.15))
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

exp(SIFL_annee) # 1,07 oiseau de plus/heure/année 
exp(SIFL_int) 


# TAPI

abond_modif_TAPI <- log(TAPI$abond_std + 1)
abond_modif_TAPI

lm_TAPI <- lm(log(abond_std) ~ Annee, data = TAPI)

summary(lm_TAPI)

plot(lm_TAPI)


TAPI_int <- lm_TAPI$coefficients["(Intercept)"]
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
     ylim = c(0.95, 1.15))
abline(h = 1, lty = 2, col = "red")
segments(x0 = 1, y0 = exp(lower_ic_annee),
         x1 = 1, y1 = exp(upper_ic_annee))


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


# DUSA

DUSA$Annee <- as.factor(as.numeric(DUSA$Annee))
DUSA$Annee <- as.integer(DUSA$Annee)

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
     ylim = c(0.95, 1.15))
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

exp(DUSA_annee) # 1,07 oiseau de plus/heure/année 
exp(DUSA_int) 


# JABO

# Puisqu'on a une abondance ~ 0 (log(8)) -> +1 sur toutes les données pour éviter 0

abond_modif_JABO <- log(JABO$abond_std + 1)
abond_modif_JABO

lm_JABO <- lm(abond_modif_JABO ~ Annee, data = JABO)

summary(lm_JABO)

par(mfrow = c(2,2))
plot(lm_JABO) # MIEUX!!

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
     ylim = c(0.95, 1.15))
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

exp(JABO_annee) # 1,07 oiseau de plus/heure/année 
exp(JABO_int) 



# Mettre les "prédictions" en un seul graphique

SIFL_int
upper_ic_annee_SIFL
lower_ic_annee_SIFL

JABO_int
upper_ic_annee_JABO
lower_ic_annee_JABO

DUSA_int
upper_ic_annee_DUSA
lower_ic_annee_DUSA

TAPI_int
upper_ic_annee_TAPI
lower_ic_annee_TAPI

tab_comp <- data.frame(c(DUSA_int, JABO_int, TAPI_int, SIFL_int),
                       c(DUSA_annee, JABO_annee, TAPI_annee, SIFL_annee),
                       c(lower_ic_annee_DUSA, lower_ic_annee_JABO, lower_ic_annee_TAPI, lower_ic_annee_SIFL),
                       c(upper_ic_annee_DUSA, upper_ic_annee_JABO, upper_ic_annee_TAPI, upper_ic_annee_SIFL))


rownames(tab_comp) <- c("DUSA", "JABO", "TAPI", "SIFL")
colnames(tab_comp) <- c("Intercepte", "annee", "ic_inf", "ic_sup")

axex <- 1:4

plot(tab_comp$annee, data = tab_comp,
     xaxt = "n",
     ylim = c(-0.1, 0.15),
     ylab = "log estimé de beta",
     xlab = "Espèces",
     main = "Estimés de log beta avec IC 95%")
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





