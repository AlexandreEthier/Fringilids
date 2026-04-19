
# Chargements des bibliothèques -------------------------------------------

library(readxl)
library(tidyr)
library(dplyr)


# Chargement des répertoires de travail -----------------------------------

setwd("C:/Users/alexe/Fringilids/Data") # Alex
setwd("/Users/maxencepoirier-joanette/Rstudio/FOR7046") # Maxence

# Chargement des jeux de données originaux --------------------------------

# ABOND

abond <- read_excel("Abondance.xlsx")

# Changement headers

colnames(abond) <- c("Annee", "Effort", "DUSA", "JABO", "SIFL", "TAPI")

# Format "tidyr"

abond <- abond %>%
  pivot_longer(cols = c("DUSA", "JABO", "SIFL", "TAPI"),
    names_to = "Espece",
    values_to = "abond")

# csv object in wd
# write.csv(abond, file = "abond_clean.csv")


# BAGUE

bague <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Baguage.xlsx")
bague <- read_excel("/Users/alexe/Fringilids/Data/Baguage.xlsx")

head(bague)
str(bague)

colnames(bague) <- c("Pre", "Su", "Espèce", "Espece", "Age", "Sexe", "Aile", "Gras", "Queue", "Masse", "Annee", "Date", "Site", "Muni") # Standardiser headers


# Uniformiser colonnes

# Espèce

xtabs(~ Espece, data = bague)  #Standardiser TAPI

bague$Espece <- replace(bague$Espece, bague$Espece %in% "tapi", "TAPI")

# Âge

xtabs(~ Age, data = bague)  #Pooler les Ages (HY vs AHY)

bague$Age <- replace(bague$Age, bague$Age %in% c("Local", "S"), "U")   #Unknown
bague$Age <- replace(bague$Age, bague$Age %in% "hy", "HY")  #Juv (Hatch year)
bague$Age <- replace(bague$Age, bague$Age %in% c("SY", "ASY", "ahy"), "AHY")  #Non-juv (After-Hatch Year)

# FILTRE DF

bague <- bague[bague$Age != "U",] # Filter out Âge = U
bague <- bague[bague$Site == "Dunes",] #Sélection des sites de capture aux Dunes de Tadoussac

bague <- bague[, -c(1,2,3,6,8,9,12,13,14)] # Drop colonnes non-nécessaires

bague <- bague[bague$Annee >= "2007",] # Cut tout ce qui est avant 2007

str(bague)

# Espece
bague$Espece <- as.factor(bague$Espece)

# Age
bague$Age <- as.factor(bague$Age)

# Annee
bague$Annee <- as.factor(as.numeric(bague$Annee))
bague$Annee <- as.integer(bague$Annee)


# csv object in wd
# write.csv(bague, "bague_clean.csv")



# Jeux de données propres à chaque espèce ----------------------------------------

# DUSA

DUSA <- abond %>% 
  filter(Espece == "DUSA")

bague_DUSA <- bague[bague$Espece == "DUSA",]

# Calcul de l'abondance standardisée

DUSA$abond_std <- round(DUSA$abond/DUSA$Effort, 3) # Nb. d'oiseaux/h

# Calcul proportions HY

bague_DUSA <- bague_DUSA %>% 
  group_by(Annee) %>% 
  mutate(nb_HY = sum(Age == "HY"),
         nb_AHY = sum(Age == "AHY"),
         nb_tot = (nb_HY + nb_AHY),
         prop_HY = nb_HY/nb_tot,
         condition = Aile/Masse) %>%  # Ajout de la condition
ungroup()

# Extraction prop annuelle

DUSA_HY <- unique(bague_DUSA[, c(5,6)]) # On cherche toutes les valeurs uniques pour avoir nb_HY, nb_AHY et nb_tot PAR année
DUSA_HY <- DUSA_HY %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>% # Il manque certain rows (année où 0 capture). On les remplace donc par 0 
  replace(is.na(.), 0)

DUSA_AHY <- unique(bague_DUSA[, c(5,7)])
DUSA_AHY <- DUSA_AHY %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>%
  replace(is.na(.), 0)

DUSA_nbtot <- unique(bague_DUSA[, c(5,8)])
DUSA_nbtot <- DUSA_nbtot %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>%
  replace(is.na(.), 0)

# Extraction condition moyenne de l'espèce en fct. de l'année

DUSA_cond <- bague_DUSA %>% 
  group_by(Annee, Age) %>% 
  summarise(cond_moy = mean(na.omit(condition))) %>% 
ungroup()

DUSA_cond_HY <- DUSA_cond %>% 
  filter(Age == "HY") %>%
  complete(Annee = seq(min(Annee), max(Annee), 1))
  
  
DUSA_cond_AHY <- DUSA_cond %>% 
  filter(Age == "AHY") %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1))


# Création colonnes vides

DUSA$nb_HY <- NA
DUSA$nb_AHY <- NA
DUSA$nb_tot <- NA

DUSA$cond_moy_HY <- NA
DUSA$cond_moy_AHY <- NA

# Populer avec NA avec vecteurs

DUSA$nb_HY[12:length(DUSA$nb_HY)] <- DUSA_HY$nb_HY
DUSA$nb_AHY[12:length(DUSA$nb_AHY)] <- DUSA_AHY$nb_AHY

DUSA$nb_tot <- round(DUSA$nb_HY + DUSA$nb_AHY, 3)
DUSA$prop_HY <- round(DUSA$nb_HY/DUSA$nb_tot, 3)

DUSA$cond_moy_HY[12:length(DUSA$cond_moy_HY)] <- round(DUSA_cond_HY$cond_moy, 3)
DUSA$cond_moy_AHY[12:length(DUSA$cond_moy_AHY)] <- round(DUSA_cond_AHY$cond_moy, 3)


# Load vecteur irruption

load("DUSA_irr.R")

DUSA$irruption <- NA

DUSA$irruption[12:length(DUSA$irruption)] <- DUSA_irr

DUSA$irruption <- ifelse(DUSA$irruption == "TRUE", 1,0) # ifelse statement pour que irruption = TRUE soit 1
DUSA$irruption <- as.factor(as.numeric(DUSA$irruption))

# csv object in wd
# write.csv(DUSA, "DUSA.csv")


# JABO

JABO <- abond %>% 
  filter(Espece == "JABO")

bague_JABO <- bague[bague$Espece == "JABO",]

# Calcul de l'abondance standardisée

JABO$abond_std <- round(JABO$abond/JABO$Effort, 3) # Nb. d'oiseaux/h

# Calcul proportions HY

bague_JABO <- bague_JABO %>% 
  group_by(Annee) %>% 
  mutate(nb_HY = sum(Age == "HY"),
         nb_AHY = sum(Age == "AHY"),
         nb_tot = (nb_HY + nb_AHY),
         prop_HY = nb_HY/nb_tot,
         condition = Aile/Masse) %>%  # Ajout de la condition
  ungroup()

# Extraction prop annuelle

JABO_HY <- unique(bague_JABO[, c(5,6)]) # On cherche toutes les valeurs uniques pour avoir nb_HY, nb_AHY et nb_tot PAR année
JABO_HY <- JABO_HY %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>% # Il manque certain rows (année où 0 capture). On les remplace donc par 0 
  replace(is.na(.), 0)

JABO_AHY <- unique(bague_JABO[, c(5,7)])
JABO_AHY <- JABO_AHY %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>%
  replace(is.na(.), 0)

JABO_nbtot <- unique(bague_JABO[, c(5,8)])
JABO_nbtot <- JABO_nbtot %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>%
  replace(is.na(.), 0)

# Extraction condition moyenne de l'espèce en fct. de l'année

JABO_cond <- bague_JABO %>% 
  group_by(Annee, Age) %>% 
  summarise(cond_moy = mean(na.omit(condition))) %>% 
  ungroup()

JABO_cond_HY <- JABO_cond %>% 
  filter(Age == "HY") %>%
  complete(Annee = seq(min(Annee), max(Annee), 1))

JABO_cond_AHY <- JABO_cond %>% 
  filter(Age == "AHY") %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1))

# Création colonnes vides

JABO$nb_HY <- NA
JABO$nb_AHY <- NA
JABO$nb_tot <- NA

JABO$cond_moy_HY <- NA
JABO$cond_moy_AHY <- NA

# Populer avec NA avec vecteurs

JABO$nb_HY[12:length(JABO$nb_HY)] <- JABO_HY$nb_HY
JABO$nb_AHY[12:length(JABO$nb_AHY)] <- JABO_AHY$nb_AHY

JABO$nb_tot <- round(JABO$nb_HY + JABO$nb_AHY, 3)
JABO$prop_HY <- round(JABO$nb_HY/JABO$nb_tot, 3)

JABO$cond_moy_HY[12:length(JABO$cond_moy_HY)] <- round(JABO_cond_HY$cond_moy, 3)
JABO$cond_moy_AHY[12:length(JABO$cond_moy_AHY)] <- round(JABO_cond_AHY$cond_moy, 3)

# Load vecteur irruption

load("JABO_irr.R")

JABO$irruption <- NA

JABO$irruption[12:length(JABO$irruption)] <- JABO_irr

JABO$irruption <- ifelse(JABO$irruption == "TRUE", 1,0) # ifelse statement pour que irruption = TRUE soit 1
JABO$irruption <- as.factor(as.numeric(JABO$irruption))


# csv object in wd
# write.csv(JABO, "JABO.csv")


# SIFL

SIFL <- abond %>% 
  filter(Espece == "SIFL")

bague_SIFL <- bague[bague$Espece == "SIFL",]

# Calcul de l'abondance standardisée

SIFL$abond_std <- round(SIFL$abond/SIFL$Effort, 3) # Nb. d'oiseaux/h

# Calcul proportions HY

bague_SIFL <- bague_SIFL %>% 
  group_by(Annee) %>% 
  mutate(nb_HY = sum(Age == "HY"),
         nb_AHY = sum(Age == "AHY"),
         nb_tot = (nb_HY + nb_AHY),
         prop_HY = nb_HY/nb_tot,
         condition = Aile/Masse) %>%  # Ajout de la condition
  ungroup()

# Extraction prop annuelle

SIFL_HY <- unique(bague_SIFL[, c(5,6)]) # On cherche toutes les valeurs uniques pour avoir nb_HY, nb_AHY et nb_tot PAR année
SIFL_HY <- SIFL_HY %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>% # Il manque certain rows (année où 0 capture). On les remplace donc par 0 
  replace(is.na(.), 0)

SIFL_AHY <- unique(bague_SIFL[, c(5,7)])
SIFL_AHY <- SIFL_AHY %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>%
  replace(is.na(.), 0)

SIFL_nbtot <- unique(bague_SIFL[, c(5,8)])
SIFL_nbtot <- SIFL_nbtot %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>%
  replace(is.na(.), 0)

# Extraction condition moyenne de l'espèce en fct. de l'année

SIFL_cond <- bague_SIFL %>% 
  group_by(Annee, Age) %>% 
  summarise(cond_moy = mean(na.omit(condition))) %>% 
  ungroup()

SIFL_cond_HY <- SIFL_cond %>% 
  filter(Age == "HY") %>%
  complete(Annee = seq(min(Annee), max(Annee), 1))

SIFL_cond_AHY <- SIFL_cond %>% 
  filter(Age == "AHY") %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1))

# Création colonnes vides

SIFL$nb_HY <- NA
SIFL$nb_AHY <- NA
SIFL$nb_tot <- NA

SIFL$cond_moy_HY <- NA
SIFL$cond_moy_AHY <- NA

# Populer avec NA avec vecteurs

SIFL$nb_HY[12:length(SIFL$nb_HY)] <- SIFL_HY$nb_HY
SIFL$nb_AHY[12:length(SIFL$nb_AHY)] <- SIFL_AHY$nb_AHY

SIFL$nb_tot <- round(SIFL$nb_HY + SIFL$nb_AHY, 3)
SIFL$prop_HY <- round(SIFL$nb_HY/SIFL$nb_tot, 3)

SIFL$cond_moy_HY[12:length(SIFL$cond_moy_HY)] <- round(SIFL_cond_HY$cond_moy, 3)
SIFL$cond_moy_AHY[12:length(SIFL$cond_moy_AHY)] <- round(SIFL_cond_AHY$cond_moy, 3)

# Load vecteur irruption
load("SIFL_irr.R")

SIFL$irruption <- NA

SIFL$irruption[12:length(SIFL$irruption)] <- SIFL_irr

SIFL$irruption <- ifelse(SIFL$irruption == "TRUE", 1,0) # ifelse statement pour que irruption = TRUE soit 1
SIFL$irruption <- as.factor(as.numeric(SIFL$irruption))

# csv object in wd
# write.csv(SIFL, "SIFL.csv")


# TAPI

TAPI <- abond %>% 
  filter(Espece == "TAPI")

bague_TAPI <- bague[bague$Espece == "TAPI",]

# Calcul de l'abondance standardisée

TAPI$abond_std <- round(TAPI$abond/TAPI$Effort, 3) # Nb. d'oiseaux/h

# Calcul proportions HY

bague_TAPI <- bague_TAPI %>% 
  group_by(Annee) %>% 
  mutate(nb_HY = sum(Age == "HY"),
         nb_AHY = sum(Age == "AHY"),
         nb_tot = (nb_HY + nb_AHY),
         prop_HY = nb_HY/nb_tot,
         condition = Aile/Masse) %>%  # Ajout de la condition
  ungroup()

# Extraction prop annuelle

TAPI_HY <- unique(bague_TAPI[, c(5,6)]) # On cherche toutes les valeurs uniques pour avoir nb_HY, nb_AHY et nb_tot PAR année
TAPI_HY <- TAPI_HY %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>% # Il manque certain rows (année où 0 capture). On les remplace donc par 0 
  replace(is.na(.), 0)

TAPI_AHY <- unique(bague_TAPI[, c(5,7)])
TAPI_AHY <- TAPI_AHY %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>%
  replace(is.na(.), 0)

TAPI_nbtot <- unique(bague_TAPI[, c(5,8)])
TAPI_nbtot <- TAPI_nbtot %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1)) %>%
  replace(is.na(.), 0)

# Extraction condition moyenne de l'espèce en fct. de l'année

TAPI_cond <- bague_TAPI %>% 
  group_by(Annee, Age) %>% 
  summarise(cond_moy = mean(na.omit(condition))) %>% 
  ungroup()

TAPI_cond_HY <- TAPI_cond %>% 
  filter(Age == "HY") %>%
  complete(Annee = seq(min(Annee), max(Annee), 1))

TAPI_cond_AHY <- TAPI_cond %>% 
  filter(Age == "AHY") %>% 
  complete(Annee = seq(min(Annee), max(Annee), 1))

# Création colonnes vides

TAPI$nb_HY <- NA
TAPI$nb_AHY <- NA
TAPI$nb_tot <- NA

TAPI$cond_moy_HY <- NA
TAPI$cond_moy_AHY <- NA

# Populer avec NA avec vecteurs

TAPI$nb_HY[12:length(TAPI$nb_HY)] <- TAPI_HY$nb_HY
TAPI$nb_AHY[12:length(TAPI$nb_AHY)] <- TAPI_AHY$nb_AHY

TAPI$nb_tot <- round(TAPI$nb_HY + TAPI$nb_AHY, 3)
TAPI$prop_HY <- round(TAPI$nb_HY/TAPI$nb_tot, 3)

TAPI$cond_moy_HY[12:length(TAPI$cond_moy_HY)] <- round(TAPI_cond_HY$cond_moy, 3)
TAPI$cond_moy_AHY[12:length(TAPI$cond_moy_AHY)] <- round(TAPI_cond_AHY$cond_moy, 3)

# Load vecteur irruption
load("TAPI_irr.R")

TAPI$irruption <- NA

TAPI$irruption[12:length(TAPI$irruption)] <- TAPI_irr

TAPI$irruption <- ifelse(TAPI$irruption == "TRUE", 1,0) # ifelse statement pour que irruption = TRUE soit 1
TAPI$irruption <- as.factor(as.numeric(TAPI$irruption))

# csv object in wd
# write.csv(TAPI, "TAPI.csv")


#### objets OISEAU_bague

load("DUSA_irr.R")

DUSA_IRR <- as.data.frame(DUSA_irr[12:length(DUSA_irr)])
DUSA_IRR$Annee <- seq(1:length(DUSA_IRR$`DUSA_irr[12:length(DUSA_irr)]`))
DUSA_IRR$Espece <- "DUSA"
colnames(DUSA_IRR) <- c("irruption", "Annee", "Espece")

load("JABO_irr.R")

JABO_IRR <- as.data.frame(JABO_irr[12:length(JABO_irr)])
JABO_IRR$Annee <- seq(1:length(JABO_IRR$`JABO_irr[12:length(JABO_irr)]`))
JABO_IRR$Espece <- "JABO"
colnames(JABO_IRR) <- c("irruption", "Annee", "Espece")

load("SIFL_irr.R")

SIFL_IRR <- as.data.frame(SIFL_irr[12:length(SIFL_irr)])
SIFL_IRR$Annee <- seq(1:length(SIFL_IRR$`SIFL_irr[12:length(SIFL_irr)]`))
SIFL_IRR$Espece <- "SIFL"
colnames(SIFL_IRR) <- c("irruption", "Annee", "Espece")

load("TAPI_irr.R")

TAPI_IRR <- as.data.frame(TAPI_irr[12:length(TAPI_irr)])
TAPI_IRR$Annee <- seq(1:length(TAPI_IRR$`TAPI_irr[12:length(TAPI_irr)]`))
TAPI_IRR$Espece <- "TAPI"
colnames(TAPI_IRR) <- c("irruption", "Annee", "Espece")

bague_irr <- bague %>% 
  left_join(DUSA_IRR, by = c("Espece", "Annee")) %>% 
  left_join(JABO_IRR, by = c("Espece", "Annee")) %>% 
  left_join(SIFL_IRR, by = c("Espece", "Annee")) %>% 
  left_join(TAPI_IRR, by = c("Espece", "Annee"))
colnames(bague_irr) <- c("X", "Espece", "Age", "Aile", "Masse", "Annee", "DUSA_IRR", "JABO_IRR", "SIFL_IRR", "TAPI_IRR")


# Manipulation df bague

DUSA_bague <- bague_irr[bague_irr$Espece == "DUSA",]
DUSA_bague <- DUSA_bague[, 1:7]
DUSA_bague$DUSA_IRR <- ifelse(DUSA_bague$DUSA_IRR == "TRUE", 1, 0)
DUSA_bague$condition <- DUSA_bague$Aile/DUSA_bague$Masse

# save(DUSA_bague, file = "DUSA_bague.csv")

JABO_bague <- bague_irr[bague_irr$Espece == "JABO",]
JABO_bague <- JABO_bague[, c(1,2,3,4,5,6,8)]
JABO_bague$JABO_IRR <- ifelse(JABO_bague$JABO_IRR == "TRUE", 1, 0)
JABO_bague$condition <- JABO_bague$Aile/JABO_bague$Masse

# save(JABO_bague, file = "JABO_bague.csv")

SIFL_bague <- bague_irr[bague_irr$Espece == "SIFL",]
SIFL_bague <- SIFL_bague[, c(1,2,3,4,5,6,9)]
SIFL_bague$SIFL_IRR <- ifelse(SIFL_bague$SIFL_IRR == "TRUE", 1, 0)
SIFL_bague$condition <- SIFL_bague$Aile/SIFL_bague$Masse

# save(SIFL_bague, file = "SIFL_bague.csv")

TAPI_bague <- bague_irr[bague_irr$Espece == "TAPI",]
TAPI_bague <- TAPI_bague[, c(1,2,3,4,5,6,10)]
TAPI_bague$TAPI_IRR <- ifelse(TAPI_bague$TAPI_IRR == "TRUE", 1, 0)
TAPI_bague$condition <- TAPI_bague$Aile/TAPI_bague$Masse

# save(TAPI_bague, file = "TAPI_bague.csv")




