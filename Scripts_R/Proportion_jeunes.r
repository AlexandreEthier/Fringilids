# Library
#install.packages("emmeans")
#install.packages("ggpubr")

library(cowplot)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(nlme)
library(emmeans)
library(writexl)

#Importe le jeu de données
bague_clean <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Fringilids/Data/bague_clean.xlsx")
abond_clean <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Fringilids/Data/abond_clean.xlsx")

#Votre chemin messieurs
#bague_clean <- read_excel("")
#abond_clean <- read_excel("")
  
# Petite transformation
bague_clean$Age <- as.factor(bague_clean$Age)
bague_clean$Sexe <- as.factor(bague_clean$Sexe)
bague_clean$Aile <- as.numeric(bague_clean$Aile)
bague_clean$Gras <- as.factor(bague_clean$Gras)
bague_clean$Queue <- as.numeric(bague_clean$Queue)
bague_clean$Masse <- as.numeric(bague_clean$Masse)
bague_clean$Annee <- as.integer(bague_clean$Annee)

str(bague_clean)

# Petite transformation
abond_clean$Annee <- as.integer(abond_clean$Annee)
abond_clean$Effort <- as.integer(abond_clean$Effort)
abond_clean$abond_std <- as.numeric(abond_clean$abond_std)
abond_clean$Espece <- as.factor(abond_clean$Espece)

str(abond_clean)


# Calcul des proportions --------------------------------------------------

jeunes_par_an <- bague_clean %>%
  filter(Age != "U") %>%    # retire U du jeu de données
  group_by(Abrv, Annee) %>%
  summarise(
    nb_HY = sum(Age == "HY"),
    nb_AHY = sum(Age == "AHY"),
    prop_jeunes = nb_HY / (nb_HY + nb_AHY),
    .groups = "drop")

# 4 tableaux pour chaque espèce
tables_par_espece <- split(jeunes_par_an, jeunes_par_an$Abrv)
prop_Dubrec <- tables_par_espece[["DUSA"]]
prop_Sizerin <- tables_par_espece[["TAPI"]]
prop_Jaseur <- tables_par_espece[["SIFL"]]
prop_Tarin <- tables_par_espece[["JABO"]]


# Combiner tous les tableaux de proportions en un seul
props_all <- bind_rows(
  prop_Dubrec,
  prop_Sizerin,
  prop_Jaseur,
  prop_Tarin
)

# Jointure avec abond
abond_joint <- abond_clean %>%
  left_join(props_all, by = c("Espece" = "Abrv", "Annee" = "Annee"))

# On va retirer <2007
abond_joint <- abond_joint[abond_joint$Annee >= 2007, ]
# Vérification
head(abond_joint)


# Visualisation des jeunes dans le temps ----------------------------------

# À noter que pour DUSA, certaines années sont manquantes
plot_condition <- ggplot(abond_joint, aes(x = Annee, y = prop_jeunes, group = Espece, color = Espece))+
  geom_line() +
  geom_point(size = 3)+
  labs(title = "Évolution de la condition corporelle",
       x = "Année",
       y = "Proportion de jeunes",
       color = "Espèce")+
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_condition


# # Relation Abondance-Proportion de jeunes -------------------------------

# J'ai mis log + 3, parce que sinon j'avais deux valeurs aberrantes
plot_condition <- ggplot(abond_joint, aes(x = (log(abond_std) +3), y = prop_jeunes, group = Espece, color = Espece))+
  geom_line() +
  geom_point(size = 3)+
  labs(title = "Relation abondance standardisé et proportion de jeunes",
       x = "Abondance standardisé",
       y = "Proportion de jeunes",
       color = "Espèce")+
  stat_cor(method = "pearson") +
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_condition

# Graphique interactif qui montre quel point 
ggplotly(plot_condition, tooltip = c("x", "y", "colour"))

# 4 graphiques
plot_condition <- ggplot(abond_joint, aes(x = (log(abond_std) +3), y = prop_jeunes, color = Espece))+
  geom_line() +
  geom_point(size = 3)+
  facet_wrap(~ Espece)+
  labs(title = "Relation abondance standardisé et proportion de jeunes",
       x = "Abondance standardisé",
       y = "Proportion de jeunes",
       color = "Espèce")+
  stat_cor(method = "pearson") +
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_condition

# Modèle linéaire mixte ---------------------------------------------------------

mod1 <- lme(prop_jeunes ~ abond_std , random =~ 1 | Annee, data = abond_joint, na.action = na.omit)

mod2 <- lme(prop_jeunes ~ abond_std + Espece, random =~ 1 | Annee, data = abond_joint, na.action = na.omit)

# CA des deux modèles ---------------------------------------------------------

par(mfrow = c(2, 2))
##homogeneity of variance
plot(residuals(mod1, type = "pearson")~ fitted(mod1))
##normality of residuals
qqnorm(residuals(mod1, type = "pearson"))
qqline(residuals(mod1, type = "pearson"))
##normality of random intercepts
qqnorm(coef(mod1)$"(Intercept)")
qqline(coef(mod1)$"(Intercept)")
##boxplot of residuals
boxplot(residuals(mod1, type = "pearson")~ abond_joint$abond_std) # erreur au niveau de la longueur

par(mfrow = c(2, 2))
##homogeneity of variance
plot(residuals(mod2, type = "pearson")~ fitted(mod2))
##normality of residuals
qqnorm(residuals(mod2, type = "pearson"))
qqline(residuals(mod2, type = "pearson"))
##normality of random intercepts
qqnorm(coef(mod2)$"(Intercept)")
qqline(coef(mod2)$"(Intercept)")
##boxplot of residuals
boxplot(residuals(mod2, type = "pearson")~ abond_joint$abond_std)

#============================================================================

# Évolution condition corporelle ------------------------------------------

# Prépare le jeu de données

bague_joint <- bague_clean %>%
  group_by(Abrv, Annee) %>%
  summarise(
    moyenne_condition = mean(Condition, na.rm = TRUE),
    sd_condition = sd(Condition, na.rm = TRUE),
    .groups = "drop"
  )

# Jointure entre abond_joint et bague_joint

# Jointure avec abond
abond_joint <- abond_joint %>%
  left_join(bague_joint, by = c("Espece" = "Abrv", "Annee" = "Annee"))


# Visualisation de la condition corporelle --------------------------------

plot_condition <- ggplot(abond_joint, aes(x = Annee, y = moyenne_condition, group = Espece, color = Espece))+
  geom_line() +
  geom_point(size = 3)+
  labs(title = "Évolution de la condition corporelle",
       x = "Année",
       y = "Condition",
       color = "Espèce")+
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_condition

# 4 graphiques
plot_condition <- ggplot(abond_joint, aes(x = (Annee), y = moyenne_condition, color = Espece))+
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = moyenne_condition - sd_condition,
                    ymax = moyenne_condition + sd_condition)) +
  facet_wrap(~ Espece)+
  labs(title = "Relation abondance standardisé et proportion de jeunes",
       x = "Abondance standardisé",
       y = "Proportion de jeunes",
       color = "Espèce")+
  stat_cor(method = "pearson") +
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())
plot_condition

# 4 graphiques, mais séparé

plot_DUSA <- ggplot(abond_joint %>% filter(Espece == "DUSA"), 
                    aes(x = Annee, y = moyenne_condition))+
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = moyenne_condition - sd_condition,
                    ymax = moyenne_condition + sd_condition)) +
  labs(title = "DUSA", x = "Année", y = "Condition moyenne")+
  stat_cor(method = "pearson") +
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())

plot_JABO <- ggplot(abond_joint %>% filter(Espece == "JABO"), 
                    aes(x = Annee, y = moyenne_condition))+
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = moyenne_condition - sd_condition,
                    ymax = moyenne_condition + sd_condition)) +
  labs(title = "JABO", x = "Année", y = "Condition moyenne")+
  stat_cor(method = "pearson") +
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())

plot_SIFL <- ggplot(abond_joint %>% filter(Espece == "SIFL"), 
                    aes(x = Annee, y = moyenne_condition))+
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = moyenne_condition - sd_condition,
                    ymax = moyenne_condition + sd_condition)) +
  labs(title = "SIFL", x = "Année", y = "Condition moyenne")+
  stat_cor(method = "pearson") +
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())

plot_TAPI <- ggplot(abond_joint %>% filter(Espece == "TAPI"), 
                    aes(x = Annee, y = moyenne_condition))+
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = moyenne_condition - sd_condition,
                    ymax = moyenne_condition + sd_condition)) +
  labs(title = "TAPI", x = "Année", y = "Condition moyenne")+
  stat_cor(method = "pearson") +
  theme(axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.background = element_blank())

plot_DUSA
plot_JABO
plot_SIFL
plot_TAPI