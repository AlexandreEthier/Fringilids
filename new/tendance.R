# Chargements des bibliothèques -------------------------------------------

library(readxl)


# Chargement des répertoires de travail -----------------------------------

setwd("C:/Users/alexe/Fringilids/Data") # Alex


# Chargement données ------------------------------------------------------

abond <- read.csv("abond_clean.csv", header = TRUE)

DUSA <- read.csv("DUSA.csv", header = TRUE)
JABO <- read.csv("JABO.csv", header = TRUE)
SIFL <- read.csv("SIFL.csv", header = TRUE)
TAPI <- read.csv("TAPI.csv", header = TRUE)

# Filtrer tout avant 2007

DUSA <- DUSA[DUSA$Annee >= "2007",]
JABO <- JABO[JABO$Annee >= "2007",]
SIFL <- SIFL[SIFL$Annee >= "2007",]
TAPI <- TAPI[TAPI$Annee >= "2007",]

DUSA$Annee <- as.factor(as.numeric(DUSA$Annee))
DUSA$Annee <- as.integer(DUSA$Annee)

JABO$Annee <- as.factor(as.numeric(JABO$Annee))
JABO$Annee <- as.integer(JABO$Annee)

SIFL$Annee <- as.factor(as.numeric(SIFL$Annee))
SIFL$Annee <- as.integer(SIFL$Annee)

TAPI$Annee <- as.factor(as.numeric(TAPI$Annee))
TAPI$Annee <- as.integer(TAPI$Annee)

##### DUSA

# Représentation graphique de l'abondance standardisée par année

abond_plot <- ggplot(abond, aes(x = Annee, y = abond_std, group = Espece, color = Espece))+
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
abond_plot


# Représentation graphique du LOG de l'abondance standardisée par année

abond_log_plot <- ggplot(abond, aes(x = Annee, y = log(abond_std), group = Espece, color = Espece))+
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
abond_log_plot


# Tendance temporelle -----------------------------------------------------

##### DUSA

# Modèle linéaire

lm_DUSA <- lm(log(abond_std) ~ Annee, data = DUSA)
summary(lm_DUSA)

# Vérification des suppositions du modèle
par(mfrow = c(2,2))
plot(lm_DUSA) # OK
dev.off()

# Extraction des coefficients

DUSA_int <- lm_DUSA$coefficients["(Intercept)"]
DUSA_annee <- lm_DUSA$coefficients["Annee"]
DUSA_sd <- coef(summary(lm_DUSA))[, "Std. Error"]

# Extraction des IC

# interceptes
DUSA_upperIC_int <- DUSA_int + (1.96 * DUSA_sd[1])
DUSA_lowerIC_int <- DUSA_int - (1.96 * DUSA_sd[1])

# annee
DUSA_upperIC_annee <- DUSA_annee + (1.96 * DUSA_sd[2])
DUSA_loweric_annee <- DUSA_annee - (1.96 * DUSA_sd[2])

# Représentation graphique

plot(exp(DUSA_annee),
     ylim = c(0.80, 1.25))
abline(h = 1, lty = 2, col = "red")
segments(x0 = 1, y0 = exp(DUSA_loweric_annee),
         x1 = 1, y1 = exp(DUSA_upperIC_annee))

# Test de randomisation afin d'obtenir des prédictions de lm

# Paramètres randomisation

n_sim <- 5000

DUSA_output <- matrix(data = NA, nrow = n_sim, ncol = 2)
DUSA_sim <- DUSA[, "Annee"]
DUSA_sim <- as.data.frame(DUSA_sim)
DUSA_sim <- rename(DUSA_sim, Annee = "DUSA_sim")


set.seed(0088)

# BOUCLE DE RANDOMISATION
#for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  DUSA_sim$abond_std <- sample(x = DUSA$abond_std, replace = FALSE)
  
  lm_DUSA_rand <- lm(log(abond_std) ~ Annee, data = DUSA_sim)
  
  DUSA_output[i,1] <- coef(lm_DUSA_rand)[1] # Beta intercepte (abond_std)
  DUSA_output[i,2] <- coef(lm_DUSA_rand)[2] # Beta année
}

# save(DUSA_output, file = "DUSA_output.R")
load("DUSA_output.R")

hist(DUSA_output[,1])
hist(DUSA_output[,2])

DUSA_beta_annee <- sum(DUSA_output[, 2] >= coef(lm_DUSA)[2])/n_sim

exp(DUSA_annee) # 1,05 oiseau de plus/heure/année 
exp(DUSA_int) 

DUSA_pred <- predict.lm(lm_DUSA)

# Représentation graphique

DUSA_log <- ggplot(DUSA, aes(x = Annee, y = log(abond_std), group = 1))+
  geom_point(size = 3, col = "darkorange1")+
  geom_line()+
  geom_abline(intercept = coef(lm_DUSA)[1], slope = coef(lm_DUSA)[2], color = "red")+
  labs(title = "Abondance du Durbec des Sapins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
DUSA_log <- DUSA_log+
  annotate(geom ="text",x = 5, y = 5, label ="y = 1.11 + 0.05x")
DUSA_log


##### JABO






##### SIFL




##### TAPI





##### GROUPED SP.




























