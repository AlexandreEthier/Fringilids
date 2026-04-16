# Chargements des bibliothèques -------------------------------------------

library(readxl)
library(cowplot)


# Chargement des répertoires de travail -----------------------------------

setwd("C:/Users/alexe/Fringilids/Data") # Alex


# Chargement données ------------------------------------------------------

abond <- read.csv("abond_clean.csv", header = TRUE)

DUSA <- read.csv("DUSA.csv", header = TRUE)
JABO <- read.csv("JABO.csv", header = TRUE)
SIFL <- read.csv("SIFL.csv", header = TRUE)
TAPI <- read.csv("TAPI.csv", header = TRUE)

# Filtrer tout avant 2007

#DUSA <- DUSA[DUSA$Annee >= "2007",]
#JABO <- JABO[JABO$Annee >= "2007",]
#SIFL <- SIFL[SIFL$Annee >= "2007",]
#TAPI <- TAPI[TAPI$Annee >= "2007",]

DUSA$Annee <- as.factor(as.numeric(DUSA$Annee))
DUSA$Annee <- as.integer(DUSA$Annee)

JABO$Annee <- as.factor(as.numeric(JABO$Annee))
JABO$Annee <- as.integer(JABO$Annee)

SIFL$Annee <- as.factor(as.numeric(SIFL$Annee))
SIFL$Annee <- as.integer(SIFL$Annee)

TAPI$Annee <- as.factor(as.numeric(TAPI$Annee))
TAPI$Annee <- as.integer(TAPI$Annee)

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


# Tendance temporelle abondance -----------------------------------------------------

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
DUSA_lowerIC_annee <- DUSA_annee - (1.96 * DUSA_sd[2])

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
# for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  DUSA_sim$abond_std <- sample(x = DUSA$abond_std, replace = FALSE)
  
  lm_DUSA_rand <- lm(log(abond_std) ~ Annee, data = DUSA_sim)
  
  DUSA_output[i,1] <- coef(lm_DUSA_rand)[1] # Beta intercepte (abond_std)
  DUSA_output[i,2] <- coef(lm_DUSA_rand)[2] # Beta année
  #}

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

# Modèle linéaire

lm_JABO <- lm(log(abond_std) ~ Annee, data = JABO)
summary(lm_JABO)

# Vérification des suppositions du modèle
par(mfrow = c(2,2))
plot(lm_JABO) # OK
dev.off()

# Extraction des coefficients

JABO_int <- lm_JABO$coefficients["(Intercept)"]
JABO_annee <- lm_JABO$coefficients["Annee"]
JABO_sd <- coef(summary(lm_JABO))[, "Std. Error"]

# Extraction des IC

# interceptes
JABO_upperIC_int <- JABO_int + (1.96 * JABO_sd[1])
JABO_lowerIC_int <- JABO_int - (1.96 * JABO_sd[1])

# annee
JABO_upperIC_annee <- JABO_annee + (1.96 * JABO_sd[2])
JABO_lowerIC_annee <- JABO_annee - (1.96 * JABO_sd[2])

# Représentation graphique

plot(exp(JABO_annee),
     ylim = c(0.80, 1.25))
abline(h = 1, lty = 2, col = "red")
segments(x0 = 1, y0 = exp(JABO_loweric_annee),
         x1 = 1, y1 = exp(JABO_upperIC_annee))


# Test de randomisation afin d'obtenir des prédictions de lm

# Paramètres randomisation

n_sim <- 5000

JABO_output <- matrix(data = NA, nrow = n_sim, ncol = 2)
JABO_sim <- JABO[, "Annee"]
JABO_sim <- as.data.frame(JABO_sim)
JABO_sim <- rename(JABO_sim, Annee = "JABO_sim")

set.seed(1234)

#for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  JABO_sim$abond_std <- sample(JABO$abond_std, replace = FALSE)
  
  lm_JABO_rand <- lm(log(abond_std) ~ Annee, data = JABO_sim)
  
  JABO_output[i,1] <- coef(lm_JABO_rand)[1] # Beta intercepte (abond_std)
  JABO_output[i,2] <- coef(lm_JABO_rand)[2] # Beta année
  
#}

# save(JABO_output, file = "JABO_output.R")
load("JABO_output.R")

hist(JABO_output[,1])
hist(JABO_output[,2])

JABO_beta_annee <- sum(JABO_output[, 2] >= coef(lm_JABO)[2])/n_sim

exp(JABO_annee) # 1,05 oiseau de plus/heure/année 
exp(JABO_int) 

JABO_pred <- predict.lm(lm_JABO)

# Représentation graphique

JABO_log <- ggplot(JABO, aes(x = Annee, y = log(abond_std), group = 1))+
  geom_point(size = 3, col = "forestgreen")+
  geom_line()+
  geom_abline(intercept = coef(lm_JABO)[1], slope = coef(lm_JABO)[2], color = "red")+
  labs(title = "Abondance du Jaseur boréal par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
JABO_log <- JABO_log+
  annotate(geom ="text",x = 5, y = 5, label ="y = 1.11 + 0.05x")
JABO_log


##### SIFL

# Modèle linéaire

lm_SIFL <- lm(log(abond_std) ~ Annee, data = SIFL)
summary(lm_SIFL)

# Vérification des suppositions du modèle
par(mfrow = c(2,2))
plot(lm_SIFL) # OK
dev.off()

# Extraction des coefficients

SIFL_int <- lm_SIFL$coefficients["(Intercept)"]
SIFL_annee <- lm_SIFL$coefficients["Annee"]
SIFL_sd <- coef(summary(lm_SIFL))[, "Std. Error"]

# Extraction des IC

# interceptes
SIFL_upperIC_int <- SIFL_int + (1.96 * SIFL_sd[1])
SIFL_lowerIC_int <- SIFL_int - (1.96 * SIFL_sd[1])

# annee
SIFL_upperIC_annee <- SIFL_annee + (1.96 * SIFL_sd[2])
SIFL_lowerIC_annee <- SIFL_annee - (1.96 * SIFL_sd[2])

# Représentation graphique

plot(exp(SIFL_annee),
     ylim = c(0.80, 1.25))
abline(h = 1, lty = 2, col = "red")
segments(x0 = 1, y0 = exp(SIFL_lowerIC_annee),
         x1 = 1, y1 = exp(SIFL_upperIC_annee))


# Test de randomisation afin d'obtenir des prédictions de lm

# Paramètres randomisation

n_sim <- 5000

SIFL_output <- matrix(data = NA, nrow = n_sim, ncol = 2)
SIFL_sim <- SIFL[, "Annee"]
SIFL_sim <- as.data.frame(SIFL_sim)
SIFL_sim <- rename(SIFL_sim, Annee = "SIFL_sim")

set.seed(4574)

#for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  SIFL_sim$abond_std <- sample(SIFL$abond_std, replace = FALSE)
  
  lm_SIFL_rand <- lm(log(abond_std) ~ Annee, data = SIFL_sim)
  
  SIFL_output[i,1] <- coef(lm_SIFL_rand)[1] # Beta intercepte (abond_std)
  SIFL_output[i,2] <- coef(lm_SIFL_rand)[2] # Beta année
  
#}

# save(SIFL_output, file = "SIFL_output.R")
load("SIFL_output.R")

hist(SIFL_output[,1])
hist(SIFL_output[,2])

SIFL_beta_annee <- sum(SIFL_output[, 2] >= coef(lm_SIFL)[2])/n_sim

exp(SIFL_annee) # 1,05 oiseau de plus/heure/année 
exp(SIFL_int) 

SIFL_pred <- predict.lm(lm_SIFL)

# Représentation graphique

SIFL_log <- ggplot(SIFL, aes(x = Annee, y = log(abond_std), group = 1))+
  geom_point(size = 3, col = "forestgreen")+
  geom_line()+
  geom_abline(intercept = coef(lm_SIFL)[1], slope = coef(lm_SIFL)[2], color = "red")+
  labs(title = "Abondance du Sizerin flammé par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
SIFL_log <- SIFL_log+
  annotate(geom ="text",x = 5, y = 5, label ="y = 1.11 + 0.05x")
SIFL_log


##### TAPI

# Modèle linéaire

lm_TAPI <- lm(log(abond_std) ~ Annee, data = TAPI)
summary(lm_TAPI)

# Vérification des suppositions du modèle
par(mfrow = c(2,2))
plot(lm_TAPI) # OK
dev.off()

# Extraction des coefficients

TAPI_int <- lm_TAPI$coefficients["(Intercept)"]
TAPI_annee <- lm_TAPI$coefficients["Annee"]
TAPI_sd <- coef(summary(lm_TAPI))[, "Std. Error"]

# Extraction des IC

# interceptes
TAPI_upperIC_int <- TAPI_int + (1.96 * TAPI_sd[1])
TAPI_lowerIC_int <- TAPI_int - (1.96 * TAPI_sd[1])

# annee
TAPI_upperIC_annee <- TAPI_annee + (1.96 * TAPI_sd[2])
TAPI_lowerIC_annee <- TAPI_annee - (1.96 * TAPI_sd[2])

# Représentation graphique

plot(exp(TAPI_annee),
     ylim = c(0.80, 1.25))
abline(h = 1, lty = 2, col = "red")
segments(x0 = 1, y0 = exp(TAPI_lowerIC_annee),
         x1 = 1, y1 = exp(TAPI_upperIC_annee))


# Test de randomisation afin d'obtenir des prédictions de lm

# Paramètres randomisation

n_sim <- 5000

TAPI_output <- matrix(data = NA, nrow = n_sim, ncol = 2)
TAPI_sim <- TAPI[, "Annee"]
TAPI_sim <- as.data.frame(TAPI_sim)
TAPI_sim <- rename(TAPI_sim, Annee = "TAPI_sim")

set.seed(6666)

#for(i in 1:n_sim){
  
  cat("iter = ", i, "\n")
  
  TAPI_sim$abond_std <- sample(TAPI$abond_std, replace = FALSE)
  
  lm_TAPI_rand <- lm(log(abond_std) ~ Annee, data = TAPI_sim)
  
  TAPI_output[i,1] <- coef(lm_TAPI_rand)[1] # Beta intercepte (abond_std)
  TAPI_output[i,2] <- coef(lm_TAPI_rand)[2] # Beta année
  
#}

# save(TAPI_output, file = "TAPI_output.R")
load("TAPI_output.R")

hist(TAPI_output[,1])
hist(TAPI_output[,2])

TAPI_beta_annee <- sum(TAPI_output[, 2] >= coef(lm_TAPI)[2])/n_sim

exp(TAPI_annee) # 1,05 oiseau de plus/heure/année 
exp(TAPI_int) 

TAPI_pred <- predict.lm(lm_TAPI)

# Représentation graphique

TAPI_log <- ggplot(TAPI, aes(x = Annee, y = log(abond_std), group = 1))+
  geom_point(size = 3, col = "forestgreen")+
  geom_line()+
  geom_abline(intercept = coef(lm_TAPI)[1], slope = coef(lm_TAPI)[2], color = "red")+
  labs(title = "Abondance du Tarin des pins par heure d'observation",
       x = "Année",
       y = expression("N. individus recensés *" ~ heure^{-1}))+
  theme_classic()
TAPI_log <- TAPI_log+
  annotate(geom ="text",x = 5, y = 5, label ="y = 1.11 + 0.05x")
TAPI_log


##### GROUPÉ


# Combine les 4 graphiques en 2x2
plot_grid(DUSA_log,
          JABO_log,
          SIFL_log,
          TAPI_log, ncol = 2)

tab_comp <- data.frame(c(DUSA_int, JABO_int, TAPI_int, SIFL_int),
                       c(DUSA_annee, JABO_annee, TAPI_annee, SIFL_annee),
                       c(DUSA_lowerIC_annee, JABO_lowerIC_annee, SIFL_lowerIC_annee, TAPI_lowerIC_annee),
                       c(DUSA_upperIC_annee, JABO_upperIC_annee, SIFL_upperIC_annee, TAPI_upperIC_annee))


rownames(tab_comp) <- c("DUSA", "JABO", "SIFL", "TAPI")
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
segments(x0 = 1, y0 = DUSA_lowerIC_annee,
         x1 = 1, y1 = DUSA_upperIC_annee)
segments(x0 = 2, y0 = JABO_lowerIC_annee,
         x1 = 2, y1 = JABO_upperIC_annee)
segments(x0 = 3, y0 = TAPI_lowerIC_annee,
         x1 = 3, y1 = TAPI_upperIC_annee)
segments(x0 = 4, y0 = SIFL_lowerIC_annee,
         x1 = 4, y1 = SIFL_upperIC_annee)



# Tendance temporelle condition ------------------------------------------------------

# Tendance de la proportion ne se font qu'à partir de 2007 car NA avant...
# Est-ce qu'on souhaite quand même évaluer la condition et la prop. dans le temps ou seulement carac. irr






















