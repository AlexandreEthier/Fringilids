# Chargements des bibliothèques -------------------------------------------

library(readxl)
library(ggplot2)
library(DHARMa)
library(glmmTMB)
library(jagsUI)
library(coda)


# Chargement des répertoires de travail -----------------------------------

setwd("C:/Users/alexe/Fringilids/Data") # Alex


# Chargement données ------------------------------------------------------

DUSA <- read.csv("DUSA.csv", header = TRUE)
JABO <- read.csv("JABO.csv", header = TRUE)
SIFL <- read.csv("SIFL.csv", header = TRUE)
TAPI <- read.csv("TAPI.csv", header = TRUE)

DUSA$Annee <- as.integer(as.factor(DUSA$Annee))

DUSA$irruption <- as.numeric(DUSA$irruption)
DUSA$irruption <- ifelse(DUSA$irruption == "2", 1, 0)

JABO$irruption <- as.numeric(JABO$irruption)
SIFL$irruption <- as.numeric(SIFL$irruption)
TAPI$irruption <- as.numeric(TAPI$irruption)


##### DUSA

DUSA <- DUSA[!is.na(DUSA$nb_tot),] # Retrait des années >= 2007 (NA)
JABO <- JABO[!is.na(JABO$nb_tot),]
SIFL <- SIFL[!is.na(SIFL$nb_tot),]
TAPI <- TAPI[!is.na(TAPI$nb_tot),]




# Approche fréquentiste proportion HY ---------------------------------------------------

# Variable réponse (Succès | Éched)
DUSA_propHY <- glmmTMB(cbind(nb_HY, nb_AHY) ~ irruption + (1 | Annee), 
                       family = betabinomial(link = "logit"),
                       data = DUSA)

summary(DUSA_propHY)

# Vérification des conditions d'applications

DUSA_outsim <- simulateResiduals(DUSA_propHY, n = 1000)
plot(DUSA_outsim)
plotQQunif(DUSA_outsim)
plotResiduals(DUSA_outsim)


# Approche bayésienne proportion HY -----------------------------------------------------

mod_HY_DUSA <- "
 
 model{
 
# prior alpha
 
 for (i in 1: n.annee){
 
 alpha.annee[i] ~ dnorm(mu.annee, tau.annee)
 
}
 
 mu.annee ~ dnorm (0,0.001)
 sigma.annee ~ dunif(0,100)
 tau.annee <- 1/(sigma.annee * sigma.annee)
 
# prior probabilité beta

 beta.irru ~ dnorm(0, 0.1)
 
 sigma ~ dunif(0,100)
 tau <- 1/(sigma * sigma)
 
# theta

 theta ~ dunif(0.0, 20)
 
# likelihood

 for(i in 1:nobs) {
 
  logit(mu[i]) <- max(-10, min(10, alpha.annee[Annee[i]] + beta.irru * irruption[i]))

# Probabilité tiré d'une distribution beta binomiale

    prob[i] ~ dbeta(mu[i] * theta, (1 - mu[i]) * theta) T(0.001, 0.999)
    
# Data observé tiré de la distribution

    PropJeunes[i] ~ dbin(prob[i], nb_tot[i])
 
}
 
 mean.int <- mean(alpha.annee[])
 mean.prob <- mean(prob[])
 }
"
writeLines(mod_HY_DUSA, con = "mod_HY_DUSA.txt")

# Paramètres

n.annee <- length(unique(DUSA$Annee))
nobs <- nrow(DUSA)

DUSA_jagsData <- list(
  irruption = DUSA$irruption,
  nobs       = nobs,
  n.annee    = n.annee,
  Annee      = DUSA$Annee,
  nb_tot   = as.integer(DUSA$nb_tot),
  PropJeunes = as.integer(round(DUSA$prop_HY * DUSA$nb_tot))
)

# Données initiales

initsFun <- function(){
  list(beta.irru = rnorm(1),
       alpha.annee = rnorm(n.annee),
       theta = runif(1, 0.01, 10))
}

params <- c("beta.irru","mean.int","mean.prob", "alpha.annee", "sigma.annee", "sigma", "mu", "theta", "prob")

DUSA_out_jags<- jags(data = DUSA_jagsData, 
                inits = initsFun, 
                parameters.to.save = params, 
                n.chains = 5, 
                n.iter = 50000,
                n.burnin = 10000, 
                n.thin = 10, 
                model= "mod_HY_DUSA.txt")

# save(DUSA_out_jags, file = "DUSA_out_jags.txt")
load("DUSA_out_jags.txt")

summary(DUSA_out_jags)
DUSA_summary <- DUSA_out_jags$summary[c("mean.int", "beta.irru","sigma.annee", "theta", "mean.prob"), c("mean", "sd", "2.5%", "97.5%", "Rhat")]
DUSA_summary

# Diagnostic

any(DUSA_out_jags$summary[, "Rhat"] > 1.1) # FALSE

# Diagramme de convergence

par(mfrow = c(4,1), mar= c(4,4,2,2))
matplot(cbind(DUSA_out_jags$samples[[1]][, "mean.int"],
              DUSA_out_jags$samples[[2]][, "mean.int"],
              DUSA_out_jags$samples[[3]][, "mean.int"],
              DUSA_out_jags$samples[[4]][, "mean.int"],
              DUSA_out_jags$samples[[5]][, "mean.int"]),
        type = "l",
        ylab = "mean.int", cex.lab = 1.2)
matplot(cbind(DUSA_out_jags$samples[[1]][, "beta.irru"],
              DUSA_out_jags$samples[[2]][, "beta.irru"],
              DUSA_out_jags$samples[[3]][, "beta.irru"],
              DUSA_out_jags$samples[[4]][, "beta.irru"],
              DUSA_out_jags$samples[[5]][, "beta.irru"]),
        type = "l",
        ylab = "beta irruption", cex.lab = 1.2)
matplot(cbind(DUSA_out_jags$samples[[1]][, "sigma.annee"],
              DUSA_out_jags$samples[[2]][, "sigma.annee"],
              DUSA_out_jags$samples[[3]][, "sigma.annee"],
              DUSA_out_jags$samples[[4]][, "sigma.annee"],
              DUSA_out_jags$samples[[5]][, "sigma.annee"]
),
type = "l",
ylab = "beta sigma année", cex.lab = 1.2)
matplot(cbind(DUSA_out_jags$samples[[1]][, "theta"],
              DUSA_out_jags$samples[[2]][, "theta"],
              DUSA_out_jags$samples[[3]][, "theta"],               
              DUSA_out_jags$samples[[4]][, "theta"],
              DUSA_out_jags$samples[[5]][, "theta"]),
        type = "l",
        ylab = "theta", cex.lab = 1.2)

DUSA_outMC <- DUSA_out_jags$samples[1:5]
DUSA_coda_out<-summary(DUSA_outMC)
range(DUSA_coda_out$statistics[, "Time-series SE"]/
        DUSA_coda_out$statistics[, "SD"]) # inférieur à 0.05 OK!

# Diagrammes d'autocorrélation

par(mfrow=c(2,3), mar=c(4,4,2,2))
autocorr.plot(DUSA_outMC[[1]][, "mean.int"],
              auto.layout = FALSE, main= "mean.int - autocorr.plot")
autocorr.plot(DUSA_outMC[[1]][, "beta.irru"],
              auto.layout = FALSE, main= "beta.irru - autocorr.plot")
autocorr.plot(DUSA_outMC[[1]][, "theta"],
              auto.layout = FALSE, main = "theta - autocorr.plot")
autocorr.plot(DUSA_outMC[[1]][, "sigma"],
              auto.layout = FALSE, main = "sigma - autocorr.plot")
autocorr.plot(DUSA_outMC[[1]][, "sigma.annee"],
              auto.layout = FALSE, main= "sigma.annee - autocorr.plot")

# Distributions postérieures

par(mfrow=c(2,3), mar=c(4,4,2,2))
plot(density(DUSA_out_jags$sims.list$mean.int),
     xlab = "mean.int",
     main = "Postérieur de mean.int")
plot(density(DUSA_out_jags$sims.list$beta.irru),
     xlab = "Irruption",
     main = "Postérieur de beta irruption")
plot(density(DUSA_out_jags$sims.list$sigma),
     xlab = "sigma",
     main = "Postérieur de sigma")
plot(density(DUSA_out_jags$sims.list$sigma.annee ),
     xlab = "sigma année",
     main = "Postérieur de sigma année") # parait être différent de 0
plot(density(DUSA_out_jags$sims.list$theta),
     xlab = "theta",
     main = "Postérieur de Theta")


# L'effet de l'irruption non-significatif sur la proportion de jeunes mais plus d'irruptions expliquerait p-t un effet.
# Meilleur subsampling des populations nécessaire aussi

##### JABO



##### SIFL



##### TAPI




# Approche fréquentiste condition HY --------------------------------------


abond <- read.csv("abond_clean.csv", header = TRUE)
bague <- read.csv("bague_clean.csv", header = TRUE)


load("DUSA_irr.R")

DUSA_IRR <- as.data.frame(DUSA_irr[12:length(DUSA_irr)])
DUSA_IRR$Annee <- seq(1:length(DUSA_IRR$`DUSA_irr[12:length(DUSA_irr)]`))
DUSA_IRR$Espece <- "DUSA"
DUSA_IRR$abond_std <- DUSA$abond_std
colnames(DUSA_IRR) <- c("irruption", "Annee", "Espece", "abond_std")

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

DUSA_bague <- bague_irr[bague_irr$Espece == "DUSA",]
DUSA_bague <- DUSA_bague[, 1:8]

DUSA_bague$DUSA_IRR <- ifelse(DUSA_bague$DUSA_IRR == "TRUE", 1, 0)


DUSA_bague$condition <- DUSA_bague$Aile/DUSA_bague$Masse

# save(DUSA_bague, file = "DUSA_bague.csv")

boxplot(condition ~ abond_std, data = DUSA_bague)

boxplot(Masse ~ irruption.x, data = DUSA_bague)

boxplot(Masse ~ abond_std, data = DUSA_bague)

mod_mass <- lm(Masse ~ abond_std, data = DUSA_bague)
summary(mod_mass)


xtabs(~ abond_std, data = DUSA_bague)


boxplot(Aile ~ abond_std, data = DUSA_bague)



DUSA_lm <- lm(condition ~ irruption, data = DUSA_bague)
plot(DUSA_lm)
summary(DUSA_lm)

DUSA_lme <- lme(condition ~irruption + (1 | Annee), data = DUSA_bague)
plot(DUSA_lme)
summary(DUSA_lme)

DUSA_anova <- anova(DUSA_lm, DUSA_lme)
par(mfrow= c(2,2))
plot(DUSA_anova)


lm(condition ~ abond_std, data)
plot(df_clean_DUSA$abond_std, df_clean_DUSA$Condition_moyenne)
dev.off()









boxplot(condition ~ DUSA_IRR, data = DUSA_bague, na.rm = TRUE)



t.test(condition ~ DUSA_IRR, data = DUSA_bague)











JABO_bague <- bague_irr[bague_irr$Espece == "JABO",]
JABO_bague <- bague_irr


SIFL_bague <- bague_irr[bague_irr$Espece == "SIFL",]





TAPI_bague <- bague_irr[bague$Espece == "TAPI",]







