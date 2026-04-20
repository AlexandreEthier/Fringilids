# Chargements des bibliothèques -------------------------------------------

library(readxl)
library(ggplot2)
library(DHARMa)
library(glmmTMB)
library(jagsUI)
library(coda)
library(dplyr)
library(performance)


# Chargement des répertoires de travail -----------------------------------

setwd("C:/Users/alexe/Fringilids/Data") # Alex


# Chargement données ------------------------------------------------------

DUSA <- read.csv("DUSA.csv", header = TRUE)
JABO <- read.csv("JABO.csv", header = TRUE)
SIFL <- read.csv("SIFL.csv", header = TRUE)
TAPI <- read.csv("TAPI.csv", header = TRUE)

DUSA <- DUSA[!is.na(DUSA$nb_tot),] # Retrait des années >= 2007 (NA)
JABO <- JABO[!is.na(JABO$nb_tot),]
SIFL <- SIFL[!is.na(SIFL$nb_tot),]
TAPI <- TAPI[!is.na(TAPI$nb_tot),]

DUSA$Annee <- as.integer(as.factor(DUSA$Annee))
JABO$Annee <- as.integer(as.factor(JABO$Annee))
SIFL$Annee <- as.integer(as.factor(SIFL$Annee))
TAPI$Annee <- as.integer(as.factor(TAPI$Annee))

DUSA$irruption <- as.numeric(DUSA$irruption)
JABO$irruption <- as.numeric(JABO$irruption)
SIFL$irruption <- as.numeric(SIFL$irruption)
TAPI$irruption <- as.numeric(TAPI$irruption)

abond <- read.csv("abond_clean.csv", header = TRUE)
bague <- read.csv("bague_clean.csv", header = TRUE)


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


##### DUSA

# Approche fréquentiste proportion HY

# Variable réponse (Succès | Échec)
DUSA_propHY <- glmmTMB(cbind(nb_HY, nb_AHY) ~ irruption + (1 | Annee), 
                       family = betabinomial(link = "logit"),
                       data = DUSA)

check_overdispersion(DUSA_propHY) # pas de surdispersion
summary(DUSA_propHY)

# Vérification des conditions d'applications

DUSA_outsim <- simulateResiduals(DUSA_propHY, n = 1000)
plot(DUSA_outsim)
plotQQunif(DUSA_outsim)
plotResiduals(DUSA_outsim)


# Approche bayésienne proportion HY 

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

#DUSA_out_jags<- jags(data = DUSA_jagsData, 
 #               inits = initsFun, 
  #              parameters.to.save = params, 
   #             n.chains = 5, 
    #            n.iter = 50000,
     #           n.burnin = 10000, 
      #          n.thin = 10, 
       #         model= "mod_HY_DUSA.txt")

# save(DUSA_out_jags, file = "DUSA_out_jags.txt")
load("DUSA_out_jags.txt")

DUSA_summary <- DUSA_out_jags$summary[c("mean.int", "beta.irru","sigma.annee", "theta", "mean.prob"), c("mean", "sd", "2.5%", "97.5%", "Rhat")]
DUSA_summary

# La valeur élevé de theta représente une faible surdispersion

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

# Approche fréquentiste proportion HY

# Variable réponse (Succès | Échec)
JABO_propHY <- glmmTMB(cbind(nb_HY, nb_AHY) ~ irruption + (1 | Annee), 
                       family = betabinomial(link = "logit"),
                       data = JABO)
check_overdispersion(JABO_propHY)

summary(JABO_propHY)

# Vérification des conditions d'applications

JABO_outsim <- simulateResiduals(JABO_propHY, n = 1000)
plot(JABO_outsim)
plotQQunif(JABO_outsim)
plotResiduals(JABO_outsim)


# Approche bayésienne proportion HY 

mod_HY_JABO <- "
 
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
writeLines(mod_HY_JABO, con = "mod_HY_JABO.txt")

# Paramètres

n.annee <- length(unique(JABO$Annee))
nobs <- nrow(JABO)

JABO_jagsData <- list(
  irruption = JABO$irruption,
  nobs       = nobs,
  n.annee    = n.annee,
  Annee      = JABO$Annee,
  nb_tot   = as.integer(JABO$nb_tot),
  PropJeunes = as.integer(round(JABO$prop_HY * JABO$nb_tot))
)

# Données initiales

initsFun <- function(){
  list(beta.irru = rnorm(1),
       alpha.annee = rnorm(n.annee),
       theta = runif(1, 0.01, 10))
}

params <- c("beta.irru","mean.int","mean.prob", "alpha.annee", "sigma.annee", "sigma", "mu", "theta", "prob")

#JABO_out_jags<- jags(data = JABO_jagsData, 
 #              inits = initsFun, 
  #            parameters.to.save = params, 
   #          n.chains = 5, 
    #        n.iter = 50000,
     #      n.burnin = 10000, 
      #    n.thin = 10, 
       #  model= "mod_HY_JABO.txt")

# save(JABO_out_jags, file = "JABO_out_jags.txt")
load("JABO_out_jags.txt")

JABO_summary <- JABO_out_jags$summary[c("mean.int", "beta.irru","sigma.annee", "theta", "mean.prob"), c("mean", "sd", "2.5%", "97.5%", "Rhat")]
JABO_summary # la proportion de jeunes est expliquée par les irruptions

# La valeur élevé de theta représente une faible surdispersion

# Diagnostic

any(JABO_out_jags$summary[, "Rhat"] > 1.1) # FALSE

# Diagramme de convergence

par(mfrow = c(4,1), mar= c(4,4,2,2))
matplot(cbind(JABO_out_jags$samples[[1]][, "mean.int"],
              JABO_out_jags$samples[[2]][, "mean.int"],
              JABO_out_jags$samples[[3]][, "mean.int"],
              JABO_out_jags$samples[[4]][, "mean.int"],
              JABO_out_jags$samples[[5]][, "mean.int"]),
        type = "l",
        ylab = "mean.int", cex.lab = 1.2)
matplot(cbind(JABO_out_jags$samples[[1]][, "beta.irru"],
              JABO_out_jags$samples[[2]][, "beta.irru"],
              JABO_out_jags$samples[[3]][, "beta.irru"],
              JABO_out_jags$samples[[4]][, "beta.irru"],
              JABO_out_jags$samples[[5]][, "beta.irru"]),
        type = "l",
        ylab = "beta irruption", cex.lab = 1.2)
matplot(cbind(JABO_out_jags$samples[[1]][, "sigma.annee"],
              JABO_out_jags$samples[[2]][, "sigma.annee"],
              JABO_out_jags$samples[[3]][, "sigma.annee"],
              JABO_out_jags$samples[[4]][, "sigma.annee"],
              JABO_out_jags$samples[[5]][, "sigma.annee"]
),
type = "l",
ylab = "beta sigma année", cex.lab = 1.2)
matplot(cbind(JABO_out_jags$samples[[1]][, "theta"],
              JABO_out_jags$samples[[2]][, "theta"],
              JABO_out_jags$samples[[3]][, "theta"],               
              JABO_out_jags$samples[[4]][, "theta"],
              JABO_out_jags$samples[[5]][, "theta"]),
        type = "l",
        ylab = "theta", cex.lab = 1.2)

JABO_outMC <- JABO_out_jags$samples[1:5]
JABO_coda_out<-summary(JABO_outMC)
range(JABO_coda_out$statistics[, "Time-series SE"]/
        JABO_coda_out$statistics[, "SD"]) # inférieur à 0.05 OK!

# Diagrammes d'autocorrélation

par(mfrow=c(2,3), mar=c(4,4,2,2))
autocorr.plot(JABO_outMC[[1]][, "mean.int"],
              auto.layout = FALSE, main= "mean.int - autocorr.plot")
autocorr.plot(JABO_outMC[[1]][, "beta.irru"],
              auto.layout = FALSE, main= "beta.irru - autocorr.plot")
autocorr.plot(JABO_outMC[[1]][, "theta"],
              auto.layout = FALSE, main = "theta - autocorr.plot")
autocorr.plot(JABO_outMC[[1]][, "sigma"],
              auto.layout = FALSE, main = "sigma - autocorr.plot")
autocorr.plot(JABO_outMC[[1]][, "sigma.annee"],
              auto.layout = FALSE, main= "sigma.annee - autocorr.plot")

# Distributions postérieures

par(mfrow=c(2,3), mar=c(4,4,2,2))
plot(density(JABO_out_jags$sims.list$mean.int),
     xlab = "mean.int",
     main = "Postérieur de mean.int")
plot(density(JABO_out_jags$sims.list$beta.irru),
     xlab = "Irruption",
     main = "Postérieur de beta irruption")
plot(density(JABO_out_jags$sims.list$sigma),
     xlab = "sigma",
     main = "Postérieur de sigma")
plot(density(JABO_out_jags$sims.list$sigma.annee ),
     xlab = "sigma année",
     main = "Postérieur de sigma année") # parait être différent de 0
plot(density(JABO_out_jags$sims.list$theta),
     xlab = "theta",
     main = "Postérieur de Theta")


##### SIFL

# Approche fréquentiste proportion HY

# Variable réponse (Succès | Échec)
SIFL_propHY <- glmmTMB(cbind(nb_HY, nb_AHY) ~ irruption + ( 1 | Annee), 
                       family = binomial(link = "logit"),
                       data = SIFL)

check_overdispersion(SIFL_propHY, alternative = "two.sided")
summary(SIFL_propHY)

# Vérification des conditions d'applications

SIFL_outsim <- simulateResiduals(SIFL_propHY, n = 1000)
plot(SIFL_outsim)
plotQQunif(SIFL_outsim)
plotResiduals(SIFL_outsim)

# Matrice hessienne non-positive

# Approche bayésienne proportion HY 

mod_HY_SIFL <- "
 
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
writeLines(mod_HY_SIFL, con = "mod_HY_SIFL.txt")

# Paramètres

n.annee <- length(unique(SIFL$Annee))
nobs <- nrow(SIFL)

SIFL_jagsData <- list(
  irruption = SIFL$irruption,
  nobs       = nobs,
  n.annee    = n.annee,
  Annee      = SIFL$Annee,
  nb_tot   = as.integer(SIFL$nb_tot),
  PropJeunes = as.integer(round(SIFL$prop_HY * SIFL$nb_tot))
)

# Données initiales

initsFun <- function(){
  list(beta.irru = rnorm(1),
       alpha.annee = rnorm(n.annee),
       theta = runif(1, 0.01, 10))
}

params <- c("beta.irru","mean.int","mean.prob", "alpha.annee", "sigma.annee", "sigma", "mu", "theta", "prob")

#SIFL_out_jags<- jags(data = SIFL_jagsData, 
  #            inits = initsFun, 
   #         parameters.to.save = params, 
    #      n.chains = 5, 
     #   n.iter = 50000,
      #    n.burnin = 10000, 
       #   n.thin = 10, 
        #    model= "mod_HY_SIFL.txt")

# save(SIFL_out_jags, file = "SIFL_out_jags.txt")
load("SIFL_out_jags.txt")

SIFL_summary <- SIFL_out_jags$summary[c("mean.int", "beta.irru","sigma.annee", "theta", "mean.prob"), c("mean", "sd", "2.5%", "97.5%", "Rhat")]
SIFL_summary # pas d'effet des irruptions sur la prop de jeunes


# Diagnostic

any(SIFL_out_jags$summary[, "Rhat"] > 1.1) # FALSE

# Diagramme de convergence

par(mfrow = c(4,1), mar= c(4,4,2,2))
matplot(cbind(SIFL_out_jags$samples[[1]][, "mean.int"],
              SIFL_out_jags$samples[[2]][, "mean.int"],
              SIFL_out_jags$samples[[3]][, "mean.int"],
              SIFL_out_jags$samples[[4]][, "mean.int"],
              SIFL_out_jags$samples[[5]][, "mean.int"]),
        type = "l",
        ylab = "mean.int", cex.lab = 1.2)
matplot(cbind(SIFL_out_jags$samples[[1]][, "beta.irru"],
              SIFL_out_jags$samples[[2]][, "beta.irru"],
              SIFL_out_jags$samples[[3]][, "beta.irru"],
              SIFL_out_jags$samples[[4]][, "beta.irru"],
              SIFL_out_jags$samples[[5]][, "beta.irru"]),
        type = "l",
        ylab = "beta irruption", cex.lab = 1.2)
matplot(cbind(SIFL_out_jags$samples[[1]][, "sigma.annee"],
              SIFL_out_jags$samples[[2]][, "sigma.annee"],
              SIFL_out_jags$samples[[3]][, "sigma.annee"],
              SIFL_out_jags$samples[[4]][, "sigma.annee"],
              SIFL_out_jags$samples[[5]][, "sigma.annee"]
),
type = "l",
ylab = "beta sigma année", cex.lab = 1.2)
matplot(cbind(SIFL_out_jags$samples[[1]][, "theta"],
              SIFL_out_jags$samples[[2]][, "theta"],
              SIFL_out_jags$samples[[3]][, "theta"],               
              SIFL_out_jags$samples[[4]][, "theta"],
              SIFL_out_jags$samples[[5]][, "theta"]),
        type = "l",
        ylab = "theta", cex.lab = 1.2)

SIFL_outMC <- SIFL_out_jags$samples[1:5]
SIFL_coda_out<-summary(SIFL_outMC)
range(SIFL_coda_out$statistics[, "Time-series SE"]/
        SIFL_coda_out$statistics[, "SD"]) # inférieur à 0.05 OK!

# Diagrammes d'autocorrélation

par(mfrow=c(2,3), mar=c(4,4,2,2))
autocorr.plot(SIFL_outMC[[1]][, "mean.int"],
              auto.layout = FALSE, main= "mean.int - autocorr.plot")
autocorr.plot(SIFL_outMC[[1]][, "beta.irru"],
              auto.layout = FALSE, main= "beta.irru - autocorr.plot")
autocorr.plot(SIFL_outMC[[1]][, "theta"],
              auto.layout = FALSE, main = "theta - autocorr.plot")
autocorr.plot(SIFL_outMC[[1]][, "sigma"],
              auto.layout = FALSE, main = "sigma - autocorr.plot")
autocorr.plot(SIFL_outMC[[1]][, "sigma.annee"],
              auto.layout = FALSE, main= "sigma.annee - autocorr.plot")

# Distributions postérieures

par(mfrow=c(2,3), mar=c(4,4,2,2))
plot(density(SIFL_out_jags$sims.list$mean.int),
     xlab = "mean.int",
     main = "Postérieur de mean.int")
plot(density(SIFL_out_jags$sims.list$beta.irru),
     xlab = "Irruption",
     main = "Postérieur de beta irruption")
plot(density(SIFL_out_jags$sims.list$sigma),
     xlab = "sigma",
     main = "Postérieur de sigma")
plot(density(SIFL_out_jags$sims.list$sigma.annee ),
     xlab = "sigma année",
     main = "Postérieur de sigma année") # parait être différent de 0
plot(density(SIFL_out_jags$sims.list$theta),
     xlab = "theta",
     main = "Postérieur de Theta")


##### TAPI

# Approche fréquentiste proportion HY

# Variable réponse (Succès | Échec)
TAPI_propHY <- glmmTMB(cbind(nb_HY, nb_AHY) ~ irruption + (1 | Annee), 
                       family = betabinomial(link = "logit"),
                       data = TAPI)

check_overdispersion(TAPI_propHY)
summary(TAPI_propHY)

# Vérification des conditions d'applications

TAPI_outsim <- simulateResiduals(TAPI_propHY, n = 1000)
plot(TAPI_outsim)
plotQQunif(TAPI_outsim)
plotResiduals(TAPI_outsim)

# Approche bayésienne proportion HY 

mod_HY_TAPI <- "
 
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
writeLines(mod_HY_TAPI, con = "mod_HY_TAPI.txt")

# Paramètres

n.annee <- length(unique(TAPI$Annee))
nobs <- nrow(TAPI)

TAPI_jagsData <- list(
  irruption = TAPI$irruption,
  nobs       = nobs,
  n.annee    = n.annee,
  Annee      = TAPI$Annee,
  nb_tot   = as.integer(TAPI$nb_tot),
  PropJeunes = as.integer(round(TAPI$prop_HY * TAPI$nb_tot))
)

# Données initiales

initsFun <- function(){
  list(beta.irru = rnorm(1),
       alpha.annee = rnorm(n.annee),
       theta = runif(1, 0.01, 10))
}

params <- c("beta.irru","mean.int","mean.prob", "alpha.annee", "sigma.annee", "sigma", "mu", "theta", "prob")

#TAPI_out_jags<- jags(data = TAPI_jagsData, 
 #          inits = initsFun, 
  #         parameters.to.save = params, 
   #        n.chains = 5, 
    #       n.iter = 50000,
     #      n.burnin = 10000, 
      #     n.thin = 10, 
       #    model= "mod_HY_TAPI.txt")

# save(TAPI_out_jags, file = "TAPI_out_jags.txt")
load("TAPI_out_jags.txt")

TAPI_summary <- TAPI_out_jags$summary[c("mean.int", "beta.irru","sigma.annee", "theta", "mean.prob"), c("mean", "sd", "2.5%", "97.5%", "Rhat")]
TAPI_summary # pas d'effet de l'irruption


# Diagnostic

any(TAPI_out_jags$summary[, "Rhat"] > 1.1) # FALSE

# Diagramme de convergence

par(mfrow = c(4,1), mar= c(4,4,2,2))
matplot(cbind(TAPI_out_jags$samples[[1]][, "mean.int"],
              TAPI_out_jags$samples[[2]][, "mean.int"],
              TAPI_out_jags$samples[[3]][, "mean.int"],
              TAPI_out_jags$samples[[4]][, "mean.int"],
              TAPI_out_jags$samples[[5]][, "mean.int"]),
        type = "l",
        ylab = "mean.int", cex.lab = 1.2)
matplot(cbind(TAPI_out_jags$samples[[1]][, "beta.irru"],
              TAPI_out_jags$samples[[2]][, "beta.irru"],
              TAPI_out_jags$samples[[3]][, "beta.irru"],
              TAPI_out_jags$samples[[4]][, "beta.irru"],
              TAPI_out_jags$samples[[5]][, "beta.irru"]),
        type = "l",
        ylab = "beta irruption", cex.lab = 1.2)
matplot(cbind(TAPI_out_jags$samples[[1]][, "sigma.annee"],
              TAPI_out_jags$samples[[2]][, "sigma.annee"],
              TAPI_out_jags$samples[[3]][, "sigma.annee"],
              TAPI_out_jags$samples[[4]][, "sigma.annee"],
              TAPI_out_jags$samples[[5]][, "sigma.annee"]
),
type = "l",
ylab = "beta sigma année", cex.lab = 1.2)
matplot(cbind(TAPI_out_jags$samples[[1]][, "theta"],
              TAPI_out_jags$samples[[2]][, "theta"],
              TAPI_out_jags$samples[[3]][, "theta"],               
              TAPI_out_jags$samples[[4]][, "theta"],
              TAPI_out_jags$samples[[5]][, "theta"]),
        type = "l",
        ylab = "theta", cex.lab = 1.2)

TAPI_outMC <- TAPI_out_jags$samples[1:5]
TAPI_coda_out<-summary(TAPI_outMC)
range(TAPI_coda_out$statistics[, "Time-series SE"]/
        TAPI_coda_out$statistics[, "SD"]) # inférieur à 0.05 OK!

# Diagrammes d'autocorrélation

par(mfrow=c(2,3), mar=c(4,4,2,2))
autocorr.plot(TAPI_outMC[[1]][, "mean.int"],
              auto.layout = FALSE, main= "mean.int - autocorr.plot")
autocorr.plot(TAPI_outMC[[1]][, "beta.irru"],
              auto.layout = FALSE, main= "beta.irru - autocorr.plot")
autocorr.plot(TAPI_outMC[[1]][, "theta"],
              auto.layout = FALSE, main = "theta - autocorr.plot")
autocorr.plot(TAPI_outMC[[1]][, "sigma"],
              auto.layout = FALSE, main = "sigma - autocorr.plot")
autocorr.plot(TAPI_outMC[[1]][, "sigma.annee"],
              auto.layout = FALSE, main= "sigma.annee - autocorr.plot")

# Distributions postérieures

par(mfrow=c(2,3), mar=c(4,4,2,2))
plot(density(TAPI_out_jags$sims.list$mean.int),
     xlab = "mean.int",
     main = "Postérieur de mean.int")
plot(density(TAPI_out_jags$sims.list$beta.irru),
     xlab = "Irruption",
     main = "Postérieur de beta irruption")
plot(density(TAPI_out_jags$sims.list$sigma),
     xlab = "sigma",
     main = "Postérieur de sigma")
plot(density(TAPI_out_jags$sims.list$sigma.annee ),
     xlab = "sigma année",
     main = "Postérieur de sigma année") # parait être différent de 0
plot(density(TAPI_out_jags$sims.list$theta),
     xlab = "theta",
     main = "Postérieur de Theta")



# Condition ---------------------------------------------------------------

load("DUSA_bague.csv")
load("JABO_bague.csv")
load("SIFL_bague.csv")
load("TAPI_bague.csv")

par(mfrow= c(2,2))

boxplot(DUSA_bague$condition[DUSA_bague$DUSA_IRR=="0"],
        DUSA_bague$condition[DUSA_bague$DUSA_IRR=="1"], 
        col = c("grey","orange"), 
        ylab= "Condition", 
        main= "Condition du durbec des sapins selon les années irruptives", 
        names = c("Non irruption", "Irruption"))

boxplot(JABO_bague$condition[JABO_bague$JABO_IRR=="0"],
        JABO_bague$condition[JABO_bague$JABO_IRR=="1"], 
        col = c("grey","forestgreen"), 
        ylab= "Condition", 
        main= "Condition du jaseur boréal selon les années irruptives", 
        names = c("Non irruption", "Irruption"))

boxplot(SIFL_bague$condition[SIFL_bague$SIFL_IRR=="0"],
        SIFL_bague$condition[SIFL_bague$SIFL_IRR=="1"],
        col = c("grey","turquoise3"), 
        ylab= "Condition", 
        main= "Condition du sizerin flammé selon les années irruptives", 
        names = c("Non irruption", "Irruption"))


boxplot(TAPI_bague$condition[TAPI_bague$TAPI_IRR=="0"],
        TAPI_bague$condition[TAPI_bague$TAPI_IRR=="1"],
        col = c("grey","violetred"), 
        ylab= "Condition", 
        main= "Condition du tarin des pins selon les années irruptives", 
        names = c("Non irruption", "Irruption"))


shapiro.test(DUSA_bague$condition[DUSA_bague$DUSA_IRR == "0"])
shapiro.test(DUSA_bague$condition[DUSA_bague$DUSA_IRR == "1"])
wilcox.test(condition ~ DUSA_IRR, data = DUSA_bague)
DUSA_cond_irr<-tapply(DUSA_bague$condition, DUSA_bague$DUSA_IRR, summary)
DUSA_cond_irr

shapiro.test(JABO_bague$condition[JABO_bague$JABO_IRR == "0"])
shapiro.test(JABO_bague$condition[JABO_bague$JABO_IRR == "1"])
wilcox.test(condition ~ JABO_IRR, data = JABO_bague)
JABO_cond_irr<-tapply(JABO_bague$condition, JABO_bague$JABO_IRR, summary)
JABO_cond_irr

shapiro.test(SIFL_bague$condition[SIFL_bague$SIFL_IRR == "0"])
shapiro.test(SIFL_bague$condition[SIFL_bague$SIFL_IRR == "1"])
wilcox.test(condition ~ SIFL_IRR, data = SIFL_bague)
SIFL_cond_irr<-tapply(SIFL_bague$condition, SIFL_bague$SIFL_IRR, summary)
SIFL_cond_irr

shapiro.test(TAPI_bague$condition[TAPI_bague$TAPI_IRR == "0"])
shapiro.test(TAPI_bague$condition[TAPI_bague$TAPI_IRR == "1"])
wilcox.test(condition ~ TAPI_IRR, data = TAPI_bague)
TAPI_cond_irr<-tapply(TAPI_bague$condition, TAPI_bague$TAPI_IRR, summary)
TAPI_cond_irr


DUSA_tableau_cond <- do.call(rbind, DUSA_cond_irr)
JABO_tableau_cond <- do.call(rbind, JABO_cond_irr)
SIFL_tableau_cond <- do.call(rbind, SIFL_cond_irr)
TAPI_tableau_cond <- do.call(rbind, TAPI_cond_irr)

tableau_cond <- rbind(DUSA_tableau_cond, JABO_tableau_cond, SIFL_tableau_cond, TAPI_tableau_cond)
colnames(tableau_cond)<- c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max", "NA")
rownames(tableau_cond)<- c("DUSA non irruption", "DUSA irruption", 
                           "JABO non irruption", "JABO irruption", 
                           "SIFL non irruption", "SIFL irruption", 
                           "TAPI non irruption", "TAPI irruption")
tableau_cond <- tableau_cond[,c(1,3,4)]
tableau_cond_irr<- tableau_cond[c(2,4,6,8), ]
tableau_cond_nirr <- tableau_cond[c(1,3,5,7),]
tableau <- cbind(tableau_cond_nirr[,1], tableau_cond_irr[,1],
                 tableau_cond_nirr[,2], tableau_cond_irr[,2],
                 tableau_cond_nirr[,3], tableau_cond_irr[,3])
colnames(tableau) <- c("1st Qu non irruption", "1st Qu irruption", 
                       "mean non irruption", "mean irruption", 
                       "3rd Qu non irruption", "3rd Qu irruption")
rownames(tableau)<- c("DUSA", "JABO", "SIFL", "TAPI")
tableau
