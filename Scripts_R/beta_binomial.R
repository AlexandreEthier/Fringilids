### Analyse avec beta binomial


# Librarie ----------------------------------------------------------------

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
library(jagsUI)


# Nettoyage du jeu de données ----------------------------------------------------------


df_final <- abond_irruption %>%
  select(
    Annee,
    Espece,
    Effort        = Effort.x,
    abond         = abond.x,
    abond_std     = abond_std.x,
    irruption,
    nb_HY         = nb_HY.x,
    nb_AHY        = nb_AHY.x,
    nb_total      = nb_total.x,
    prop_jeunes   = prop_jeunes.x,
    Condition_moyenne = Condition_moyenne.x,
    Condition_sd  = Condition_sd.x,
    Condition_se  = Condition_se.x
  )


str(df_final)
#write_xlsx(df_final, "df_final.xlsx")

# Lire nouveau jeu de données ---------------------------------------------


df_clean <- read_excel("/Users/maxencepoirier-joanette/Rstudio/FOR7046/Fringilids/Scripts_R/df_final.xlsx")

df_clean <- read_excel("C:/Users/alexe/Fringilids/Scripts_R/df_final.xlsx")
df_clean<-read_excel("df_final.xlsx")
#df_clean %>%
#  group_by(Espece) %>%
#  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Corrélation -------------------------------------------------------------

# On veut voir la corrélation et voir si on peut ajouter les deux termes
df_clean %>%
  group_by(Espece) %>%
  summarise(cor = cor(abond_std, as.numeric(irruption)))

# On retire les NA

df_clean <- df_clean[!is.na(df_clean$nb_total), ]

# Vérifier
nrow(df_clean)
sum(is.na(df_clean$nb_total))  

str(df_clean)

df_clean$irruption <- as.factor(as.numeric(df_clean$irruption))
df_clean$irruption<- as.numeric(df_clean$irruption)

# Modèle ------------------------------------------------------------------

# Pourquoi faire une beta binomiale

# 1) Nous avons des valeurs binomiales (TRUE or FALSE pour les irruptions)
# 2) Gros déséquilibre pour nos proportions

# Notre formule serait la suivante: Proportion ~ Irruption

# Dans une beta binomiale voici ce qu'il faut comprendre

# 1) Le beta considère la vairabilité écologique
# 2)  La binomial distribution accounts for sampling variability (number of successes out of trials n)

# JAGS --------------------------------------------------------------------


# GLMM  -------------------------------------------------------------------
install.packages("glmmTMB")
library(glmmTMB)

fit <- glmmTMB(prop_jeunes ~ irruption + Annee,
               family   = betabinomial(link = "logit"),
               weights = nb_total,
               data     = df_clean)
summary(fit)


#### DUSA ####


#df_clean$irruption <- ifelse(df_clean$irruption=="1", 0,1)
df_clean$irruption1 <- ifelse(df_clean$irruption=="2", 1, 0)


df_clean_DUSA <- df_clean %>% 
  filter(Espece == "DUSA")

str(df_clean_DUSA)
#df_clean_DUSA$irruption1<- as.numeric(as.factor(df_clean_DUSA$irruption1))
levels(df_clean_DUSA$irruption1)
df_clean_DUSA$Annee<-as.integer(as.factor(df_clean_DUSA$Annee))

# approche fréquentiste
fit_DUSA <- glmmTMB(prop_jeunes ~ irruption + (1 | Annee),
               family   = betabinomial(link = "logit"),
               weights = nb_total,
               data     = df_clean_DUSA)

summary(fit_DUSA)

library(DHARMa) 
install.packages("DHARMa")
simOut <- simulateResiduals(fit_DUSA, n = 500)
##quantile-quantile plot of  observed residuals vs residuals from simulations
plot(simOut) 
plotQQunif(simOut) #QQ plot based on uniform distribution
plotResiduals(simOut)

# approche bayésienne
model_jeune_DUSA <- "
 model{
 
 ## prior pour alpha
 for (i in 1: n.annee){
 alpha.annee[i] ~ dnorm(mu.annee, tau.annee)
 }
 
 mu.annee ~dnorm (0,0.001)
 sigma.annee ~dunif(0,100)
 tau.annee <- 1/(sigma.annee*sigma.annee)
 
 ##priors for betas on probability
 # beta0 ~ dnorm(0, 0.1) #reduce variance for betas because dnorm(0, 0.01) too wide
 beta.irru ~ dnorm(0, 0.1)
 
 sigma ~ dunif(0,100)
 tau <- 1/(sigma*sigma)
 
 ##priors for alpha and beta parameters of beta distribution
 theta ~ dunif(0.0, 20) #move away from 0
 
 ##likelihood
 for(i in 1:nobs) {
 
logit(mu[i]) <- max(-10, min(10, alpha.annee[Annee[i]] + beta.irru * irruption1[i]))

    ##prob drawn from beta distribution
    prob[i] ~ dbeta(mu[i] * theta, (1 - mu[i]) * theta) T(0.001, 0.999) #truncate to avoid 0 and 1
    
    ##observed data drawn from binomial
    PropJeunes[i] ~ dbin(prob[i], nb_total[i])
    ##########################################	
 
 }
 
 mean.int <- mean(alpha.annee[])
 mean.prob <- mean(prob[])
 }
"
writeLines(model_jeune_DUSA, con= "model_jeune_DUSA.txt")

n.annee <- length(unique(df_clean_DUSA$Annee))
jagsData <- list(
  irruption1 = df_clean_DUSA$irruption1,
  nobs       = nobs,
  n.annee    = n.annee,
  Annee      = df_clean_DUSA$Annee,
  nb_total   = as.integer(df_clean_DUSA$nb_total),
  PropJeunes = as.integer(round(df_clean_DUSA$prop_jeunes * df_clean_DUSA$nb_total))
)

str(jagsData)
str(df_clean_DUSA)
# valeurs initiales
initsFun <- function(){
  list(beta.irru = rnorm(1),
       alpha.annee = rnorm(n.annee),
       theta= runif(1, 0.01,10))
}

params<- c("beta.irru","mean.int","mean.prob", "alpha.annee", "sigma.annee", "sigma", "mu", "theta", "prob")

out.test<- jags(data = jagsData, 
                inits = initsFun, 
                parameters.to.save = params, 
                n.chains = 5, 
                n.iter = 50000,
                n.burnin = 10000, 
                n.thin = 5, 
                model= "model_jeune_DUSA.txt")
summary(out.test)
out.sum<- out.test$summary[c("mean.int", "beta.irru","sigma.annee", "theta", "mean.prob"), c("mean", "sd", "2.5%", "97.5%", "Rhat")]
out.sum #

# diagnostics
any(out.test$summary[, "Rhat"]> 1.1)

par(mfrow = c(4,1), mar= c(4,4,2,2))
matplot(cbind(out.test$samples[[1]][, "mean.int"],
              out.test$samples[[2]][, "mean.int"],
              out.test$samples[[3]][, "mean.int"],
              out.test$samples[[4]][, "mean.int"],
              out.test$samples[[5]][, "mean.int"]),
        type = "l",
        ylab = "mean.int", cex.lab = 1.2)
matplot(cbind(out.test$samples[[1]][, "beta.irru"],
              out.test$samples[[2]][, "beta.irru"],
              out.test$samples[[3]][, "beta.irru"],
              out.test$samples[[4]][, "beta.irru"],
              out.test$samples[[5]][, "beta.irru"]),
        type = "l",
        ylab = "beta irruption", cex.lab = 1.2)
matplot(cbind(out.test$samples[[1]][, "sigma.annee"],
              out.test$samples[[2]][, "sigma.annee"],
              out.test$samples[[3]][, "sigma.annee"],
              out.test$samples[[4]][, "sigma.annee"],
              out.test$samples[[5]][, "sigma.annee"]
),
        type = "l",
        ylab = "beta sigma année", cex.lab = 1.2)
matplot(cbind(out.test$samples[[1]][, "theta"],
              out.test$samples[[2]][, "theta"],
              out.test$samples[[3]][, "theta"],               
              out.test$samples[[4]][, "theta"],
              out.test$samples[[5]][, "theta"]),
        type = "l",
        ylab = "theta", cex.lab = 1.2)

# longueur des chaines 
library(coda)
outMC<-out.test$samples[1:5]
coda.out<-summary(outMC)
range(coda.out$statistics[, "Time-series SE"]/
        coda.out$statistics[, "SD"]) # inférieur à 0.05 OK!

# autocorrélation
par(mfrow=c(2,3), mar=c(4,4,2,2))
autocorr.plot(outMC[[1]][, "mean.int"],
              auto.layout =FALSE, main= "mean.int - autocorr.plot")
autocorr.plot(outMC[[1]][, "beta.irru"],
              auto.layout =FALSE, main= "beta.irru - autocorr.plot")
autocorr.plot(outMC[[1]][, "theta"],
              auto.layout =FALSE, main= "theta - autocorr.plot")
autocorr.plot(outMC[[1]][, "sigma"],
              auto.layout =FALSE, main= "sigma - autocorr.plot")
autocorr.plot(outMC[[1]][, "sigma.annee"],
              auto.layout =FALSE, main= "sigma.annee - autocorr.plot")


# postérieur
par(mfrow=c(2,3), mar=c(4,4,2,2))
plot(density(out.test$sims.list$mean.int),
     xlab = "mean.int",
     main = "Postérieur de mean.int")
plot(density(out.test$sims.list$beta.irru),
     xlab = "Irruption",
     main = "Postérieur de beta irruption")
plot(density(out.test$sims.list$sigma),
     xlab = "sigma",
     main = "Postérieur de sigma")
plot(density(out.test$sims.list$sigma.annee ),
     xlab = "sigma année",
     main = "Postérieur de sigma année") # parait être différent de 0
plot(density(out.test$sims.list$theta),
     xlab = "theta",
     main = "Postérieur de Theta")














