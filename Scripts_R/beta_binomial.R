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


# Jeu de données ----------------------------------------------------------


str(abond_irruption)

#Nettoyage

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


# Jeu de données final

str(df_final)
write_xlsx(df_final, "df_final.xlsx")


# Corrélation -------------------------------------------------------------

# On veut voir la corrélation et voir si on peut ajouter les deux termes
df_clean %>%
  group_by(Espece) %>%
  summarise(cor = cor(abond_std, as.numeric(irruption)))



# Modèle ------------------------------------------------------------------

# Pourquoi faire une beta binomiale

# 1) Nous avons des valeurs binomiales (TRUE or FALSE pour les irruptions)
# 2) Gros déséquilibre pour nos proportions

# Notre formule serait la suivante: Proportion ~ Irruption


# JAGS --------------------------------------------------------------------

model_string <- "
model{

# Intercept par espèce 
  for (s in 1:n_espece) {
    alpha[s] ~ dnorm(0, 0.001)  
  } 
  
##priors for betas on probability
beta_irruption ~ dnorm(0, 0.001) 
 
##priors for alpha and beta parameters of beta distribution
kappa ~ dgamma(1, 0.1)
 
##likelihood
for(i in 1:nobs) {
 
##likelihood specified as compound beta and binomial (slicer stuck at values at infinite density - due to low values of alpha and beta)
    ##linear predictor
    logit(mu[i]) <- alpha[espece_id[i]] + beta_irruption * irruption[i]
    
    # Reparamétrisation Beta via mu et kappa
    a[i] <- mu[i] * kappa
    b[i] <- (1 - mu[i]) * kappa
    
    # Prior hiéarchique sur la proprotion anuelle
     p[i] ~ dbeta(a[i], b[i])
     
     # Vraisemblance binomiale
         nb_HY[i] ~ dbin(p[i], nb_total[i])

  }
 
    ##prob drawn from beta distribution
    ##prob[i] ~ dbeta(mu[i] * theta, (1 - mu[i]) * theta) #error when some values are at boundary
    prob[i] ~ dbeta(mu[i] * theta, (1 - mu[i]) * theta) T(0.001, 0.999) #truncate to avoid 0 and 1
##    
##    ##observed data drawn from binomial
##    NumberGerminated[i] ~ dbin(prob[i], Total[i])
##    ##########################################	
## 
## }
## 
## ##derived parameters
## #alpha <- theta - beta
## #beta <- theta - alpha
## 
## }", 
## 
## , fill = TRUE)
## 
## sink( )
## 
## germ$Props <- germ$NumberGerminated/germ$Total
## germ$CanopyOpenness.std <- (germ$CanopyOpenness - mean(germ$CanopyOpenness))/sd(germ$CanopyOpenness)
## 
## ##assemble data in list
## jagsData <- list(NumberGerminated = germ$NumberGerminated,
##                  Total = germ$Total,
##                  CanopyOpenness = germ$CanopyOpenness.std,
##                  nobs = nrow(germ))
## 
## ##initial values
## initsFun <- function( ) {
##     list(beta0 = rnorm(1),
##          beta.canopy = rnorm(1), #
##          theta = runif(1, 0.01, 10))  #important to find good initial values       
##     }
##     
## ##parameters to save
## params <- c("beta0", "beta.canopy", "mu", 
##             "prob", "theta")
## 
## ##use JAGS
## library(jagsUI)
## 
## out.bb <- jags(data = jagsData,
##                inits = initsFun,
##                parameters.to.save = params,
##                model.file = "betabin.jags",
##                parallel = TRUE,
##                n.chains = 5,
##                n.adapt = 10000, #000,
##                n.iter = 250000, #75000,#000,
##                n.burnin = 200000, #50000,#000,
##                n.thin = 10)#0)
## save(out.bb, file = "beta-binom.Rdata")
## load("beta-binom.Rdata")
## 
## hist(out.bb$summary[, "Rhat"])
## any(out.bb$summary[, "Rhat"] > 1.1, na.rm = TRUE)
## max(out.bb$summary[, "Rhat"], na.rm = TRUE)                   
## rownames(out.bb$summary)[which(out.bb$summary[, "Rhat"] > 1.1)]
## 
## library(coda)
## combo <- as.mcmc.list(out.bb$samples)
## sumOut.bb <- summary(combo)
## save(sumOut.bb, file = "sumOut.bb.Rdata")
## 
## pdf(file = "traceBetaBin.pdf")
## par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
## traceplot(combo[, c("beta0", "beta.canopy", "theta")])
## dev.off( )
## 
## pdf(file = "postBetaBin.pdf")
## par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
## densplot(combo[, c("beta0", "beta.canopy", "theta")])
## dev.off( )
## 
## ##check ratio of MCMC/naïve SE
## ratio <- sumOut.bb$statistics[, "Time-series SE"]/sumOut.bb$statistics[, "SD"]
## hist(ratio)
## max(ratio, na.rm = TRUE)
## which.max(ratio)
## 
## 
## ##check output for each species
## results.bb <- out.bb$summary[c("beta0", "beta.canopy", "theta"),
##                              c("mean", "sd", "2.5%", "97.5%")]
## results.bb
## save(results.bb, file = "results.bb.Rdata")