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

# Revoir les valeurs de dnorm et dgammamodel_jeu
# glm(prop_jeunes ~ Irruption, family = binomial, weigths = nb_total, data= df_clean)
model_jeune <- "
 model{
 
 ##priors for betas on probability
 beta0 ~ dnorm(0, 0.1) #reduce variance for betas because dnorm(0, 0.01) too wide
 beta.irru ~ dnorm(0, 0.1)
 
 ##priors for alpha and beta parameters of beta distribution
 theta ~ dunif(0.0, 20) #move away from 0
 
 ##likelihood
 for(i in 1:nobs) {
 
    ##########################################
    ##likelihood specified as compound beta and binomial (slicer stuck at values at infinite density - due to low values of alpha and beta)
    ##linear predictor
    logit(mu[i]) <- beta0 + beta.irru * Irruption[i] 
 
 
    ##prob drawn from beta distribution
    prob[i] ~ dbeta(mu[i] * theta, (1 - mu[i]) * theta) T(0.001, 0.999) #truncate to avoid 0 and 1
    
    ##observed data drawn from binomial
    NombreJeunes[i] ~ dbin(prob[i], nb_total[i])
    ##########################################	
 
 }
 
 ##derived parameters
 #alpha <- theta - beta
 #beta <- theta - alpha
 }
"
writeLines(model_jeune, con= "model_jeune.txt")

jagsData<- list(Prop_Jeunes= df_clean$prop_jeunes, 
                nb_total = df_clean$nb_total,
                Irruption = df_clean$irruption,
                nobs = nrow(df_clean))

# valeurs initiales
initsFun <- function(){
  list(beta0 = rnorm(1),
       beta.irru = rnorm(1), 
       theta= runif(1, 0.01,10))
}

params<- c("beta0", "beta.irru","mu", "theta", "prob", "PropJeunes")

out.test<- jags(data = jagsData, 
                inits = initsFun, 
                parameters.to.save = params, 
                n.chains = 5, 
                n.iter = 10000,
                n.burnin = 2000, 
                n.thin = 5, 
                model= "model_jeune.txt")
summary(out.test)
out.sum<- out.test$summary[c("beta0", "beta.irru", "theta"), c("mean", "sd", "2.5%", "97.5%", "Rhat")]
out.sum # pas d'effet de l'irruption mais on n'a pas fait les diagnostics
## 

?rbinom

plot(density(rbinom(50, 100, 0.5)))



model_strings <- "

model{

# Intercept par espèce 
  for (s in 1:n_espece) {
    alpha[s] ~ dnorm(0, 0.001)  
  } 
  
# prior pour beta

  beta.irruption ~ dnorm(0, 0.001) 
 
# priors for alpha and beta parameters of beta distribution
  
  theta ~ dgamma(1, 0.1) # ça peut être unif, gamma (à décider). C'est le paramètre de précision
 
#likelihood

  for(i in 1:N) {
# linear predictor (portion binomial du modèle)
    lmu[i] <- alpha[espece_id[i]] + beta.irruption * irruption[i]
    mu[i] <- exp(lmu[i])/(1+exp(lmu[i])) 
    
    # Reparamétrisation Beta via mu et kappa
    a[i] <- mu[i] * kappa
    b[i] <- (1 - mu[i]) * kappa
    
    # Pour dire que si le chiffre est élevé, plus la confiance est bonne envers la valeur 
     p[i] ~ dbeta(a[i], b[i]) 
     
     # Portion observation du modèle
     nb_HY[i] ~ dbin(p[i], nb_total[i])

}
  
  for (s in 1:n_espece) {

    # Proportion prédite par espèce
    lmu.non.irr[s] <- alpha[s] + beta.irruption * 0
    lmu.irr[s]     <- alpha[s] + beta.irruption * 1

    mu.non.irr[s] <- exp(lmu.non.irr[s]) / (1 + exp(lmu.non.irr[s]))
    mu.irr[s]     <- exp(lmu.irr[s])     / (1 + exp(lmu.irr[s]))

    # Différence irruption vs non-irruption par espèce
    delta[s] <- mu.irr[s] - mu.non.irr[s]
  }

  # Effet global moyen sur toutes les espèces
  delta.global <- mean(delta[])

}
"

writeLines(model_string, con = "beta_binom.txt")


# Préparation du jeu de données -------------------------------------------

# Convertir espece en index numérique
df_clean$espece_id <- as.integer(as.factor(df_clean$Espece))

# Vérifier la correspondance espèce <-> index
unique(df_clean[, c("Espece", "espece_id")])

# Bundle data
N         <- nrow(df_clean)
n_espece  <- length(unique(df_clean$espece_id))

jags_data <- list(
  N          = N,
  n_espece   = n_espece,
  nb_HY      = df_clean$nb_HY,
  nb_total   = df_clean$nb_total,
  irruption  = as.integer(df_clean$irruption),  # FALSE=0, TRUE=1
  espece_id  = df_clean$espece_id
)

# Valeurs initiales
inits <- function() {
  list(
    alpha          = rnorm(n_espece, 0, 1),
    beta.irruption = rnorm(1),
    kappa          = rgamma(1, 1, 0.1)
  )
}

# Paramètres à surveiller
params <- c("alpha", "beta.irruption", "kappa",
            "mu.irr", "mu.non.irr", "delta", "delta.global")

# MCMC settings
nc <- 3       # chains
ni <- 10000   # iterations
nb <- 2000   # burn-in
nt <- 5       # thinning

out.beta <- jags(
  data       = jags_data,
  inits      = inits,
  parameters = params,
  model      = "beta_binom.txt",
  n.thin     = nt,
  n.chains   = nc,
  n.burnin   = nb,
  n.iter     = ni,
  n.adapt    = 1000
)

outSum<- out.beta$summary[, c("mean", "sd", 
                                  "2.5%", "97.5%", "Rhat")]
outSum

# Layout
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))


# Diagnostique ------------------------------------------------------------


for (s in 1:4) {
  matplot(cbind(out.beta$samples[[1]][, paste0("alpha[", s, "]")],
                out.beta$samples[[2]][, paste0("alpha[", s, "]")],
                out.beta$samples[[3]][, paste0("alpha[", s, "]")]),
          type = "l",
          ylab = paste0("alpha[", s, "]"),
          main = paste0("Tracé alpha[", s, "] - Espèce ", s))
}

matplot(cbind(out.beta$samples[[1]][, "beta.irruption"],
              out.beta$samples[[2]][, "beta.irruption"],
              out.beta$samples[[3]][, "beta.irruption"]),
        type = "l",
        ylab = expression(beta[irruption]),
        main = "Tracé beta.irruption")

matplot(cbind(out.beta$samples[[1]][, "kappa"],
              out.beta$samples[[2]][, "kappa"],
              out.beta$samples[[3]][, "kappa"]),
        type = "l",
        ylab = expression(kappa),
        main = "Tracé kappa")

matplot(cbind(out.beta$samples[[1]][, "delta.global"],
              out.beta$samples[[2]][, "delta.global"],
              out.beta$samples[[3]][, "delta.global"]),
        type = "l",
        ylab = expression(delta[global]),
        main = "Tracé delta.global")


# Résultat ----------------------------------------------------------------

# -------------------------------------------------------
# GRAPHIQUE PRINCIPAL — delta par espèce avec IC 95%
# -------------------------------------------------------
par(mfrow = c(1, 1), mar = c(5, 6, 4, 2))

# Noms de tes espèces dans le bon ordre
espece_noms <- levels(as.factor(df_clean$Espece))  # ordre alphab.

# Extraire mean et IC 95% pour delta[1:4]
delta_mean <- out.beta$mean$delta
delta_low  <- out.beta$q2.5$delta
delta_high <- out.beta$q97.5$delta

# Plot
plot(1:4, delta_mean,
     xlim = c(0.5, 4.5),
     ylim = c(min(delta_low) - 0.02, max(delta_high) + 0.02),
     pch  = 19,
     xaxt = "n",
     xlab = "Espèce",
     ylab = expression(delta ~ "(irruption - non irruption)"),
     main = "Effet de l'irruption sur la proportion de juvéniles")

# Axe x avec noms des espèces
axis(1, at = 1:4, labels = espece_noms)

# Barres d'erreur IC 95%
arrows(x0  = 1:4,
       y0  = delta_low,
       y1  = delta_high,
       angle  = 90,
       code   = 3,
       length = 0.05)

# Ligne à zéro
abline(h = 0, col = "red", lty = 2)

# delta global
abline(h = out.beta$mean$delta.global, col = "blue", lty = 2)
legend("topright",
       legend = c("Zéro", "Delta global"),
       col    = c("red", "blue"),
       lty    = 2)


# GLMM  -------------------------------------------------------------------
install.packages("glmmTMB", type = "source", dependencies = TRUE)
library(glmmTMB)

fit <- glmmTMB(cbind(nb_HY, nb_AHY) ~ irruption + Espece,
               family   = betabinomial(link = "logit"),
               data     = df_clean)
summary(fit)

df_clean_DUSA <- df_clean %>% 
  filter(Espece == "DUSA")

fit_DUSA <- glmmTMB(cbind(nb_HY, nb_AHY) ~ irruption,
               family   = betabinomial(link = "logit"),
               data     = df_clean_DUSA)

summary(fit_DUSA)

library(DHARMa) 
install.packages("DHARMa")
simOut <- simulateResiduals(fit_DUSA, n = 500)
##quantile-quantile plot of  observed residuals vs residuals from simulations
plot(simOut) 
plotQQunif(simOut) #QQ plot based on uniform distribution
plotResiduals(simOut)
