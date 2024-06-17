library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(sandwich)        # Robust Covariance Matrix Estimators
library(quantreg)        # Regresión por cuantiles
library(xtable)
library(broom)

rm(list=ls()) # borro objetos

eph2 <- readRDS("Bases/eph_1de.RDS") 

# Cargo los paquetes necesarios:
#install.packages("mfx") 
#library(devtools)
#install_github("leeper/prediction")
#install_github("leeper/margins")

# library(mfx) # para evaluar el efecto marginal en las medias
# library(margins) # para evaluar el efecto marginal en un valor cualquiera
# library(ggplot2) # grÃ¡ficos, dentro de tidyverse
# library(stargazer)

mpl <- lm(estado ~ educf + edad + I(edad^2) + region + est_civ, eph2)

resumen <- modelsummary(mpl)
resumen

eph2$proba_mpl <- predict(mpl, newdata = eph2, type = "response")

negs  <- sum(eph2$proba_mpl < 0)
mays1 <- sum(eph2$proba_mpl > 1)
no_probs <- negs + mays1
filas <- sum(!is.na(eph2$proba_mpl))
fuera_rango <- no_probs/filas


ocups <- sum(eph2$estado == 0)
desoc <- sum(eph2$estado == 1)
ocups
desoc

probit <- glm(estado ~ educf + edad + I(edad^2) + region + est_civ, family = binomial(link = "probit"), 
                data = eph2)
logit <- glm(estado ~ educf + edad + I(edad^2) + region + est_civ, family = binomial(link = "logit"), 
               data = eph2)

rdo_probit <- tidy(probit)
rdo_logit <- tidy(logit)

eph2$proba_probit <- predict(probit, newdata = eph2, type = "response")
eph2$proba_logit <- predict(logit, newdata = eph2, type = "response")

cc <- coef(logit)
cons_logit <- cc["(Intercept)"]
edad_logit <- cc["edad"]
edad2_logit <- cc["I(edad^2)"]
# vec_coef <- c(cc["educfSecundario completo"], cc[est_civCasado], 5)
# hombrecasado <- 
# print(vec_coef)

dfz <- data.frame(
  Edad = 25:65,
  z = rep(0, 65 - 25 + 1),
  Proba_Desocup = rep(0, 65 - 25 + 1)
)

dfz <- dfz %>% mutate(z = cons_logit + edad_logit * dfz$Edad+ edad2_logit * (dfz$Edad)^2)

dfz <- dfz %>% mutate(Proba_Desocup = exp(dfz$z) / (1 + exp(dfz$z))^2)

