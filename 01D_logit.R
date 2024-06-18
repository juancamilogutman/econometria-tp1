rm(list=ls()) # borramo objetos

library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(sandwich)        # Robust Covariance Matrix Estimators
library(quantreg)        # Regresión por cuantiles
library(broom)           # Permite
library(plotly)          # Para gráficos mejores
library(mfx)             # Para evaluar el efecto marginal en las medias
library(margins)         # Puerto/adaptación del comando margins de Stata
library(stargazer)       # Otras tablas de resumen de modelos

eph2 <- readRDS("Bases/eph_1de.RDS") 

#Estimamos un modelo de probabilidad lineal
mpl <- lm(estado ~ educf + edad + I(edad^2) + region + est_civ, eph2)
resumen <- modelsummary(mpl)
resumen

eph2$proba_mpl <- predict(mpl, newdata = eph2, type = "response")

# Proporción de la muestra con probabilidad estimada negativa o mayor a 1:
negativas  <- sum(eph2$proba_mpl < 0)
mayores_que_1 <- sum(eph2$proba_mpl > 1)
no_probs <- negativas + mayores_que_1
filas <- sum(!is.na(eph2$proba_mpl))
fuera_rango <- no_probs/filas
# Esta última es la proporción que pide la consigna, con probabilidades
# predichas mayores a 1 o menores a 0:

# Queríamos ver la proporción de ocupados vs. desocupados
ocupados <- sum(eph2$estado == 0)
desocupados <- sum(eph2$estado == 1)
ocupados
desocupados
# ESTIMAR DICHA PROPORCIÓN Y MENCIONAR QUE SEMEJANTE DISPARIDAD 
# AFECTA LA ESTIMACIÓN.

# Estimamos un probit y un logit:
probit_eph <- glm(estado ~ educf + edad + I(edad^2) + region + est_civ, family = binomial(link = "probit"), 
                data = eph2)
logit_eph <- glm(estado ~ educf + edad + I(edad^2) + region + est_civ, family = binomial(link = "logit"), 
               data = eph2)

rdo_probit <- tidy(probit_eph)
rdo_logit <- tidy(logit_eph)

# A la hora de interpretar esto hay que tener mucho cuidado, porque lo único que se puede interpretar de los coeficientes
# directamente es su signo, gracias a que las funciones "link" o "de enlace" con el modelo de variable latente tienen densidad positiva.

eph2$proba_probit <- predict(probit_eph, newdata = eph2, type = "response")
eph2$proba_logit <- predict(logit_eph, newdata = eph2, type = "response")


# Graficar los efectos marginales de la edad sobre la probabilidad
# de estar desempleado, junto con los errores estándar de la estimación.

cc <- coef(logit_eph)
cons_logit <- cc["(Intercept)"]
edad_logit <- cc["edad"]
edad2_logit <- cc["I(edad^2)"]

dfz <- data.frame(
  Edad = 25:65,
  z = rep(0, 65 - 25 + 1),
  Proba_Desocup = rep(0, 65 - 25 + 1)
)

#Acá un código de LaTeX lindo, de la z como la derivada parcial, etc.
dfz <- dfz %>% mutate(z = cons_logit + edad_logit * dfz$Edad+ edad2_logit * (dfz$Edad)^2)

# P(z) es la probabilidad de ocurrencia de un evento dadas las log-odds z (REVISAR ESTO, PORQUE "LOG"?) ####
logistica <- function(z) {
  exp(z) / (1 + exp(z))
}

dfz <- dfz %>% mutate(Proba_Desocup = logistica(dfz$z))

#GRÁFICO CON GGPLOT
ggplot(dfz, aes(x = Edad, y = Proba_Desocup)) +
  geom_line() +  
  geom_point() +  
  labs(x = "Edad", y = "Probabilidad de Desocupación", 
       title = "Probabilidad de Desocupación por Edad") +
  theme_minimal() 

# Graficar los efectos marginales de la edad sobre la probabilidad
# de estar desempleado, junto con los errores estándar de la estimación.


# Efectos marginales en la media
efectos_en_media <- logitmfx(estado ~ educf + edad + I(edad^2) + region + est_civ,
                             data = eph2,
                             atmean = TRUE,  #Hay que ver qué hace esto
                             robust = FALSE
                             ) 
dff <- as.data.frame(efectos_en_media$mfxest)

alpha <- 0.05
dff$CI_Inferior <- dff$"dF/dx" - qnorm(1 - alpha/2) * dff$"Std. Err."
dff$CI_Superior <- dff$"dF/dx" + qnorm(1 - alpha/2) * dff$"Std. Err."


names(efectos_en_media)
stargazer(efectos_en_media["mfxest"], type = 'text')
# Media de los efectos marginales:
# esto es, sacamos todos los efectos marginales
# media_de_efecto <- summary(margins(logit_eph))
# media_de_efecto
# margins(logit_eph)
# Esto no funciona, quizá hay que dropear un level del est_civ 
# o incluso Sin intrucción (pero creo q esto último no)

library(ggalt)
efectos_educ <- dff[1:6,]
efectos_educ