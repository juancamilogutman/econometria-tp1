#PUNTO 1E
eph1 <- readRDS("Bases/eph_1e.RDS")

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
library(sampleSelection) # Para modelo de Heckman

eph2 <- readRDS("Bases/eph_1e.RDS") 

eph2$estado <- as.factor(eph2$estado)

#Este va probablemente con el segundo step sujeto a problemas de multicolinealidad
heckman_1 <- heckit(
  selection = estado ~ educf + edadi + est_civ + region,
  outcome = logSal ~ educf + edadi + est_civ + region,
  data = eph2
)

#Entonces vamos con este:
heckman_2 <- heckit(
  selection = estado ~ educf + edadi + est_civ + region,
  outcome = logSal ~ educf + edadi + region,
  data = eph2
)

summary(heckman_1)
summary(heckman_2)