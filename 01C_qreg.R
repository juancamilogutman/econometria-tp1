#PUNTO 1C
#rm(list=ls()) #Limpiamos la memoria

library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(sandwich)        # Robust Covariance Matrix Estimators
library(quantreg)        # Regresión por cuantiles
library(xtable)
library(broom)

options(tinytable_tt_digits = 3)
options(tinytable_theme_placement_latex_float = "H")

eph1 <- readRDS("Bases/eph_1abc.RDS")

# Ajustamos al mismo modelo del punto 1A una regresión por deciles.
qreg <- rq(logSal ~ educf + edadi +  est_civ + region, tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), data = eph1)

# Mostrar resumen del modelo
# rdo_qreg <- summary(qreg, se = "boot")

tidy_qreg <- tidy(qreg)
View(tidy_qreg)
