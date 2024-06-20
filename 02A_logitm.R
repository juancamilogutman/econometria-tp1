#PUNTO 2A
library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(sandwich)        # Robust Covariance Matrix Estimators
library(quantreg)        # Regresión por cuantiles
library(nnet)
library(broom)
library(stargazer)

eph1 <- readRDS("Bases/eph_2ab.RDS")

logit_m <- multinom(formalidad ~ educf + edadi + region + est_civ, data = eph1)

# Get a summary of the model
model_summary <- summary(logit_m)

coef <- as.data.frame(model_summary$coefficients)
model_summary$standard.errors

coef

stargazer(logit_m, type = "text")
