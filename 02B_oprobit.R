# rm(list=ls()) #Limpiamos la memoria

library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(MASS)
library(margins)
library(mfx)

eph <- readRDS("Bases/eph_2ab.RDS")

oprobit <- polr(formalidad ~ educf, data = eph, method = "probit")

marginal_effects <- margins(oprobit)

print(summary(marginal_effects))