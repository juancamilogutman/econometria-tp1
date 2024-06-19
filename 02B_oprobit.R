# rm(list=ls()) #Limpiamos la memoria

library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(MASS)

eph <- readRDS("Bases/eph_2ab.RDS")

oprobit <- polr(formalidad ~ educf + edadi + region, data = eph, method = "probit")
summary(oprobit)