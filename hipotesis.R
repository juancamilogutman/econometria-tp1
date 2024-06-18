rm(list=ls()) #Limpiamos la memoria

library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(sandwich)        # Robust Covariance Matrix Estimators
library(marginaleffects) # Método Delta implementación
library(quantreg)        # Regresión por cuantiles
library(msm)             # Test simil testnl de STATA
library(car)

options(tinytable_tt_digits = 3)
options(tinytable_theme_placement_latex_float = "H")

eph1 <- readRDS("Bases/eph_1abc.RDS")

a_reg1 <- lm(logSal ~ educn + edad + I(edad^2) + est_civ + region, data = eph1)


# A MANOPLA ####
# Extraer los coeficientes
beta_hat <- coef(a_reg1)

# Extraer la matriz de varianza-covarianza de los coeficientes
vcov_beta_hat <- vcov(a_reg1)

# Definir la matriz de hipótesis para "2 * beta[I(edad^2)] * 50 + beta[edad] = 0"
# Asumiendo que 'edad' es el tercer coeficiente y 'I(edad^2)' es el cuarto coeficiente
A <- matrix(c(0, 0, 1, 100, rep(0, length(beta_hat) - 4)), ncol = length(beta_hat))

# Calcular el estadístico de prueba Wald
W <- t(A %*% beta_hat) %*% solve(A %*% vcov_beta_hat %*% t(A)) %*% (A %*% beta_hat)

# El estadístico de prueba Wald sigue una distribución chi-cuadrado
# Obtener el valor p
p_value <- pchisq(W, df = 1, lower.tail = FALSE)

# Imprimir el estadístico de prueba y el valor p
print(W)
print(p_value)

# Crear un dataframe con el estadístico de prueba y el valor p
results_df <- data.frame(Estadistico_De_Prueba = W, pValor = p_value)

# Convertir el dataframe a una tabla tinytable
results_table <- tt(results_df)

# Mostrar la tabla
print(results_table)

# CON PAQUETE CAR ####
resultado_prueba <- linearHypothesis(a_reg1, "100 * I(edad^2) + edad = 0")

anova(a_reg1)

# CON PAQUETE MARGINAL EFFECTS ####
hypotheses(a_reg1, hypothesis = "(-1*edad)/(2*I(edad^2)) - 50 = 0") %>% tt()

hypotheses(a_reg1, hypothesis = "100 * I(edad^2) + edad = 0") %>% tt()
