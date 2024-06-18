#PUNTO 1B
rm(list=ls()) #Limpiamos la memoria

library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(sandwich)        # Robust Covariance Matrix Estimators
library(quantreg)        # Regresión por cuantiles

eph1 <- readRDS("Bases/eph_1abc.RDS")

reg_b <- lm(logSal ~ educn + edad + I(edad^2) + est_civ + region, data = eph1)
b_coef_estim <- coef(reg_b)
b_edad_max_salario <- (-1*b_coef_estim ["edad"])/(2*b_coef_estim["I(edad^2)"])
print (b_edad_max_salario)


#Buscamos la varianza según el método delta
##Definimos a la función g de los parametros de interés como (-1*b_coef_estim ["edad"])/(2*b_coef_estim["I(edad^2)"])
## _b[edad2] * 2 * edad + _b[edad] = 0
## edad = -(_b[edad]) / 2 _b[edad2]

b_dg_coef_edad <-(-1)/(2*2*b_coef_estim["I(edad^2)"])
print(b_dg_coef_edad)

b_dg_coef_edad2 <-(b_coef_estim ["edad"])/(2*((b_coef_estim["I(edad^2)"])^2))
print(b_dg_coef_edad2)

b_gradiente <- c(b_dg_coef_edad, b_dg_coef_edad2)
print(b_gradiente)

b_vcov_mat <- vcov(reg_b)
#view(b_vcov_mat)

b_vcov_mat_edad <- b_vcov_mat [3:4,3:4]
#view(b_vcov_mat_edad)

# Aplicar el método delta para obtener Varianza de g(beta1, beta2) aproximada por el método delta
b_var_delta <- t(b_gradiente) %*% b_vcov_mat_edad %*% b_gradiente
print(b_var_delta)

#Armamos el intervalo de confianza utilizando la distribución normal estándar

b_alpha1 <- 0.1  # Nivel de significancia: 90% de confianza
b_alpha2 <- 0.05 # Nivel de significancia: 95% de confianza
b_alpha3 <- 0.01  # Nivel de significancia: 99% de confianza

# Estimación de g(theta) {NO}
#g_hat <- g(theta_hat) {NO}

b_alpha = b_alpha3
b_error_std <- qnorm(1 - b_alpha/ 2) * sqrt(b_var_delta)
b_int_conf <- c(b_edad_max_salario - b_error_std, b_edad_max_salario + b_error_std)
b_estad_z_crit <- qnorm(1 - b_alpha/ 2)
print(b_int_conf)

#Insertar gráfico

# Define los límites del gráfico
plot(c(-1, 6), c(b_edad_max_salario - 3 * sqrt(b_var_delta), b_edad_max_salario + 3 * sqrt(b_var_delta)),
     type = "n", xlab = "Valor crítico (Z)", ylab = "Edad estimada asociada al salario máximo",
     main = "Intervalo de confianza utilizando método delta")

# Línea vertical en el valor crítico de la distribución normal estándar
abline(v = b_estad_z_crit, col = "red", lty = 2)

# Intervalo de confianza
segments(b_estad_z_crit, b_int_conf[1], b_estad_z_crit, b_int_conf[2], col = "blue", lwd = 2)

# Punto en g_hat
points(b_estad_z_crit, b_edad_max_salario, col = "blue", pch = 19)
text(b_estad_z_crit + 0.1, b_edad_max_salario, "Edad_estim W_max", pos = 4, col = "blue")

# "Para el int confianza creado a partir del metodo delta, aceptaríamos la Hip nula de que edad=50 maximiza al salario"
# NO ME GUSTA COMO ESTA REDACTADO ESTO, es poco riguroso, hay que decirlo como que "esto sugiere que probablemente rechazaríamos"

# Definimos la función de restricción no lineal
g <- function(coef) {
  beta1 <- coef["edad"]
  beta2 <- coef["I(edad {2)"]
  return((-1*beta1)/(2*(beta2)))
}

# Obtener las estimaciones de los coeficientes
b_betas_hat <- coef(reg_b)
b_beta1_hat <- b_betas_hat["edad"]
b_beta2_hat <- b_betas_hat["I(edad^2)"]

# Evaluar la función de restricción en las estimaciones
#restriction_value <- g(b_beta1_hat, b_beta2_hat)
b_hipotesis_nl <- g(b_betas_hat)

print(b_beta1_hat)
print(b_beta2_hat)

##Armado de test de hipotesis simil 'testnl' en STATA
# transformation <- function(coef) { exp(coef["edad"]) / (1 +coef["I(edad^2)"]) }

# A MANOPLA ####
# Extraer los coeficientes
beta_hat <- coef(reg_b)

# Extraer la matriz de varianza-covarianza de los coeficientes
vcov_beta_hat <- vcov(reg_b)

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