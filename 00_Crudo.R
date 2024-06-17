rm(list=ls()) #Limpiamos la memoria

library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(sandwich)        # Robust Covariance Matrix Estimators
library(marginaleffects) # Método Delta implementación
library(quantreg)        # Regresión por cuantiles
library(msm)             # Test simil testnl de STATA

options(tinytable_tt_digits = 3)
options(tinytable_theme_placement_latex_float = "H")

eph1 <- readRDS("Bases/eph_1abc.RDS")
#str(eph1)

#De la limpieza ya trajimos las variables categóricas como factores

#PUNTO 1A
a_reg1 <- lm(logSal ~ educn + edad +  est_civ + region, data = eph1)
reg2 <- lm(logSal ~ educf + edad +  est_civ + region, data = eph1)
reg3 <- lm(logSal ~ educf + edad +  est_civ + region, data = eph1)
regs <- list("Años de educación" = a_reg1, "Nivel de educación" = reg2, "Salario No Log" = reg3)

res_reg1 <- modelsummary(a_reg1,
                           escape = TRUE,
                           shape = term ~ model + statistic,
                           #cap = "1er regresión",
                           estimate="{estimate}{stars}",
                           statistic = c("p.value", "conf.low", "conf.high"),
                           stars = c('*' = .1,
                                     '**' = .05,
                                     '***'=0.01
                                     ),
                           # vcov = "classical" #para var y covar clásicas
                           vcov = c("classical", "robust") #Compara errores estándar robustos y no robustos
                           )

#Contraste popr supuesto de homocedasticidad con Breusch-Pagan
#
#
#
#

#PARA INFERENCIA ROBUSTA DE WHITE:
#modelsummary(reg3,vcov = "robust")
#incluso, capaz se pueden comparar con:
# vcov = c("classical", "robust")

##Calcular matriz de covarianza robusta (corrección de heterocedasticidad) Las
##opciones comunes son "HC0", "HC1", "HC2", "HC3", etc. Estas opciones difieren
##en la forma en que ajustan la matriz de covarianza. Por ejemplo, type = "HC0"
##es la estimación más básica y type = "HC3" es la más conservadora.

vcov_White <- vcovHC (a_reg1, type="HC0")

##Obtenemos estadísticos de inferencia robustas (pruebas de hipótesis de los
##coef obtenidos pero utilizando la mat de covarianzas robusta)

# df_coef <- as.data.frame(coeftest(a_reg1, vcov_White))

#PUNTO 1B
b_reg1 <- lm(logSal ~ educn + edad + I(edad^2) + est_civ + region, data = eph1)
b_coef_estim <- coef(b_reg1)
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

b_vcov_mat <- vcov(b_reg1)
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

#"Para el int confianza creado a partir del metodo delta, aceptamos la Hip nula de que edad=50 maximiza al salario"

# Instala y carga el paquete car si no está instalado
library(car)

# Supongamos que ajustaste un modelo lineal
# Realiza una prueba de hipótesis lineal
#linearHypothesis(b_reg1, c("edad=50"))
#view(linearHypothesis(b_reg1, c("edad+I(edad^2)=50")))

# Definir la función de restricción no lineal
g <- function(coef) {
  beta1 <- coef["edad"]
  beta2 <- coef["I(edad {2)"]
  return((-1*beta1)/(2*(beta2)))
}

# Obtener las estimaciones de los coeficientes
b_betas_hat <- coef(b_reg1)
b_beta1_hat <- b_betas_hat["edad"]
b_beta2_hat <- b_betas_hat["I(edad^2)"]

# Evaluar la función de restricción en las estimaciones
#restriction_value <- g(b_beta1_hat, b_beta2_hat)
b_hipotesis_nl <- g(b_betas_hat)

print(b_beta1_hat)
print(b_beta2_hat)

##Armado de test de hipotesis simil 'testnl' en STATA

transformation <- function(coef) { exp(coef["edad"]) / (1 +coef["I(edad^2)"]) }

# You can use deltamethod to get the standard errors of the transformation:
# result <- deltamethod(g, b_betas_hat, vcov(b_reg1))
# print(result)

# The result will contain the estimate, its standard error, and confidence interval

# Define the hypothesis function
#hypothesis <- function(b_betas_hat) {
#  b_betas_hat[2]^2 + coef[3] - 1
#}

# Perform the non-linear Wald test
#b_result_nl <- nlWaldtest(b = b_betas_hat, V = b_vcov_mat, h = b_hipotesis_nl)
# Print the result
#print(b_result_nl)


#PUNTO 1C

# Ajustar un modelo de regresión cuantílica
c_reg <- rq(logSal ~ educf + edad +  est_civ + region, tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), data = eph1)

# Mostrar resumen del modelo
resumen_cuant <- summary(c_reg)



# Obtener coeficientes por cuantil
#coef(c_reg)
