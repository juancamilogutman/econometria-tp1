#PUNTO 1A

# rm(list=ls()) #Limpiamos la memoria

library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(modelsummary)    # Mejores tablas de regresión 
library(tinytable)       # Motor de creación de tablas
library(sandwich)        # Robust Covariance Matrix Estimators
library(marginaleffects) # Método Delta implementación
library(quantreg)        # Regresión por cuantiles
library(lmtest)

options(tinytable_tt_digits = 3)
options(tinytable_theme_placement_latex_float = "H")

eph1 <- readRDS("Bases/eph_1abc.RDS")

#De la limpieza ya trajimos las variables categóricas como factores
reg <- lm(logSal ~ educf + edadi +  est_civ + region, data = eph1)

# La regresión para hacer el test "manualmente" es la siguiente:
# reg_residuos <- lm(I(residuos^2) ~ educf + edadi + est_civ + region, data = eph1)
# eph1$residuos <- residuals(reg)


# Corremos un test de Breusch-Pagan con la librería lmtest
bp_reg <- bptest(reg)
print(bp_reg)

# Realizamos la prueba de Cameron & Trivedi
ct_reg <- bptest(reg, ~ educf + edadi + est_civ + region, data = eph1, studentize = FALSE)

# Mostramos los resultados de la prueba de Cameron & Trivedi
print(ct_reg)

rdo_clasica_y_robusta <- modelsummary(reg,
                         escape = TRUE,
                         shape = term ~ model + statistic,
                         #cap = "1er regresión",
                         estimate="{estimate}{stars}",
                         statistic = c("p.value", "conf.low", "conf.high"),
                         stars = c('*' = .1,
                                   '**' = .05,
                                   '***'=0.01
                         ),
                         vcov = c("classical", "robust") #Compara errores estándar robustos y no robustos
)

rdo_clasica_y_robusta

# Contraste popr supuesto de homocedasticidad con Breusch-Pagan  
# Gabriel dijo en el minuto 1:17:00 de la clase del tp que para
# hacer un contraste de si se cumple homocedasticidad habría que
# correr una regresión de los residuos al cuadrado contra las 
# variables explicativas.



# Ver el p valor del test F de esa regresión auxiliar.

#PARA INFERENCIA ROBUSTA DE WHITE:
#modelsummary(reg3,vcov = "robust")
#incluso, capaz se pueden comparar con:
# vcov = c("classical", "robust")

##Calcular matriz de covarianza robusta (corrección de heterocedasticidad) Las
##opciones comunes son "HC0", "HC1", "HC2", "HC3", etc. Estas opciones difieren
##en la forma en que ajustan la matriz de covarianza. Por ejemplo, type = "HC0"
##es la estimación más básica y type = "HC3" es la más conservadora.

vcov_White <- vcovHC (reg, type="HC0")

##Obtenemos estadísticos de inferencia robustas (pruebas de hipótesis de los
##coef obtenidos pero utilizando la mat de covarianzas robusta)

# df_coef <- as.data.frame(coeftest(reg, vcov_White))