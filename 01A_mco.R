#PUNTO 1A

# rm(list=ls()) #Limpiamos la memoria

library(tidyverse)       # Para manejar bases de datos
library(ggplot2)         # Para graficar
library(sandwich)        # Robust Covariance Matrix Estimators
library(marginaleffects) # Método Delta implementación
library(quantreg)        # Regresión por cuantiles
library(lmtest)

options(tinytable_tt_digits = 3)
options(tinytable_theme_placement_latex_float = "H")

eph1 <- readRDS("Bases/eph_1abc.RDS")

#De la limpieza ya trajimos las variables categóricas como factores
reg1 <- lm(logSal ~ educf + edadi +  est_civ + region, data = eph1)

stargazer(reg1, type = "text",
          omit.stat = c("LL", "ser"),
          single.row = TRUE,
          header = FALSE)

robust_cov <- vcovHC(reg1, type = "HC1")

stargazer(reg1, reg1, type = "text",
          se = list(NULL, sqrt(diag(robust_cov))),
          title = "Comparación del modelo con Desvíos Estándar Clásicos y Robustos de White",
          model.names = TRUE,
          model.numbers = TRUE,
          omit.stat = c("LL", "ser"),
          single.row = TRUE,
          header = FALSE)

msumreg_clasica <- modelsummary(reg1,
                         escape = TRUE,
                         shape = term ~ model + statistic,
                         cap = "Regresión con Desvíos Clásicos",
                         #width = 0.8,
                         # estimate = "estimate",
                         coef_rename = coef_rename,
                         #coef_map = cm,
                         estimate="{estimate}{stars}",
                         statistic = c("p.value",
                                       "conf.low",
                                       "conf.high"
                         ),
                         stars = c('*' = .1,
                                   '**' = .05,
                                   '***'=0.01
                         )
)

msumreg_robusta <- modelsummary(reg1,
                                escape = TRUE,
                                vcov = robust_cov,
                                shape = term ~ model + statistic,
                                cap = "Regresión con Desvíos Clásicos",
                                #width = 0.8,
                                # estimate = "estimate",
                                coef_rename = coef_rename,
                                #coef_map = cm,
                                estimate="{estimate}{stars}",
                                statistic = c("p.value",
                                              "conf.low",
                                              "conf.high"
                                ),
                                stars = c('*' = .1,
                                          '**' = .05,
                                          '***'=0.01
                                )
)

# La regresión para hacer el test "manualmente" es la siguiente:
# eph1$residuos <- residuals(reg)
# reg_residuos <- lm(I(residuos^2) ~ educf + edadi + est_civ + region, data = eph1)

# Corremos un test de Breusch-Pagan con la librería lmtest
bp_reg <- bptest(reg)
print(bp_reg)

# Realizamos la prueba de Cameron & Trivedi
ct_reg <- bptest(reg, ~ educf + edadi + est_civ + region, data = eph1, studentize = FALSE)

# Mostramos los resultados de la prueba de Cameron & Trivedi
print(ct_reg)


##Calcular matriz de covarianza robusta (corrección de heterocedasticidad) Las
##opciones comunes son "HC0", "HC1", "HC2", "HC3", etc. Estas opciones difieren
##en la forma en que ajustan la matriz de covarianza. Por ejemplo, type = "HC0"
##es la estimación más básica y type = "HC3" es la más conservadora.

vcov_White <- vcovHC (reg, type="HC0")

##Obtenemos estadísticos de inferencia robustas (pruebas de hipótesis de los
##coef obtenidos pero utilizando la mat de covarianzas robusta)

# df_coef <- as.data.frame(coeftest(reg, vcov_White))