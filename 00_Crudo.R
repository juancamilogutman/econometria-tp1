library(tidyverse)    #Para manejar bases de datos
library(ggplot2)      #Para graficar
library(modelsummary) #Mejores tablas de regresión 
library(tinytable)    #Motor de creación de tablas

options(tinytable_tt_digits = 3)
options(tinytable_theme_placement_latex_float = "H")

eph1 <- readRDS("Bases/eph_1abc.RDS")

#De la limpieza ya trajimos las variables categóricas como factores
reg1 <- lm(logSal ~ NIVEL_ED + edad +  est_civ + REGION, data = eph1)
reg2 <- lm(logSal ~ NIVEL_ED + edad + I(edad^2) +  est_civ + REGION, data = eph1)
regs <- list("Sin edad^2" = reg1, "Con edad^2" = reg2)

resultados <- modelsummary(regs,
                           escape = TRUE,
                           shape = term ~ model + statistic,
                           #cap = "1er regresión",
                           estimate="{estimate}{stars}",
                           statistic = c("p.value", "conf.low", "conf.high"),
                           stars = c('*' = .1,
                                     '**' = .05,
                                     '***'=0.01
                                     ),

                           )
resultados


#para la inferencia robusta de white:
# modelsummary(reg3,vcov = "robust")

#incluso, capaz se pueden comparar con:
# vcov = c("classical", "robust")

