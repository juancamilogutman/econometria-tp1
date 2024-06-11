library(tidyverse)    #Para manejar bases de datos
library(ggplot2)      #Para graficar
library(modelsummary) #Mejores tablas de regresión 
library(tinytable)    #Motor de creación de tablas

options(tinytable_tt_digits = 3)
options(tinytable_theme_placement_latex_float = "H")

eph1 <- readRDS("Bases/eph_1abc.RDS")
str(eph1)

#De la limpieza ya trajimos las variables categóricas como factores

#PUNTO 1A
reg1 <- lm(logSal ~ educn + edad +  est_civ + region, data = eph1)
reg2 <- lm(logSal ~ educf + edad +  est_civ + region, data = eph1)
regs <- list("Sin edad^2" = reg1, "Con edad^2" = reg2)

res_reg1 <- modelsummary(reg1,
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
res_reg1 #comparación entre


#para la inferencia robusta de white:
# modelsummary(reg3,vcov = "robust")

#incluso, capaz se pueden comparar con:
# vcov = c("classical", "robust")

#PUNTO 1B
reg1 <- lm(logSal ~ educf + edad +  est_civ + region, data = eph1)

