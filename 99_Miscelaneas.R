# format:
#   pdf:
#     toc: true
#     toc-depth: 3
#     toc-location: left
#     number-sections: true
#     mainfont: Latin Modern Roman
#     sansfont: Latin Modern Roman
#     include-in-header:
#       text: |
#         \usepackage{typearea}

##Para cambiar la orientación con latex (y un header)
#
# \newpage
# 
# \KOMAoptions{paper=landscape,pagesize}
# 
# \recalctypearea
# 
# y luego...
# 
# 
# \newpage
# 
# \KOMAoptions{paper=portrait,pagesize}
# 
# \recalctypearea

# project:
#   title: "1er Trabajo Práctico de Econometría"
# output-dir: Salidas
# format:
#   pdf:
#   toc: true
# toc-depth: 3
# toc-location: left
# lang: es
# bibliography: biblio.bib
# csl: apa7.csl

# ```{r}
# #| echo: true
# #| eval: false # skip evaluation
# #| file: C:/directory/script.R
# ```

# msummary(
#   models,
#   output = "default",
#   fmt = 3,
#   estimate = "estimate",
#   statistic = "std.error",
#   vcov = NULL,
#   conf_level = 0.95,
#   exponentiate = FALSE,
#   stars = FALSE,
#   shape = term + statistic ~ model,
#   coef_map = NULL,
#   coef_omit = NULL,
#   coef_rename = FALSE,
#   gof_map = NULL,
#   gof_omit = NULL,
#   group_map = NULL,
#   add_columns = NULL,
#   add_rows = NULL,
#   align = NULL,
#   notes = NULL,
#   title = NULL,
#   escape = TRUE,
#   ...
# )

# #ver si sirve esto:
# 
# # Obtener la tabla ANOVA
# anova_a_reg1 <- anova(a_reg1)
# 
# # Crear un resumen similar al de Stata
# resumen_anova <- data.frame(
#   Source = c("Model", "Residual", "Total"),
#   SS = c(sum(anova_a_reg1$"Sum Sq")[1], anova_a_reg1$"Sum Sq"[2], sum(anova_a_reg1$"Sum Sq")),
#   df = c(sum(anova_a_reg1$"Df")[1], anova_a_reg1$"Df"[2], sum(anova_a_reg1$"Df")),
#   MS = c(sum(anova_a_reg1$"Sum Sq")[1] / sum(anova_a_reg1$"Df")[1], anova_a_reg1$"Sum Sq"[2] / anova_a_reg1$"Df"[2], sum(anova_a_reg1$"Sum Sq") / sum(anova_a_reg1$"Df"))
# )
# 
# # Añadir estadísticas de bondad de ajuste
# resumen_anova$`Number of obs` <- nrow(eph1)
# resumen_anova$`F value` <- summary(a_reg1)$fstatistic[1]
# resumen_anova$`Prob > F` <- pf(summary(a_reg1)$fstatistic[1], summary(a_reg1)$fstatistic[2], summary(a_reg1)$fstatistic[3], lower.tail = FALSE)
# resumen_anova$`R-squared` <- summary(a_reg1)$r.squared
# resumen_anova$`Adj R-squared` <- summary(a_reg1)$adj.r.squared
# resumen_anova$`Root MSE` <- sigma(a_reg1)
# 
# # Mostrar el resumen
# View(resumen_anova)

# # CON PAQUETE CAR ####
# resultado_prueba <- linearHypothesis(reg_b, "100 * I(edad^2) + edad = 0")
# 
# anova(reg_b)
# 
# # CON PAQUETE MARGINAL EFFECTS ####
# hypotheses(reg_b, hypothesis = "(-1*edad)/(2*I(edad^2)) - 50 = 0") %>% tt()
# 
# hypotheses(reg_b, hypothesis = "100 * I(edad^2) + edad = 0") %>% tt()