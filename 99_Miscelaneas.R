## Cuando queramos redefinir la categoría base:
# 
# CPS1985$gender <- relevel(CPS1985$gender,"female")
# CPS1985$occupation <- relevel(CPS1985$occupation,"management")

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