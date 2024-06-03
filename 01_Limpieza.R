#Limpieza de las bases de datos ####

rm(list=ls()) #Limpiamos la memoria

##Librerías####
library(tidyverse)    #Para manejar bases de datos.
library(eph)          #Librería hecha por científicxs argentinxs: una caja de 
                      #herramentas para manejar la EPH.

##Descarga de base####
df <- get_microdata(year = 2023,
                    trimester = 4,
                    type = 'individual'
                    ) %>% 
      organize_labels(type='individual') 

##Transformaciones y filtros####

### Transformaciones de tipo de variable
df <- df %>% mutate_at(vars(NIVEL_ED,
                            AGLOMERADO,
                            CH07,
                            ESTADO,
                            REGION
                            ),
                       ~as.factor(.)
                       )

## Generación de la variable de años de educación
## ESTO TODAVÍA NO FUNCIONA
df <- df %>%
  mutate(CH14 = case_when(
    CH13 != 1 & CH13 != 2 ~ 0
          )
        ) %>%
  mutate(ult_anio = as.numeric(CH14))

df <- df %>%
  mutate(educ = case_when(
    CH12 == 2 & NIVEL_ED == "Primaria incompleta" ~ ult_anio,             
    CH12 == 2 & NIVEL_ED == "Primaria completa" ~ 7,
    CH12 == 3 & NIVEL_ED == "Primaria incompleta" ~ ult_anio,            
    CH12 == 3 & NIVEL_ED == "Primaria completa" ~ 9,    
    CH12 == 4 & NIVEL_ED == "Secundaria incompleta"~ (7 + ult_anio),       
    CH12 == 4 & NIVEL_ED == "Secundaria completa" ~ 7,    
    CH12 == 5 & NIVEL_ED == "Secundaria incompleta"~ (9 + ult_anio),       
    CH12 == 5 & NIVEL_ED == "Secundaria completa"~ (9),           
    CH12 == 6 ~ (12 + ult_anio),      
    CH12 == 7 ~ (12 + ult_anio),      
    CH12 == 8 ~ (12 + ult_anio),      
    TRUE ~ 0
  ))

#Cambios de nombres de variables
df <- df %>% 
  rename(edad = CH06) %>% 
  rename(est_civ = CH07)

### Filtramos según la consigna del primer punto
df1 <- df %>% 
  filter(CH03 == 1,           #Jefes/as de hogar
         CH04 == 1,           #Hombres          
         edad >= 25,          #Entre 25...      
         edad <= 65,          #...y 65 años     
         ESTADO == "Ocupado", #Ocupados         
         CAT_OCUP == 3,       #Asalariados      
         P21 > 0,             #Salario positivo 
         CH12 != 9            #¬No respuesta    
  )

#Logaritmo del Salario (P21)
df1 <- df1 %>% 
  mutate(logSal = log(df1$P21)
  )

df1 <- df1 %>% 
  select(NIVEL_ED,
         logSal,
         educ,
         edad,
         est_civ,
         REGION,
         AGLOMERADO,
         PONDERA,
         PONDIIO,
         PONDII,
         CODUSU,
         ANO4,
         TRIMESTRE
  )

# • Factores de expansión
# 
# Para minimizar el efecto de la no respuesta de ingresos, se asignó a los no respondentes
# el comportamiento de los respondentes por estrato de la muestra. Por lo tanto,
# para el tratamiento de los ingresos y la pobreza se presentan dos tipos de ponderadores:
# 1. El campo PONDERA, sin corrección, que se utiliza además para el resto de las
# variables.
# 2. Los campos PONDII, PONDIIO, PONDIH con corrección por no respuesta:
# •PONDII para el tratamiento del ingreso total individual (p47t, decindr,
#  adecindr, rde-cindr, pdecindr, gdecindr, idecindr).
# •PONDIIO para el ingreso de la ocupación principal (p21, pp06c, pp06d,
#  pp08d1, pp08d4, pp08f1, pp08f2, decocur, adecocur, rdecocur, pdecocur, gdecocur, idecocur).
# •PONDIH para el ingreso total familiar (ITF, decifr, adecifr, rdecifr,
#  pdecifr, gdecifr, idecifr), el ingreso per cápita familiar (IPCF, deccfr, adecifr, rdecifr, pdecifr, gdecifr, idecifr).

saveRDS(df1, file = "Bases/eph_1abc.RDS")
#saveRDS(df2, file = "eph_1de.RDS")
#saveRDS(df3, file = "eph_2ab.RDS")







