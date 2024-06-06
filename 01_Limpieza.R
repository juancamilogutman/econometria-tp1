#Limpieza de las bases de datos ####

rm(list=ls()) #Limpiamos la memoria

##Librerías####
library(tidyverse)    #Para manejar bases de datos.
library(eph)          #Librería hecha por científicxs argentinxs. 

##Descarga de base####
df <- get_microdata(year = 2023, #del paquete EPH
                    trimester = 4,
                    type = 'individual'
                    ) 

## Transformaciones y filtros ####

### Transformaciones de tipo de variable ####
df <- df %>% mutate_at(vars(NIVEL_ED,
                            AGLOMERADO,
                            CH07, #estado civil
                            ESTADO,
                            REGION
                            ),
                       ~as.factor(.)
                       )

### Generación de la variable de años de educación ####
df <- df %>% 
  mutate(CH14bis = replace_na(CH14, 0)) %>% 
  mutate(CH14bis = replace(CH14, CH14 == "", 0))

df <- df %>% 
  mutate(ult_anio = as.numeric(CH14bis)) %>%
  filter(ult_anio != 98, #¬Educación especial
         ult_anio != 99) #¬Ns/Nr

# df$ult_anio <- remove_var_label(df$ult_anio)

df <- df %>%
  mutate(educn = case_when(               #Nivel más alto cursado:
    CH12 == 1 ~ 0,                           #Jardín o preescolar
    CH12 == 2 & CH13 == 1 ~ 7,               #Primario completo
    CH12 == 2 & CH13 == 2 ~ (0 + ult_anio),  #Primario incompleto
    CH12 == 3 & CH13 == 1 ~ 9,               #EGB completo          
    CH12 == 3 & CH13 == 2 ~ (0 + ult_anio),  #EGB incompleto
    CH12 == 4 & CH13 == 1 ~ 12,              #Secundario completo      
    CH12 == 4 & CH13 == 2 ~ (7 + ult_anio),  #Secundario incompleto    
    CH12 == 5 & CH13 == 1 ~ 12,              #Polimodal completo
    CH12 == 5 & CH13 == 2 ~ (9 + ult_anio),  #Polimodal incompleto        
    CH12 == 6 & CH13 == 1 ~ 15,              #Terciario completo
    CH12 == 6 & CH13 == 2 ~ (12 + ult_anio), #Terciario incompleto
    CH12 == 7 & CH13 == 1 ~ 18,              #Universitario completo
    CH12 == 7 & CH13 == 2 ~ (12 + ult_anio), #Universitario incompleto  
    CH12 == 8 & CH13 == 1 ~ 22,              #Posgrado completo
    CH12 == 8 & CH13 == 2 ~ (18 + ult_anio), #Posgrado incompleto        
    TRUE ~ 0
  ))

#Cambios de nombres de variables
df <- df %>% 
  rename(edad = CH06) %>%
  rename(est_civ = CH07) %>%
  mutate(est_civ = recode_factor(est_civ,
                                 "1" = "Unido",
                                 "2" = "Casado",
                                 "3" = "Separado/Divorciado",
                                 "4" = "Viudo",
                                 "5" = "Soltero"
                                 )
        )
    
### Filtramos según la consigna del primer punto
df1 <- df %>% 
  filter(CH03 == 1,           #Jefes/as de hogar
         CH04 == 1,           #Hombres          
         edad >= 25,          #Entre 25...      
         edad <= 65,          #...y 65 años     
         ESTADO == 1,         #Ocupados         
         CAT_OCUP == 3,       #Asalariados      
         P21 > 0,             #Salario positivo 
         CH12 != 9            #¬Educación especial    
  )

#Logaritmo del Salario (P21)
df1 <- df1 %>% 
  mutate(logSal = log(df1$P21)
  )

df1 <- df1 %>% 
  select(NIVEL_ED,
         logSal,
         educn,
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







