#Limpieza de las bases de datos ####

rm(list=ls()) #Limpiamos la memoria

##Librerias####
library(tidyverse)    #Para manejar bases de datos.
library(eph)          #Libreria hecha por cientificxs argentinxs. 

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

### Generacion de la variable de años de educacion ####
df <- df %>% 
  mutate(CH14bis = replace_na(CH14, 0)) %>% 
  mutate(CH14bis = replace(CH14, CH14 == "", 0))

df <- df %>% 
  mutate(ult_anio = as.numeric(CH14bis)) %>%
  filter(ult_anio != 98, #¬Educacion especial
         ult_anio != 99) #¬Ns/Nr

df <- df %>%
  mutate(educn = case_when(               #Nivel mas alto cursado:
    CH12 == 1 ~ 0,                           #Jardin o preescolar
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
  mutate(est_civ = recode_factor(CH07,
                                 "1" = "Unido",
                                 "2" = "Casado",
                                 "3" = "Separado/Divorciado",
                                 "4" = "Viudo",
                                 "5" = "Soltero"
                                 )
        ) %>%
  mutate(region = recode_factor(REGION,
                                 "1" = "GBA",
                                "40" = "Noroeste",                                
                                "41" = "Noreste",                                
                                "42" = "Cuyo",                                
                                "43" = "Pampeana",                                
                                "44" = "Patagonia"
                                )
        ) %>%
  mutate(aglomerado = recode_factor(AGLOMERADO,
                                 "2" = "Gran La Plata",
                                "02" = "Gran La Plata",
                                 "3" = "Bahia Blanca - Cerri",
                                "03" = "Bahia Blanca - Cerri",
                                 "4" = "Gran Rosario",
                                "04" = "Gran Rosario",
                                 "5" = "Gran Santa Fe",
                                "05" = "Gran Santa Fe",
                                 "6" = "Gran Parana",
                                "06" = "Gran Parana",
                                 "7" = "Posadas",
                                "07" = "Posadas",
                                 "8" = "Gran Resistencia",
                                "08" = "Gran Resistencia",
                                 "9" = "Comodoro Rivadavia - Rada Tilly",
                                "09" = "Comodoro Rivadavia - Rada Tilly",
                                "10" = "Gran Mendoza",
                                "12" = "Corrientes",
                                "13" = "Gran Cordoba",
                                "14" = "Concordia",
                                "15" = "Formosa",
                                "17" = "Neuquen - Plottier",
                                "18" = "Santiago del Estero - La Banda",
                                "19" = "Jujuy - Palpala",
                                "20" = "Rio Gallegos",
                                "22" = "Gran Catamarca",
                                "23" = "Gran Salta",
                                "25" = "La Rioja",
                                "26" = "Gran San Luis",
                                "27" = "Gran San Juan",
                                "29" = "Gran Tucuman - Tafi Viejo",
                                "30" = "Santa Rosa - Toay",
                                "31" = "Ushuaia - Rio Grande",
                                "32" = "Ciudad Autonoma de Buenos Aires",
                                "33" = "Partidos del GBA",
                                "34" = "Mar del Plata",
                                "36" = "Rio Cuarto",
                                "38" = "San Nicolas - Villa Constitucion",
                                "91" = "Rawson - Trelew",
                                "93" = "Viedma - Carmen de Patagones"
                                )
         ) %>% 
  mutate(educf = recode_factor(NIVEL_ED,
                               "1" = "Primario incompleto",
                               "2" = "Primario completo",
                               "3" = "Secundario incompleto",
                               "4" = "Secundario completo",
                               "5" = "Superior universitario incompleto",
                               "6" = "Superior universitario completo",
                               "7" = "Sin instruccion",
                               "9" = "Ns/Nr"
                               )
         )

### Filtramos segan la consigna del primer punto
df1 <- df %>% 
  filter(CH03 == 1,           #Jefes/as de hogar
         CH04 == 1,           #Hombres          
         edad >= 25,          #Entre 25...      
         edad <= 65,          #...y 65 años     
         ESTADO == 1,         #Ocupados         
         CAT_OCUP == 3,       #Asalariados      
         P21 > 0,             #Salario positivo 
         CH12 != 9            #¬Educacion especial    
  )

#Logaritmo del Salario (P21)
df1 <- df1 %>% 
  mutate(logSal = log(df1$P21)
  )

df1 <- df1 %>% 
  select(logSal,
         educn,
         educf,
         edad,
         est_civ,
         region,
         aglomerado,
         PONDERA,
         PONDIIO,
         PONDII
  )

# • Factores de expansion
# 
# Para minimizar el efecto de la no respuesta de ingresos, se asigno a los no respondentes
# el comportamiento de los respondentes por estrato de la muestra. Por lo tanto,
# para el tratamiento de los ingresos y la pobreza se presentan dos tipos de ponderadores:
# 1. El campo PONDERA, sin correccion, que se utiliza ademas para el resto de las
# variables.
# 2. Los campos PONDII, PONDIIO, PONDIH con correccion por no respuesta:
# •PONDII para el tratamiento del ingreso total individual (p47t, decindr,
#  adecindr, rde-cindr, pdecindr, gdecindr, idecindr).
# •PONDIIO para el ingreso de la ocupacion principal (p21, pp06c, pp06d,
#  pp08d1, pp08d4, pp08f1, pp08f2, decocur, adecocur, rdecocur, pdecocur, gdecocur, idecocur).
# •PONDIH para el ingreso total familiar (ITF, decifr, adecifr, rdecifr,
#  pdecifr, gdecifr, idecifr), el ingreso per capita familiar (IPCF, deccfr, adecifr, rdecifr, pdecifr, gdecifr, idecifr).

saveRDS(df1, file = "Bases/eph_1abc.RDS")
#saveRDS(df2, file = "eph_1de.RDS")
#saveRDS(df3, file = "eph_2ab.RDS")

#sjlabelled::write_stata(df1, "Bases/eph_1abc.dta")








