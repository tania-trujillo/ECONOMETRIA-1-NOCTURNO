###################################################################

######                                                       ######

#                      UNIVERSIDAD DEL QUINDÍO                    #

#                       PROGRAMA DE ECONOMIA                      #

#                            ECONOMETRÍA I                        #

######                                                       ######

###################################################################

# BY: tania Trujillo Escalante


## REGRESIÓN LINEAL SIMPLE ----

# https://microdatos.dane.gov.co/index.php/catalog/853

getwd()

options('scipen' = 100 , 'digits' = 4)

rm(list = ls())

# Paquetes o librerias ----

library("skimr")

library("readxl")

library("stringr")

library("stringi")

library("haven")

library("tidyverse")

library("plyr")

library("rstatix")

library("descr")

library("splitstackshape")

library("e1071")

## Cargamos la base de datos

DATA = readxl::read_excel("C:/Users/Tata/Desktop/REGRESION LINEAL/Beta_estimado.xlsx") |> data.frame()

## Descriptivo-----

head(DATA)

str(DATA , list.len = 3)

skimr::skim(DATA)

Colombia - Gran Encuesta Integrada de Hogares - GEIH - 2025
El Departamento Administrativo Nacional de Estadística (DANE) ha desarrollado e implementado encuestas de hogares, desde finales de la década de los años 1960, que consideran las temáticas de fuerz...

## Ojo recordemos que es una muestra muy corta

## Analisis descriptivo

### Indicadores de posición y centro -----

for (variable in c("Tasa.de.ahorro", "Tasa.de.interes")) {
  
  DATA |> 
    
    dplyr::summarise(
      
      variable       = variable,
      
      mediana        = median(.data[[variable]], na.rm = TRUE),
      
      media          = mean(.data[[variable]], na.rm = TRUE),
      
      rango_medio    = (max(.data[[variable]], na.rm = TRUE) - min(.data[[variable]], na.rm = TRUE)) / 2,
      
      minimo         = min(.data[[variable]], na.rm = TRUE),
      
      Q1             = quantile(.data[[variable]], 0.25, na.rm = TRUE),
      
      Q3             = quantile(.data[[variable]], 0.75, na.rm = TRUE),
      
      rango_q        = IQR(.data[[variable]], na.rm = TRUE),
      
      maximo         = max(.data[[variable]], na.rm = TRUE)
      
    ) |> 
    
    print()
  
}

### Indicadores de dispersión --------

for (variable in c("Tasa.de.ahorro", "Tasa.de.interes")) {
  
  DATA |> dplyr::group_by(1) |> dplyr::summarise(
    
    rango = (max(.data[[variable]]) - min(.data[[variable]])),
    
    sd = sd(.data[[variable]]) ,
    
    varianza = var(.data[[variable]]) ,
    
    c_variacion = (sd(.data[[variable]]) / mean(.data[[variable]]) *100) ,
    
    c_curtosis = kurtosis(.data[[variable]]) ,
    
    c_asimetria = skewness(.data[[variable]])
    
  ) |> print()
  
}
install.packages("e1071")
library(e1071)

