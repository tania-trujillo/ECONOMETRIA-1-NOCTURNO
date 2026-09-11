#####################################################
#########                                   #########
#              UNIVERSIDAD DEL QUINDÍO              #  
#                PROGRAMA DE ECONOMÍA               #
#                   ECONOMETRIÁ I                   #
#########                                   #########
#####################################################

#by: Tania Trujillo Escalante
# tania.trujilloe@uqvirtual.edu.co

##script para analizar los ingresos de la población: base de datos GEIH año 2025---- 
# https://microdatos.dane.gov.co/index.php/catalog/853

getwd()
options(scipen = 100 , digits = 4)
rm(list = ls())

#Paquetes o librerias ----
install.packages("skimr")
install.packages("readxl")
install.packages("stringr")
install.packages("stringi")
install.packages("haven")
install.packages("tidyverse")
install.packages("plyr")
install.packages("rstatix")
install.packages("descr")
install.packages("splitstackshape")
install.packages("e1071")

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

DATA_INGRESOS <- read_csv("C:/Users/Tata/Desktop/ARCHIVO R DATA INGRESOS/DATA_INGRESOS/DATA_INGRESOS.csv")


# Descriptivo
head(DATA_INGRESOS)
str(DATA_INGRESOS , LIST.LEN = 492)
skimr::skim(DATA_INGRESOS)

# Analisis descriptivo: Indicadores de posición y centro -----
DATA_INGRESOS |> dplyr::group_by(1) |> mutate( INGLABO = replace_na(INGLABO , replace = 0) ) |> summarise( 
  median_INGLABO =  median(INGLABO) , 
  mean_INGLABO = mean(INGLABO) ,
  rango_medio_INGLABO = ((max(INGLABO) - min(INGLABO))/2 ) , 
  min_INGLABO = min(INGLABO) ,
  Q1_INGLABO = quantile(INGLABO , c(0.25) , na.rm = TRUE ) ,
  Q3_INGLABO = quantile(INGLABO , c(0.75) , na.rm = TRUE ),
  rango_q_INGLABO = IQR(INGLABO) ,
  max_INGLABO = max(INGLABO)
)

### Indicadores de dispersión




## Analisis descriptivo
### Indicadores de posición y centro -----
DATA_INGRESOS |> dplyr::group_by(1) |> mutate( INGLABO = replace_na(INGLABO , replace = 0) ) |> dplyr::summarise( 
  median_INGLABO =  median(INGLABO) , 
  mean_INGLABO = mean(INGLABO) ,
  rango_medio_INGLABO = ((max(INGLABO) - min(INGLABO))/2 ) , 
  min_INGLABO = min(INGLABO) ,
  Q1_INGLABO = quantile(INGLABO , c(0.25) , na.rm = TRUE ) ,
  Q3_INGLABO = quantile(INGLABO , c(0.75) , na.rm = TRUE ),
  rango_q_INGLABO = IQR(INGLABO) ,
  max_INGLABO = max(INGLABO)
)

### Indicadores de dispersión
DATA_INGRESOS |> dplyr::group_by(1) |> mutate( INGLABO = replace_na(INGLABO , replace = 0) ) |> dplyr::summarise(
  rango_INGLABO = (max(INGLABO) - min(INGLABO)),
  sd_INGLABO = sd(INGLABO) ,
  varianza_INGLABO = var(INGLABO) ,
  c_variacion_INGLABO = (sd(INGLABO) / mean(INGLABO) *100) ,
  c_curtosis_INGLABO = kurtosis(INGLABO) ,
  c_asimetria_INGLABO = skewness(INGLABO)
)

