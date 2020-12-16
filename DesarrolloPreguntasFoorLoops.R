# Limpiado de variables
rm(list = ls())

#instalamos paquetes 
install.packages("dplyr")
install.packages("tidyverse")

# cargar las librerias
library(rvest)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)

#Fijamos la ruta donde se encuentran los datos 
setwd("C:/Users/Bastian Matamala/Documents/bigdata/tarea_foorloops")

#Debemos cargar todas las bases de datos en r 

#Categoria Grandes 
#Chile

grandes.chile <- read.csv2("grandes_chile.csv")
print(grandes.chile)

#Colombia 

grandes.colombia <- read.csv2("grandes_colombia.csv")
print(grandes.colombia)

#Peru

grandes.peru <- read.csv2("grandes_peru.csv")
print(grandes.peru)

#Categoria Mediana
#Chile 

medianas.chile <- read.csv2("medianas_chile.csv")
print(medianas.chile)

#Colombia

medianas.colombia <- read.csv2("medianas_colombia.csv")
print(medianas.colombia)

#Peru

medianas.peru <- read.csv2("medianas_peru.csv")
print(medianas.peru)

#Categoria Micro 
#Chile

micro.chile <- read.csv2("micro_chile.csv")
print(micro.chile)

#Colombia 

micro.colombia <- read.csv2("micro_colombia.csv")
print(micro.colombia)

#Peru 

micro.peru <- read.csv2("micro_peru.csv")
print(micro.peru)

#Categoria Pequeña
#Chile 

pequena.chile <- read.csv2("pequena_chile.csv")
print(pequena.chile)

#Colombia 

pequena.colombia <- read.csv2("pequena_colombia.csv")
print(pequena.colombia)

#Peru

pequena.peru <- read.csv2("pequena_peru.csv")
print(pequena.peru)

#Agregamos Tamaño a cada base 

#Chile 

granchile <- data.frame(grandes.chile,"tamanio"=as.character(c("grande")))
granchile

mediachile <- data.frame(medianas.chile,"tamanio"=as.character(c("mediana")))
mediachile

microchile <- data.frame(micro.chile,"tamanio"=as.character(c("micro")))
microchile 

pequechile <- data.frame(pequena.chile,"tamanio"=as.character(c("pequena")))
pequechile

#Colombia 

grancolombia <- data.frame(grandes.colombia,"tamanio"=as.character(c("grande")))
grancolombia

mediacolombia <- data.frame(medianas.colombia,"tamanio"=as.character(c("mediana")))
mediacolombia

microcolombia <- data.frame(micro.colombia,"tamanio"=as.character(c("micro")))
microcolombia

pequecolombia <- data.frame(pequena.colombia,"tamanio"=as.character(c("pequena")))
pequecolombia

#Peru
granperu<- data.frame(grandes.peru,"tamanio"=as.character(c("grande")))
granperu

mediaperu <- data.frame(medianas.peru,"tamanio"=as.character(c("mediana")))
mediaperu

microperu <- data.frame(micro.peru,"tamanio"=as.character(c("micro")))
microperu

pequeperu <- data.frame(pequena.peru,"tamanio"=as.character(c("pequena")))
pequeperu

# Pregunta 2

#Unir todads las bases de datos 
#Se debe modificar algunas cosas de la base de datos por problemas de union 

granChile <- granchile %>%
  rename(porcentajeMujeres = procentaje_mujeres)
granColombia <- grancolombia %>%
  rename(porcentajeMujeres = procentaje_mujeres)
granPeru <- granperu %>%
  rename(porcentajeMujeres = procentaje_muejeres)
mediaChile <- mediachile %>%
  rename(porcentajeMujeres = procentaje_mujeres)
mediaColombia <- mediacolombia %>%
  rename(porcentajeMujeres = porcentaje_mujeres)
mediaPeru <- mediaperu %>%
  rename(porcentajeMujeres = procentaje_muejeres)
microChile <- microchile %>%
  rename(porcentajeMujeres = procentaje_muejeres)
microColombia <- microcolombia %>%
  rename(porcentajeMujeres = porcentaje_mujeres)
microPeru <- microperu %>%
  rename(porcentajeMujeres = porcentaje_mujeres)
pequeChile <- pequechile %>%
  rename(porcentajeMujeres = porcentaje_mujeres)
pequeColombia <- pequecolombia %>%
  rename(porcentajeMujeres = porcentaje_mujeres)
pequePeru <- pequeperu %>%
  rename(porcentajeMujeres = procentaje_mujeres)

#Unimos las base de datos 

granChile %>%
  union_all(granColombia) %>% 
  union_all(granPeru) %>% 
  union_all(mediaChile) %>% 
  union_all(mediaColombia) %>% 
  union_all(mediaPeru) %>% 
  union_all(microChile) %>% 
  union_all(microColombia) %>% 
  union_all(microPeru) %>% 
  union_all(pequeChile) %>% 
  union_all(pequeColombia) %>% 
  union_all(pequePeru)-> DatoTotal

#Definimos tipo de datos de la base 

names(DatoTotal)

#Fecha : tipo de datos caracter (chr)

str(DatoTotal$fecha) 

#Pais : tipo de datos caracter (chr)

str(DatoTotal$pais) 

#Ingresos : tipo de datos numericos (num)

str(DatoTotal$ingresos)

#Costo tipo de datos numericos (num)

str(DatoTotal$costos) 

#Porcentaje Mujeres tipo de datos numericos (num)

str(DatoTotal$porcentajeMujeres)

#Exportaciones tipo de datos numericos(num) 

str(DatoTotal$exportaciones) 

#Importaciones tipo de datos numericos (num)

str(DatoTotal$importaciones) 

#Endeudamiento tipo de datos numericos (num)

str(DatoTotal$endeudamiento) 

#Morosidad tipo de datos numericos (num )
str(DatoTotal$morosidad) 

#Rservas tipo de datos numericos (num)

str(DatoTotal$reservas) 

#Spread tipo de datos numericos (num)
str(DatoTotal$spread)

#Tasa de interes tipo de datos (num)

str(DatoTotal$tasa_interes)

#Tamano tipo de datos caracter (chr)

str(DatoTotal$tamanio)

#Pregunta 3 

#Cantidad de observaciones Chile Peru

cantObs <- dplyr::select(DatoTotal, pais)

Obs <- "chilevsperu"

if (Obs == "chilevsperu"){
  Chile <- paged_table(DatoTotal %>% 
                         filter(pais == "chile")) %>% count(pais)
  Peru <- paged_table(DatoTotal %>% 
                        filter(pais == "peru")) %>% count(pais)
  print(paste(Chile,"tiene en total observaciones"))
  print(paste("Perú tiene",Peru[-1],"observaciones"))
  chileGran <- filter(DatoTotal, tamanio %in% c("grande"), pais %in% c("chile")) %>% count(tamanio)
  chileMedia <- filter(DatoTotal, tamanio %in% c("mediana"), pais %in% c("chile")) %>% count(tamanio)
  chileMicro <- filter(DatoTotal, tamanio %in% c("micro"), pais %in% c("chile")) %>% count(tamanio)
  chilePeque <- filter(DatoTotal, tamanio %in% c("pequena"), pais %in% c("chile")) %>% count(tamanio)
  peruGran <- filter(DatoTotal, tamanio %in% c("grande"), pais %in% c("peru")) %>% count(tamanio)
  peruMedia <- filter(DatoTotal, tamanio %in% c("mediana"), pais %in% c("peru")) %>% count(tamanio)
  peruMicro <- filter(DatoTotal, tamanio %in% c("micro"), pais %in% c("peru")) %>% count(tamanio)
  peruPeque <- filter(DatoTotal, tamanio %in% c("pequena"), pais %in% c("peru")) %>% count(tamanio)
  print(paste("De estas",Chile[-1],"observaciones que se tiene ,",chileGran[-1], "son de tamano grande,", 
              chileMedia[-1], "son de tamano mediano,", chileMicro[-1], "son de tamano micro y",
              chilePeque[-1],"son de tamano pequeno"))
  print(paste("Por otra parte,",Peru[-1],"observaciones que tiene Peru:",peruGran[-1], "son de tamano grande,", 
              peruMedia[-1], "son de tamano mediano,", peruMicro[-1], "son de tamano micro y",
              peruPeque[-1],"son de tamano pequeño"))           
}


#Pregunta 4 
#Recolectamos datos de la base en cuanto a ingresos por explotacion

ingreChile <- DatoTotal %>% select(ingresos, pais) %>%
  filter(pais == "chile") 
ingrePeru <- DatoTotal %>% select(ingresos, pais) %>%
  filter(pais == "peru")
ingreColombia <- DatoTotal %>% select(ingresos, pais) %>%
  filter(pais == "colombia") 

#Se ordenan los datos recolectados 

ingresos <- c(sum(ingrePeru[1]), sum(ingreColombia[1]),sum(ingreChile[1]))
numingre <- function(ingresos){
  if(ingresos [1] > ingresos[2]){
    tiem <- ingresos[1]
    ingresos [1] <- tiem[2]
    ingresos[2] <- tiem
  } 
  if(ingresos[2] > ingresos[3]){
    tiem <- ingresos[2]
    ingresos[2] <- ingresos[3]
    ingresos[3] <- tiem
  } 
  if(ingresos[1] > ingresos[2]){
    tiem <- ingresos[1]
    ingresos[1] <- ingresos[2]
    ingresos[2] <- tiem
  } 
  if(ingresos[2] > ingresos[3]){
    tiem <- ingresos[2]
    ingresos[2] <- ingresos[3]
    ingresos[3] <- tiem
  }
  return(ingresos)
}

#se obtiene el resultado 

ordeningre <- numingre(ingresos)
print(paste("De acuerdo al periodo 2012-2017 de explotacion, Chile es el pais  con menor ingresos por concepto de explotacion",
            ordeningre[1], "Peru recaudo un total de ",ordeningre[2],", donde el pais con mayores ingresos por concepto de explotacion fue colombia",
            ordeningre[3]))

#Pregunta 5
# Se debe hacer Chile multiplique la tasa de interes por 0,1 
#cuando sea Peru le sume 0,3 
# si es Colombia divida por 10

tintChile <-  DatoTotal %>% filter(pais == "chile") %>% 
  mutate(tasaInteresNueva = tasa_interes * 0.1)
tintColombia <-  DatoTotal %>% filter(pais == "colombia") %>% 
  mutate(tasaInteresNueva = tasa_interes / 10)
tintPeru <-  DatoTotal %>% filter(pais == "peru") %>% 
  mutate(tasaInteresNueva = tasa_interes + 0.3)



DatoTotal <- rbind(tintChile,tintColombia,tintPeru)


#Pregunta 6 

#Reemplace en la columna exportaciones con 1 cuando es mayor a 2,1
#con un 2 cuando es menor 2,1
#un 3 cuando es igual a 2,1, redondee al primer decimal la variable

exportaUno <-  DatoTotal %>% filter(round(DatoTotal[6], 1) > 2.1) %>% 
  mutate(exportaciones = 1)
exportaDos <-  DatoTotal %>% filter(round(DatoTotal[6], 1) < 2.1) %>% 
  mutate(exportaciones = 2)
exportaTres <-  DatoTotal %>% filter(round(DatoTotal[6], 1) < 2.1) %>% 
  mutate(exportaciones = 3)


reexport <- rbind(exportaDos,exportaTres)

#Pregunta 7
#Gráfique algunas variables seleccionadas
#las cuales puedan responder a una pregunta que se haga con respecto a los datos


ggplot(reexport,aes(x=ingresos ,y=endeudamiento))+
  geom_boxplot(fill="cyan")

#  Según el gráfico anterior entregado, se puede ver una concentracion en cuanto 
# mayor a la media, donde se puede decir
#que el endeudamiento se situa sobre el 50% sobre ingresos 


