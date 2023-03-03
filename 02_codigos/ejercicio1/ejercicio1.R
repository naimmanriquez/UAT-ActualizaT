# Capas shp
## https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=889463807469


# Abrimos las paqueterías con un sólo comando:
## La herramienta pacman sirve para llamar más herramientas
library("pacman")

## La funcion p_load sirve para llamar mas herramientas que serviran para nuestro analisis
## Algunas de esas son las básicas para hacer un analisis estadistico:

## haven sirve para llamar archivos desde otros software como stata o spss
## readr sirve para leer archivos propios de rstudio
## ggplot2 sirve para crear graficos
## tidyverse sirve para manipular, transformar datos
## lmtest sirve para pruebas de hipótesis en el analisis de regresion
## DescTools sirve para manipulacion de datos
## ineq sirve para hacer curvas de lorenz que sirven para el analisis de la pobreza
## expss es una herramienta para generar tablas, estadísticas que integra spss
## MASS alberga funciones del libro Modern And Applied Statistics
## RColorBrewer sirve para generar paletas de colores en los graficos
## Foreign es una herramienta para cargar archivos de extensiones como kml, pdf, etc.
## knitr sirve para generar reportes dinamicos. 

p_load(ineq, haven, readr, readxl, ggplot2, tidyverse, expss, 
       DescTools, lmtest, MASS, knitr, foreign, RColorBrewer)

##############
#P herramientas y paqueterías para mapear
## La herramienta sf sirve para utilizar vectores con informacion espacial
## La herramienta ggspatial sirve para mapear objetos espaciales

library("sf")
library("ggspatial")
library("colorspace")

##############
# Paso #1 Abrir capas para mapas, la cual proviene del archivo .shp
##############
#AGEBs de tamaulipas
ageb_tamaulipas <- st_read("28a.shp")

#Crear variable para filtrar por municipios de interés
ageb_tamaulipas$mun <- substr(ageb_tamaulipas$CVEGEO, 3, 5)

#Filtrar / cortar para tener solo las ageb del municipio de Tampico
ageb_tampico <- ageb_tamaulipas %>%    
  filter(mun == "038")

## Mapa de tampico
plot(ageb_tampico)

## Abrir los datos del censo censo
RESAGEB28 <- read.csv("RESAGEB28.csv")

# Filtrar por municipio
ageb_datos <- RESAGEB28 %>%    
  filter (MUN == "38")

# Filtrar solo para el total de la ageb urbana
ageb_urbanas <- ageb_datos %>% 
  filter (NOM_LOC == "Total AGEB urbana")

ageb_urbanas <- rename(ageb_urbanas, CVE_AGEB = AGEB)

# Ahora vamos a hacer Union de bases de datos (la capa del mapa con los datos del censo)

bd_final <- left_join(ageb_tampico, ageb_urbanas)

# Convertimos la variable a numerico, muchas veces el INEGI pone las variables 
## en modo caracter, lo que genera problemas a la hora de crear graficos o estadisticos
## esa es la razon de convertir a numerico.
bd_final <- bd_final %>%
  mutate_at("POB65_MAS", ~as.numeric(.)) 

## Generamos nuestro primer mapa y lo guardamos como un objeto
mapa <- ggplot(bd_final) +
  geom_sf(aes(fill = POB65_MAS)) +
  ggtitle("Población de 65 y más") +
  scale_fill_gradientn(colors=brewer.pal(name="Oranges", n=6)) + theme_void()

## Llamamos para ver el mapa
mapa

## Creamos otro mapa
mapa2 <- ggplot(bd_final) +
  geom_sf(aes(fill = POB65_MAS)) +
  labs(title = 'Población por AGEB, 2020',
       subtitle = "Población adulta en Tampico por Área Geoestadistica Base.",
       caption='Censo de Población y Vivienda - INEGI || CETH',
       x = NULL,
       y = NULL) +
  scale_fill_gradientn(colors=brewer.pal(name="Oranges", n=6)) + theme_void()

mapa2

## mas opciones
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
class(pal)

plot(bd_final["POB65_MAS"], 
     main = "Poblacion aduta en Tampico", 
     breaks = "quantile", nbreaks = 7,
     pal = pal)


# mas con leaflet
# https://rstudio.github.io/leaflet/
# Leaflet es una librería utilizada para la publicación de mapas en la web.

library(leaflet) 

# reproject
tampico_leaflet <- st_transform(bd_final, 4326)

leaflet(tampico_leaflet) %>%
  addPolygons()

pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)

p_popup <- paste0("<strong>Poblacion: </strong>", bd_final$POB65_MAS)

leaflet(tampico_leaflet) %>%
  addPolygons(
    stroke = FALSE, # remove polygon borders
    fillColor = ~pal_fun(POB65_MAS), # set fill color with function from above and value
    fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
    popup = p_popup)  # add popup

leaflet(tampico_leaflet) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(POB65_MAS),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addTiles()