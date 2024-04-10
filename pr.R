##Instalacion y carga de las Librerias utilizadas
install.packages("raster")
install.packages("sf")

library(readxl)
library(RSQLite)
library(stringr)
library(tidyverse)
library(dplyr)
library(raster)
library(sf)

###Importacion de los archivos archivos excel a R
municipios <-readxl::read_xlsx('C:/Users/Juan/Downloads/Prueba técnica Analista de datos - ADRES/Municipios.xlsx')
df <- as.data.frame(municipios)
prestadores <-readxl::read_xlsx('C:/Users/Juan/Downloads/Prueba técnica Analista de datos - ADRES/Prestadores.xlsx')
df2 <- as.data.frame(prestadores)
rm("municipios","prestadores")

###Limpieza de las  bases de datos 
#Eliminamos los caracteres NO alfabeticos
df$Municipio <- str_replace_all(df$Municipio,"[^[a-z A-ZáéíóúÁÉÍÓÚ]]","")
df$Departamento <- str_replace_all(df$Departamento,"[^[a-z A-ZáéíóúÁÉÍÓÚ]]","")
#Reemplazamos los espacios dobles por espacios sencillos
df$Municipio <- str_replace_all(df$Municipio,"\\s{2}", " ")
df$Departamento <- str_replace_all(df$Departamento,"\\s{2}", " ")
#Eliminamos los espacios al inicio de las variables
df$Municipio <- str_replace_all(df$Municipio,"^ *","")
#Eliminamos los espacios al final de la cadena de caracteres
df$Municipio <- str_replace_all(df$Municipio," +$","")
#Cambiamos las cadenas de texto a MAYUSCULAS
df$Municipio <- toupper(df$Municipio)
df$Departamento <- toupper(df$Departamento)
df[149,3] <-  "BOGOTÁ"





###Carga de los dataframes en SQLite
con <- dbConnect(RSQLite::SQLite(),"C:/Users/Juan/Downloads/Prueba técnica Analista de datos - ADRES/dtbmunicipios.db")
dbWriteTable(con,"dtbmunicipios",df)
dbWriteTable(con,"dtbprestadores",df2)
dbListTables(con)

##Raster "Departamentos de Colombia"
colombia_pais <- getData(name = "GADM", country = "COL", level = 1)
prueba <- st_as_sf(colombia_pais)

##Agregamos Bogotá del raster "Municipios de Colombia" al raster "Departamentos de Colombia"
DC <- getData(name = "GADM", country = "COL", level = 2) %>% st_as_sf() %>% filter_all(any_vars(. %in% c("Santafé de Bogotá"))) 
DC$GID_1 <- NULL
DC$NAME_1<- NULL
DC$NL_NAME_1<- NULL
names(DC) <- names(prueba)
finaldf <- rbind(prueba,DC)
finaldf


##Conteo Número de Prestadores por Departamento
datos <- df2 %>% count(depa_nombre)
datos[36,1]<-"Valle del Cauca" ##Corrección Mayuscula
datos[6,1] <-  "Santafé de Bogotá"
###Grafica Número de Prestadores por Departamento
finaldf %>% 
  rename(depa_nombre = NAME_1) %>% 
  left_join(y = datos, by = "depa_nombre") %>% 
  ggplot(aes(fill = n)) +
  labs(title = "Numero de prestadores de Salud por Departamento",fill= "Prestadores")+
  geom_sf(color = "black")+
  scale_fill_gradientn(colours = c("#3b1506","#cc3232", "#db7b2b", "#e7b416", "#99c140","#2dc937"))



###Query Número de Prestadores en las principales ciudades
main_cities <- dbGetQuery(con,'SELECT clpr_nombre,muni_nombre FROM dtbprestadores WHERE muni_nombre in ("BOGOTÁ","MEDELLÍN", "CALI", "CARTAGENA", "BARRANQUILLA");')

####Histograma Tipos de Prestadores en las principales ciudades
ggplot(main_cities, aes(x =muni_nombre ,fill=clpr_nombre)) +
  geom_bar()+
  scale_fill_manual(values=c("blue","green","red","purple"),labels=c("IPS","Otros","Profesional Independiente","Transporte de Pacientes"))+
  labs(
    title = "Tipos de Prestadores del Servicio Salud \n en las Principales Ciudades",
    x = "Ciudad", y = "Porcentaje",
    fill = "Tipo de Prestador"
  )

###Query Número de Prestadores en cada municipio
all_cities <- dbGetQuery(con,'SELECT muni_nombre, COUNT(*) AS numero_prestadores
FROM dtbprestadores
GROUP BY muni_nombre
ORDER BY muni_nombre;')
colnames(all_cities) <- c("Municipio","Prestadores")
###Query numero de habitantes en cada municipio
poblacion <- dbGetQuery(con,'SELECT Municipio, Poblacion FROM dtbmunicipios')
data<- left_join(all_cities, poblacion, by = "Municipio")

###Grafica Prestadores vs Numero de habitantes para todos los municipios
options(scipen = 999)
ggplot(data,aes(x=Poblacion,y=Prestadores))+
  geom_point(aes(size=Poblacion,color=Poblacion), show.legend = FALSE)+ 
  scale_color_gradientn(colours = c("#3b1506","#cc3232", "#db7b2b", "#e7b416", "#99c140","#2dc937"))+
  scale_x_continuous(labels = scales::comma)+
  labs(title = "Relacion Número de Prestadores de Salud vs Poblacion\n para todos los municipios de Colombia")+
  geom_label(data=subset(data,Poblacion>2000000),aes(label=Municipio),nudge_y = -830)+
  geom_label(data=subset(data,2000000>Poblacion & Poblacion>1000000),size=3,aes(label=Municipio),nudge_x = 500000)+
  geom_smooth(method = "lm", se = FALSE)


###Regresion lineal para hallar la Media de habitantes por prestador
summary(lm(formula=Poblacion ~ Prestadores,data))
