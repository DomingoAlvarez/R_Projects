library(geosphere)
library(readxl)
library(dplyr)
library(ggmap)
library(xlsx)



#register_google(key = "mQkzTpiaLYjPqXQBotesgif3EfGL2dbrNVOrogg") (Esto es una key falsa)

Codigo_Postal <- read_excel("Z:/ESTRATEGICO-OPERATIVO-CUSTOMER/OPERATIVO/Modeler/17. Modelo de datos Beneficiario/0. Fichero de Datos/Codigo_Postal_Google.xlsx")

#Necesitamos las coordenadas de los cp según Googleapis. Para ello primero debemos obtener una key en google
#Si buscamos en la documentación de geocode, podemos ir a la web https://cloud.google.com/docs/authentication/api-keys#securing_an_api_key
#obtener una key y una vez registrada podremos usar geocode("CP + Localidad + Provincia") para obtener las coordenadas

#Creamos una matriz vacía con 4 columnas y tantas filas como Códigos Postales

x <- matrix(ncol = 4, nrow = dim(Codigo_Postal)[1])

#Recorremos un bucle por cada CP + Localidad y guardamos las coordenadas en nuestra matriz.

for(i in 1:dim(Codigo_Postal)[1]){
  
  concat <- paste(Codigo_Postal$poblacion[i],", ", "CP= ",Codigo_Postal$codigopostalid[i],", ",Codigo_Postal$provincia[i] )
  coord <- geocode(concat)
  x[i,1] <- Codigo_Postal$codigopostalid[i]
  x[i,2] <- Codigo_Postal$poblacion[i]
  x[i,3] <- coord$lat[1]
  x[i,4] <- coord$lon[1]
  
}

#Creamos una matriz con los datos de Google
colnames(x) <- c("CP", "Municipio", "GoogleLat", "GoogleLon")

x <- as_tibble(x)

#Cruzamos los datos de google con nuestro excel
Codigo_Postal_Google <- inner_join(Codigo_Postal, x, by=c("codigopostalid"="CP","poblacion"="Municipio"))


as.double(Codigo_Postal_Google$GoogleLat)
as.double(Codigo_Postal_Google$GoogleLon)

#Creamos un nuevo excel con todo
write_excel_csv(Codigo_Postal_Google, path  = "Z:/ESTRATEGICO-OPERATIVO-CUSTOMER/OPERATIVO/Modeler/17. Modelo de datos Beneficiario/0. Fichero de Datos/Codigo_Postal_Google.csv")

#
#El siguiente código me va a dar la matriz diagonal con el cp más cercano
#He usado el dataframe modelerData que nos da Modeler

modelerData <- read_excel("//ficheros/Estudios_Marketing/ESTRATEGICO-OPERATIVO-CUSTOMER/OPERATIVO/Modeler/17. Modelo de datos Beneficiario/0. Fichero de Datos/Codigo_Postal_Google.xlsx")

LonLat <- cbind(as.double(modelerData$GoogleLon),as.double(modelerData$GoogleLat))

matriz <- distm(LonLat[1:10,], fun =distGeo)

min<- apply(matriz, 1, function(x) order(x, decreasing=F)[2])


newdata <- cbind(modelerData[1:10,], modelerData[min,], apply(matriz, 1, function(x) sort(x, decreasing=F)[2]))

colnames(newdata) <- c(colnames(modelerData),'codigopostalid_mascercano','poblacionid_mascercano','poblacion_mascercano','provinciaid_mascercano','provincia_mascercano','ineid_mascercano','lat_mascercano','lon_mascercano','CCAA_mascercano','GoogleLat_mascercano','GoogleLon_mascercano', "distancia_metros")



#Dado el resultado de caracterización persona y una tabla con los hospitales públicos
#y privados. 
#

rm(list=ls())

modelerData <- read_excel("//ficheros/Estudios_Marketing/ESTRATEGICO-OPERATIVO-CUSTOMER/OPERATIVO/Modeler/17. Modelo de datos Beneficiario/0. Fichero de Datos/Codigo_Postal_Google.xlsx")


hospitales_publicos <- read_excel("//ficheros/Estudios_Marketing/ESTRATEGICO-OPERATIVO-CUSTOMER/OPERATIVO/Modeler/17. Modelo de datos Beneficiario/0. Fichero de Datos/GSD_2019 Y CENTROS MEDICOS/hospitales_publicos.xlsx")


hospitales_privados <- read_excel("//ficheros/Estudios_Marketing/ESTRATEGICO-OPERATIVO-CUSTOMER/OPERATIVO/Modeler/17. Modelo de datos Beneficiario/0. Fichero de Datos/GSD_2019 Y CENTROS MEDICOS/hospitales_privados.xlsx")

#
#
#
#Hospitales Públicos
vec_postal <- vector(length = dim(modelerData)[1])  
vec_distancia <- vector(length = dim(modelerData)[1])  
for(i in 1926:dim(modelerData)[1]){
  vec <- vector(length = dim(hospitales_publicos)[1])  
  for(j in 1:dim(hospitales_publicos)[1]){
    
      vec[j] <- distGeo(c(modelerData$GoogleLon[i],modelerData$GoogleLat[i]),c(hospitales_publicos$lon[j],hospitales_publicos$lat[j]))
      
  }
  vec_postal[i] <- order(vec, decreasing=F)[1]
  vec_distancia[i] <- min(vec)
  print(i)
}


distkm <- vec_distancia/1000

modelerData <- cbind(modelerData,hospitales_publicos[vec_postal,])
modelerData <- cbind(modelerData,distkm)
  
write.csv2(modelerData, file = "//ficheros/Estudios_Marketing/ESTRATEGICO-OPERATIVO-CUSTOMER/OPERATIVO/Modeler/17. Modelo de datos Beneficiario/0. Fichero de Datos/GSD_2019 Y CENTROS MEDICOS/CP_hosp_publicos.csv")

#
#
#
#Hospitales Privados
vec_postal <- vector(length = dim(modelerData)[1])  
vec_distancia <- vector(length = dim(modelerData)[1])  
for(i in 1:dim(modelerData)[1]){
  vec <- vector(length = dim(hospitales_privados)[1])  
  for(j in 1:dim(hospitales_privados)[1]){
    
    vec[j] <- distGeo(c(modelerData$GoogleLon[i],modelerData$GoogleLat[i]),c(hospitales_privados$lon[j],hospitales_privados$lat[j]))
    
  }
  vec_postal[i] <- order(vec, decreasing=F)[1]
  vec_distancia[i] <- min(vec)
  print(i)
}


distkm <- vec_distancia/1000

modelerData <- cbind(modelerData,hospitales_privados[vec_postal,])
modelerData <- cbind(modelerData,distkm)

write.csv2(modelerData, file = "//ficheros/Estudios_Marketing/ESTRATEGICO-OPERATIVO-CUSTOMER/OPERATIVO/Modeler/17. Modelo de datos Beneficiario/0. Fichero de Datos/GSD_2019 Y CENTROS MEDICOS/CP_hosp_privados.csv")

