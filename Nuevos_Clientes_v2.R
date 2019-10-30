library("tidyverse")

rm(list = ls())

options(scipen = 9999)
#Cargar datos

datos <- read.csv2("//ficheros/Estudios_Marketing_Operativo/Modeler/33._Venta_Cruzada/2._Resultados/RNuevosClientes/salida1.csv",sep= ";")

#Todo a TRUE

datos$Nuevos_Clientes[1] <- "T"

#Separo los titulares con más de una póliza

titulares_0 <- datos %>% group_by(Cod_Titular_Poliza)%>% tally() %>% filter(n ==1) # una poliza

names(titulares_0) <- c("v1", "n")

titulares_1 <- datos %>% group_by(Cod_Titular_Poliza)%>% tally() %>% filter(n >1) # + Una Poliza

names(titulares_1) <- c("v2", "n")

datos_1 <- merge(datos, titulares_1, by.x = "Cod_Titular_Poliza", by.y = "v2")

datos_0 <- merge(datos, titulares_0, by.x = "Cod_Titular_Poliza", by.y = "v1")

#Ordenar datos_1

datos_1 <- datos_1[order(datos_1$Cod_Titular_Poliza, datos_1$Fecha_Emision_Poliza),]

#Format dates

emision <- as.POSIXct(datos_1$Fecha_Emision_Poliza, format= "%Y-%m-%d", tz="UTC")

anulacion <- as.POSIXct(datos_1$Fecha_Anulacion, format= "%Y-%m-%d", tz="UTC")

#La primera póliza a TRUE las demás a FALSE

for (t in 2:nrow(datos_1)) {
  
  print(cat("Esta es t: ",t,"\n"))
  
  if (datos_1$Cod_Titular_Poliza[t]==datos_1$Cod_Titular_Poliza[t-1]){
    
    datos_1$Nuevos_Clientes[t] <- "F"
    
  }
  
}

emision_na <- which(is.na(emision)==T)

for(b in 1:length(emision_na)){
  
  emision[emision_na[b]] <- emision[emision_na[b]-1]
}

#Defino vector con índices
vec <- vector(length = nrow(datos_1)) 
vec <- which(datos_1$Nuevos_Clientes == "T")

vec[length(vec)+1] <- nrow(datos_1)+1

#Creo una función para calcular el máximo de las anulaciones anteriores

max_anula <- function( q, w) {
  
  voc <- vector(length = q-w)
  
  for (m in w:(q-1)) {
    voc[m-w+1]<- anulacion[m]
    
  }
  
  maxi <- max(voc, na.rm = TRUE)
  
  return(maxi)
}

#Creo una función para calcular el máximo de las anulaciones anteriores considerando NA's

max_anula_na <- function( q, w) {
  
  voc <- vector(length = q-w)
  
  for (m in w:(q-1)) {
    voc[m-w+1]<- datos_1$Estado_Poliza[m]
    
  }
  
  maxi <- min(voc)
  
  return(maxi)
}

#Recorre las pólizas
s <- 1
for (h in vec){
  s <- s+1
  print(cat("Esta es h: ",h,"\n"))
  if(h==max(vec) | s == length(vec)+1){
    break
  }  
  else if (vec[s]-vec[s-1]==1){
    
    next}
  
  else {
    
    for (i in (h+1):(vec[s]-1)){
      
      if(emision[i]==emision[i-1]){
        
        datos_1$Nuevos_Clientes[i]<-datos_1$Nuevos_Clientes[i-1]
        
      } 
      
        else if (is.na(anulacion[i-1]==TRUE) | max_anula_na(i-1,h)==1){
        
        for (n in i:(vec[s]-1)) {
          
          datos_1$Nuevos_Clientes[i]<- "F" #Como hay una arriba "Activa" todos los de abajo "F"
        }
          
        break #rompe el bucle ya no tengo que mirar más pólizas de este titular
          
      } else if (emision[i]<=anulacion[i-1]){
        
        datos_1$Nuevos_Clientes[i]<- "F"
      }
      
      else if (as.numeric(emision[i])<= max_anula(i-1, h)){ #Compruebo que la fecha más reciente de las anulaciones anteriores
        #es mayor que la fecha de emision de la poliza
        datos_1$Nuevos_Clientes[i]<- "F" 
      } else 
      
       datos_1$Nuevos_Clientes[i]<- "T" 
          }  
  }
}

datos <- rbind(datos_1,datos_0)

write.csv2(datos, file= "//ficheros/Estudios_Marketing_Operativo/Modeler/33._Venta_Cruzada/2._Resultados/RNuevosClientes/La108NC.csv")

#write.csv2(datos, file= "C:/Users/alvarezp/Desktop/La108NC.csv")




q()
