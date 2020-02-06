library("tidyverse")
library("zoo")

rm(list = ls())

options(scipen = 9999)
#Cargar datos

datos <- read.csv2("//ficheros/Estudios_Marketing_Operativo/Modeler/1.BBDD/0._Ficheros_Origen_Fijos/Venta_Cruzada/RNuevosClientes/salida2.csv",sep= ";")

#Todo a FALSE

datos$Falsas_Altas[1] <- "F"

#Separo los titulares con más de una póliza

titulares_0 <- datos %>% group_by(Cod_Titular_Poliza)%>% tally() %>% filter(n ==1) # una poliza

names(titulares_0) <- c("v1", "n")

titulares_1 <- datos %>% group_by(Cod_Titular_Poliza)%>% tally() %>% filter(n >1) # + Una Poliza

names(titulares_1) <- c("v2", "n")

datos_1 <- merge(datos, titulares_1, by.x = "Cod_Titular_Poliza", by.y = "v2")

datos_0 <- merge(datos, titulares_0, by.x = "Cod_Titular_Poliza", by.y = "v1")

#Ordenar datos_1

datos_1 <- datos_1[order(datos_1$Cod_Titular_Poliza, datos_1$Nombre_Ramo, datos_1$Fecha_Emision_Poliza),]


#Format dates

emision <- as.POSIXct(datos_1$Fecha_Emision_Poliza, format= "%Y-%m-%d", tz="UTC")

anulacion <- as.POSIXct(datos_1$Fecha_Anulacion, format= "%Y-%m-%d", tz="UTC")

#La primera póliza a TRUE las demás a FALSE

for (t in 2:nrow(datos_1)) {
  
  print(cat("Esta es t: ",t,"\n"))
  
  if (datos_1$Cod_Titular_Poliza[t]==datos_1$Cod_Titular_Poliza[t-1]){
    
    datos_1$Falsas_Altas[t] <- "S"
    
  }
  
}

emision_na <- which(is.na(emision)==T)

for(b in 1:length(emision_na)){
  
  emision[emision_na[b]] <- emision[emision_na[b]-1]
}

#Defino vector con índices
vec <- vector(length = nrow(datos_1)) 
vec <- which(datos_1$Falsas_Altas == "F")

vec[length(vec)+1] <- nrow(datos_1)+1



#Recorre las pólizas
s <- 1
for (h in vec){
  s <- s+1 
  
  print(cat("Esta es h: ",h,"\n"))
  
  if(h==max(vec) | s == length(vec)+1){
    break
  }  
  
  else { for (i in h:(vec[s]-1)){
    
    print(cat("Esta es i: ",i,"\n"))
    
    if(i==max(vec)-1){
      break
    }  
    
    if(datos_1$Nombre_Ramo[i]==datos_1$Nombre_Ramo[i+1]){
   
      for (j in i:(vec[s]-1)){
        
        print(cat("Esta es j: ",j,"\n"))
        
        if(datos_1$Nombre_Ramo[j]!=datos_1$Nombre_Ramo[j+1] | j==max(vec[s])-1 ){
          
          print(cat("BREAK ",j,"\n"))
          break
          
        }
        
      if(emision[j+1]==emision[j]){
        
       datos_1$Falsas_Altas[j+1]<-datos_1$Falsas_Altas[j]
      }
       else if(is.na(abs(emision[j+1]-anulacion[i]))==T ){
         
          if(datos_1$Nombre_Ramo[j]==datos_1$Nombre_Ramo[j+1] & datos_1$Fecha_Emision_Poliza[j]==datos_1$Fecha_Emision_Poliza[j+1]){
            
            datos_1$Falsas_Altas[j+1]<-datos_1$Falsas_Altas[j]
          }
          else if(datos_1$Nombre_Ramo[j]==datos_1$Nombre_Ramo[j+1]){
            
            datos_1$Falsas_Altas[j+1]<-datos_1$Falsas_Altas[j]
          }
          next 
        } 
      
       else if(abs(12 * as.numeric((as.yearmon(emision[j+1]) - as.yearmon(anulacion[i]))))<=1){
         
          datos_1$Falsas_Altas[j+1]<-"T"
        }
       else if(datos_1$Falsas_Altas[j+1]!="T"){
         
        datos_1$Falsas_Altas[j+1]<-"F" 
        } 
      
    }
    }
    else datos_1$Falsas_Altas[i+1]<-"F"
    
  }
    
  }
  
  
} 
  


datos <- rbind(datos_1,datos_0)

write.csv2(datos, file= "//ficheros/Estudios_Marketing_Operativo/Modeler/1.BBDD/0._Ficheros_Origen_Fijos/Venta_Cruzada/RNuevosClientes/La108FA.csv")

#write.csv2(datos, file= "C:/Users/alvarezp/Desktop/La108FA.csv")




q()
