library(readxl)
library(stratification)
library(tidyverse)


rm(list=ls())

#Cargamos fichero

modelerData <- read_excel("//ficheros/Estudios_Marketing/ESTRATEGICO-OPERATIVO-CUSTOMER/OPERATIVO/Modeler/17. Modelo de datos Beneficiario/0. Fichero de Datos/GSD_muestra.xlsx")

#Preparamos la variable

modelerData$inmi_tot <-  (modelerData$nac_tot_ex/modelerData$nac_tot_po)*1000

head(modelerData$inmi_tot)

#Aplicamos función Dalenious Hodges

segment1 <- strata.cumrootf(modelerData$inmi_tot, n = length(modelerData$inmi_tot), Ls=5)


#Creamos un vector para guardar la segmentación

Segmento_inmig_total <- vector(length = length(modelerData$inmi_tot))

#Bucle para asignar el segmento en una nueva variable.

for (i in 1:length(modelerData$inmi_tot)){
  
  if(modelerData$inmi_tot[i]==0){
    Segmento_inmig_total[i] <- "NO"
  } 
    else if(min(modelerData$inmi_tot)<= modelerData$inmi_tot[i] & modelerData$inmi_tot[i] <=segment1$bh[1]) {
    Segmento_inmig_total[i] <- "A"
  } else if (segment1$bh[1]< modelerData$inmi_tot[i] & modelerData$inmi_tot[i]<= segment1$bh[2]) {
    Segmento_inmig_total[i] <- "B"
  } else if (segment1$bh[2]< modelerData$inmi_tot[i] & modelerData$inmi_tot[i]<= segment1$bh[3]){
    Segmento_inmig_total[i] <- "C"
  } else if (segment1$bh[3]< modelerData$inmi_tot[i] & modelerData$inmi_tot[i]<= segment1$bh[4]){
    Segmento_inmig_total[i] <- "D"
  } else {
    Segmento_inmig_total[i] <- "E"
  }
}

#Con las siguientes dos líneas podemos comprobar qué proporción es igual a la segmentación
#realizada en Excel.

####################################################
#iguales <- Segmento_inmig_total == GSD$segmento_aleman_inmi
#table(iguales)
####################################################

#Creamos la nueva columna

modelerData$Segmento_inmig_total <- Segmento_inmig_total
