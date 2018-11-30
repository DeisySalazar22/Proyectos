# PROYECTO 3 - CAPTURAS 

# Carga de archivo
dir_ar <- "C:/Users/Deisy\ Salazar\ Parra/Desktop/Info\ Deisy/Maestría\ ISC/Materias/III\ Semestre/Inferencia"
setwd(dir_ar)
info_capturas <- read.csv("Capturas_2018.csv")

#install.packages("RWeka")
library(RWeka)
write.arff(info_capturas, file = "capturas.arff")

#Capturas para el departamento de Risaralda
capturas_risa <- subset(info_capturas, info_capturas$Departamento=='RISARALDA');capturas_risa
#write.arff(capturas_risa, file = "capturas_risa.arff")

# Eliminación columnas no deseadas
capturas_risa$Cantidad <- capturas_risa$CÃ³digo.DANE <- capturas_risa$CÃ³digo.DANE <- NULL
capturas_risa$Escolaridad <- NULL

# Capturas de Hombres
captu_hombres <- subset(capturas_risa, capturas_risa$Sexo == 'MASCULINO');captu_hombres

# Tabla de frecuencias de delitos hombres
frec_delito <- sort(table(captu_hombres$Delito.Captura), decreasing=T);frec_delito
frec_delito.get(0)
plot(frec_delito)


###########################################################

# Capturas de mujeres
captu_mujeres <- subset(capturas_risa, capturas_risa$Sexo == 'FEMENINO');captu_mujeres

# Tabla de frecuencias delitos mujeres
frec_del_fem <- table(captu_mujeres$Delito.Captura);frec_del_fem

