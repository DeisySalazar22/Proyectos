# PROYECTO 2

#Lectura archivo
setwd("C:/Users/Deisy\ Salazar\ Parra/Desktop/Info\ Deisy/Maestría\ ISC/Materias/III\ Semestre/Inferencia")
CapHosp <- read.csv("./Capacidad_Hospitalaria_de_Risaralda.csv")

# 1. Tome un municipio de Risaralda y encuentre la media en camas 
CapHosp
levels(CapHosp$tipocapacidad)
Muni <- subset(CapHosp, CapHosp$mpio == "MARSELLA"); Muni
Camas <- subset(Muni,Muni$tipocapacidad=="CAMAS"); Camas

mean(Muni$cantidadcapacidad);mean
mean(Camas$cantidadcapacidad);mean

# 2. El municipio seleccionado tiene una capacidad hospitalaria dentro de lo esperado respecto al promedio del departamento?

# Capacidad hospitalaria, en camas, del departamento