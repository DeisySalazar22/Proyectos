# PROYECTO 2

#Lectura archivo
setwd("C:/Users/Deisy\ Salazar\ Parra/Desktop/Info\ Deisy/Maestría\ ISC/Materias/III\ Semestre/Inferencia")
CapHosp <- read.csv("./Capacidad_Hospitalaria_de_Risaralda.csv")

# 1. Tome un municipio de Risaralda y encuentre la media en camas 
CapHosp
levels(CapHosp$tipocapacidad)
Muni <- subset(CapHosp, CapHosp$mpio == "MARSELLA" & CapHosp$tipocapacidad == 'CAMAS'); Muni

mean(Muni$cantidadcapacidad);mean

#Respuesta: La media en camas del municipio de Marsella es de 3.333

# 2. El municipio seleccionado tiene una capacidad hospitalaria dentro de lo esperado respecto al promedio del departamento?

# Capacidad hospitalaria, en camas, del departamento

Dep <- subset(CapHosp, CapHosp$dpto == 'Risaralda' & CapHosp$tipocapacidad == 'CAMAS'); Dep

# 3. Tomar una muestra de 25 entidades, estimar la media (capacidad hospitalaria) con intervalo de confianza del 90%
# a. En general
# b. Municipios pequeños
# c. Área metropolitana

# 4.¿El número de salas en Santa Rosa está en el promedio de los municipios "pequeños" de Risaralda?

# 5. ¿Dada la variación en el número de ambulancias entre los municipios pequeños, la de Quinchía es igual a la departamental?