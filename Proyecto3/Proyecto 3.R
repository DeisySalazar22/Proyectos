# PROYECTO 3 - CAPTURAS 

# Carga de archivo
dir_ar <- "D:/Escritorio/Maestria/Inferencia\ Estadistica"
#dir_ar <- "C:/Users/Deisy\ Salazar\ Parra/Desktop/Info\ Deisy/Maestría\ ISC/Materias/III\ Semestre/Inferencia"
setwd(dir_ar)
info_capturas <- read.csv("Capturas_2018.csv")
info_capturas <- subset(info_capturas, info_capturas$Fecha != 'TOTAL')

# Delitos mas capturados en el año 2018
delitosGeneral <- aggregate(info_capturas$Cantidad, by=list(Delito=info_capturas$Delito.Captura), FUN=sum)
head(delitosGeneral[order(delitosGeneral$x, decreasing=TRUE), ], n=5)

# Delitos de hombres capturados en el año 2018 
capturasHombre <- subset(info_capturas, info_capturas$Sexo == 'MASCULINO')
delitosHombre <- aggregate(capturasHombre$Cantidad, by=list(Delito=capturasHombre$Delito.Captura), FUN=sum)
head(delitosHombre[order(delitosHombre$x, decreasing=TRUE), ], n=5)

# Delitos de mujeres capturados en el año 2018 
capturasMujer <- subset(info_capturas, info_capturas$Sexo == 'FEMENINO')
delitosMujer <- aggregate(capturasMujer$Cantidad, by=list(Delito=capturasMujer$Delito.Captura), FUN=sum)
head(delitosMujer[order(delitosMujer$x, decreasing=TRUE), ], n=5)

# Analisis de captura por causa de Violencia Intrafamiliar
capturas_violencia_intrafamiliar <- subset(info_capturas, info_capturas$Delito.Captura == 'ARTÃCULO 229. VIOLENCIA INTRAFAMILIAR')

capturas_violencia_intrafamiliar$Fecha <- capturas_violencia_intrafamiliar$Hora <- NULL
capturas_violencia_intrafamiliar$Edad <- capturas_violencia_intrafamiliar$Barrio <- NULL
capturas_violencia_intrafamiliar$Zona <- capturas_violencia_intrafamiliar$Clase.de.sitio <- NULL
capturas_violencia_intrafamiliar$Estado.civil <- capturas_violencia_intrafamiliar$Clase.de.empleado <- NULL
capturas_violencia_intrafamiliar$ProfesiÃ³n <- capturas_violencia_intrafamiliar$Escolaridad <- NULL
capturas_violencia_intrafamiliar$CÃ³digo.DANE <- capturas_violencia_intrafamiliar$Delito.Captura <- NULL

capturas_violencia_intrafamiliar$DÃ.a <- factor(capturas_violencia_intrafamiliar$DÃ.a, levels=c("Lunes","Martes","MiÃ©rcoles","Jueves","Viernes","SÃ¡bado","Domingo"), ordered = TRUE)
