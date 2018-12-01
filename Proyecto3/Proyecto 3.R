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
# Media
mean(delitosGeneral$x)
# Mediana
median(delitosGeneral$x)
# Desviacion Estandar
sd(delitosGeneral$x)
# Varianza
var(delitosGeneral$x)
# Resumen
summary(delitosGeneral$x)

# Delitos de hombres capturados en el año 2018 
capturasHombre <- subset(info_capturas, info_capturas$Sexo == 'MASCULINO')
delitosHombre <- aggregate(capturasHombre$Cantidad, by=list(Delito=capturasHombre$Delito.Captura), FUN=sum)
head(delitosHombre[order(delitosHombre$x, decreasing=TRUE), ], n=5)
# Media
mean(delitosHombre$x)
# Mediana
median(delitosHombre$x)
# Desviacion Estandar
sd(delitosHombre$x)
# Varianza
var(delitosHombre$x)
# Resumen
summary(delitosHombre$x)

# Delitos de mujeres capturados en el año 2018 
capturasMujer <- subset(info_capturas, info_capturas$Sexo == 'FEMENINO')
delitosMujer <- aggregate(capturasMujer$Cantidad, by=list(Delito=capturasMujer$Delito.Captura), FUN=sum)
head(delitosMujer[order(delitosMujer$x, decreasing=TRUE), ], n=5)
# Media
mean(delitosMujer$x)
# Mediana
median(delitosMujer$x)
# Desviacion Estandar
sd(delitosMujer$x)
# Varianza
var(delitosMujer$x)
# Resumen
summary(delitosMujer$x)

# Delitos de Risaralda en el año 2018 
capturasRisaralda <- subset(info_capturas, info_capturas$Departamento == 'RISARALDA')
delitosRisaralda <- aggregate(capturasRisaralda$Cantidad, by=list(Delito=capturasRisaralda$Delito.Captura), FUN=sum)
head(delitosRisaralda[order(delitosRisaralda$x, decreasing=TRUE), ], n=5)
# Media
mean(delitosRisaralda$x)
# Mediana
median(delitosRisaralda$x)
# Desviacion Estandar
sd(delitosRisaralda$x)
# Varianza
var(delitosRisaralda$x)
# Resumen
summary(delitosRisaralda$x)

# COMPARAR CON LA MEDIA DE COLOMBIA
# Generar consolidacion de las capturas por departamento por delito
delitosColombiaDepartamento <- aggregate(info_capturas$Cantidad, by=list(Departamento=info_capturas$Departamento, Delito=info_capturas$Delito.Captura), FUN=sum)

# Prueba de Hipótesis Sobre la cantidad de capturas de Risaralda contra el Promedio Nacional
# Delito Seleccionado: ARTÍCULO 376. TRÁFICO, FABRICACIÓN O PORTE DE ESTUPEFACIENTES
# Ha = Risaralda tiene una criminalidad mas alta al promedio de colombia
# Ho = Risaralda tiene una criminalidad igual o mas baja al promedio de colombia

media_risaralda = delitosRisaralda$x[delitosRisaralda$Delito== 'ARTÃ\u008dCULO 376. TRÃ\u0081FICO, FABRICACIÃ"N O PORTE DE ESTUPEFACIENTES']; media_risaralda

# Calculo del promedio nacional de capturas por delito
delitosColombia <- delitosColombiaDepartamento[delitosColombiaDepartamento$Delito == 'ARTÃ\u008dCULO 376. TRÃ\u0081FICO, FABRICACIÃ"N O PORTE DE ESTUPEFACIENTES', ]
# Media
media_colombia <- mean(delitosColombia$x); media_colombia
# Mediana
median(delitosColombia$x)
# Desviacion Estandar
desviacion_colombia <- sd(delitosColombia$x); desviacion_colombia
# Varianza
var(delitosColombia$x)
# Resumen
summary(delitosColombia$x)

# Desviación estándar del depto
# Ha = Risaralda tiene una criminalidad mas alta al promedio de colombia
alpha <- 0.05 # Certeza del 95%

n <- length(delitosColombia$Departamento); n

# Z = (media_colombia - media_departamento) / (desviación_estandar / raíz del total)
z <- (media_colombia - media_risaralda) / (desviacion_colombia / sqrt(n));z #z=0.6928

z1 <- 1 - qnorm(alpha);z1 # z1 = 2.644

# Como Z = 0.6928 y es menor que Z1 = 2.644, entonces se rechaza la hipótesis alternativa 
# Se acepta la Ho, es decir, que Risaralda tiene una criminalidad igual o mas baja al promedio de colombia
