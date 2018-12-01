# PROYECTO 3 - CAPTURAS 

# Carga de archivo
#dir_ar <- "D:/Escritorio/Maestria/Inferencia\ Estadistica"
dir_ar <- "C:/Users/Deisy\ Salazar\ Parra/Desktop/Info\ Deisy/Maestría\ ISC/Materias/III\ Semestre/Inferencia"
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

# Analisis de captura por causa de Violencia Intrafamiliar
capturas_violencia_intrafamiliar <- subset(info_capturas, info_capturas$Delito.Captura == 'ARTÃCULO 229. VIOLENCIA INTRAFAMILIAR')

capturas_violencia_intrafamiliar$Fecha <- capturas_violencia_intrafamiliar$Hora <- NULL
capturas_violencia_intrafamiliar$Edad <- capturas_violencia_intrafamiliar$Barrio <- NULL
capturas_violencia_intrafamiliar$Zona <- capturas_violencia_intrafamiliar$Clase.de.sitio <- NULL
capturas_violencia_intrafamiliar$Estado.civil <- capturas_violencia_intrafamiliar$Clase.de.empleado <- NULL
capturas_violencia_intrafamiliar$ProfesiÃ³n <- capturas_violencia_intrafamiliar$Escolaridad <- NULL
capturas_violencia_intrafamiliar$CÃ³digo.DANE <- capturas_violencia_intrafamiliar$Delito.Captura <- NULL

capturas_violencia_intrafamiliar$DÃ.a <- factor(capturas_violencia_intrafamiliar$DÃ.a, levels=c("Lunes","Martes","MiÃ©rcoles","Jueves","Viernes","SÃ¡bado","Domingo"), ordered = TRUE)

datos <- aggregate(capturas_violencia_intrafamiliar$Cantidad, by=list(Departamento=capturas_violencia_intrafamiliar$Departamento,Municipio=capturas_violencia_intrafamiliar$Municipio, Dia = capturas_violencia_intrafamiliar$DÃ.a, Sexo=capturas_violencia_intrafamiliar$Sexo), FUN=sum)

dias <- aggregate(datos$x, list(Dia = datos$Dia), sum)
head(dias[order(dias$x, decreasing=TRUE), ], n=3)

departamento <- aggregate(datos$x, list(Departamento = datos$Departamento), sum)
head(departamento[order(departamento$x, decreasing=TRUE), ], n=3)


# ANALISIS DE DATOS
set.seed(1234)
indice = sample(2, nrow(capturas_violencia_intrafamiliar), replace = TRUE, prob = c(0.7, 0.3))
train.data <- capturas_violencia_intrafamiliar[indice == 1,]
test.data <- capturas_violencia_intrafamiliar[indice == 2,]

# Arbol de desicion con RPART
library(rpart)
myFormula <- DÃ.a ~ Departamento + Municipio + Sexo
captura_rpart <- rpart(myFormula, data = train.data, control = rpart.control(minsplit = 10))
table(predict(captura_rpart), train.data$DÃ.a)
print(captura_rpart)
plot(captura_rpart)
text(captura_rpart, use.n=T)

set.seed(1234)
capturas_violencia_intrafamiliar2 <- capturas_violencia_intrafamiliar
capturas_violencia_intrafamiliar2$DÃ.a <- NULL
kmeans.result <- kmeans(capturas_violencia_intrafamiliar2, 7)
table(capturas_risa$DÃ.a, kmeans.result$cluster)

bundesCluster <- kmeans(tabla[,c(2,4)], 3, nstart = 10)
bundesCluster


#-----------------------------------------------------------------
# Arbol de decision con RPART
library(rpart)
myFormula <- Dia ~ Departamento + x
captura_rpart <- rpart(myFormula, data = datos)
table(predict(captura_rpart), datos.Dia)
print(captura_rpart)
plot(captura_rpart)
text(captura_rpart, use.n=T)






install.packages("RWeka")
library(RWeka)
write.arff(info_capturas, file = "capturas.arff")


set.seed(1234)
indice = sample(2, nrow(capturas_risa), replace = TRUE, prob = c(0.7, 0.3)); indice
train.data <- capturas_risa[indice == 1,]
test.data <- capturas_risa[indice == 2,]

# Arbol de desicion con PARTY
install.packages("party")
library(party)
myFormula <- DÃ.a ~ Delito.Captura
captura_ctree <- ctree(myFormula, data = train.data)
table(predict(captura_ctree), train.data$DÃ.a)
print(captura_ctree)
plot(captura_ctree)
plot(captura_ctree, type= "simple")

table(predict(captura_ctree, newdata = test.data), test.data$DÃ.a)

# Arbol de desicion con RPART
library(rpart)
myFormula <- DÃ.a ~ Delito.Captura
captura_rpart <- rpart(myFormula, data = train.data, control = rpart.control(minsplit = 10))
table(predict(captura_rpart), train.data$DÃ.a)
print(captura_rpart)
plot(captura_rpart)
text(captura_rpart, use.n=T)

# Cluster con Kmeans
set.seed(1234)
capturas_risa2 <- capturas_risa
capturas_risa2$DÃ.a <- NULL
kameans.result <- kmeans(capturas_risa2, 7)
table(capturas_risa$DÃ.a, kmeans.result$cluster)

# Cluster con pamk
install.packages("fpc")
library(fpc)
pamk.result <- pamk(capturas_risa2)
# número de clusters
pamk.result$nc

table(pamk.result$pamobject$clustering, capturas_risa$DÃ.a)

# Cluster con pam
library(cluster)
pam.result <- pam(capturas_risa2, 7)
table(pam.result$clustering, capturas_risa$DÃ.a)
plot(pam.result)











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