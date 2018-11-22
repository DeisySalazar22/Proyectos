# Taller 1 - Punto 2  
getwd() #Directorio actual
directorio = "C:\\Users\\Deisy Salazar Parra\\Desktop\\Info Deisy\\Maestría ISC\\Materias\\III Semestre\\Inferencia"
setwd(directorio)
ParqueAutomotor <- read.csv("Parque_Automor_Departamento_de_Risaralda.csv")

head(ParqueAutomotor)
levels(ParqueAutomotor$CLASE)

buses <- subset(ParqueAutomotor, ParqueAutomotor$CLASE == "BUS" | ParqueAutomotor$CLASE == "BUSETA" | ParqueAutomotor$CLASE == "MICRO BUS");buses

# DISTRIBUCIÓN NORMAL 
#Pregunta 1. Cual es la probabilidad que hayan buses con mas de 20 años de vida

modelo_buses <- buses$MODELO

#Caja de bigotes
Q1 <- quantile(modelo_buses)[2]
Q3 <- quantile(modelo_buses)[4]

r_iqr <- 1.5*IQR(modelo_buses); r_iqr

#Limite inferior
lim_inf <- Q1 - r_iqr
lim_inf

lim_sup <- Q3 + r_iqr
lim_sup

#buses a analizar, quitar valores atípicos
buses_rango <- modelo_buses[modelo_buses >= lim_inf & modelo_buses <= lim_sup];buses_rango

#Tabla de frecuencias modelo de buses
frec_model <- table(buses_rango);frec_model

#Promedio
prom_buses <- mean(buses_rango);prom_buses

#Desviacion estandar
ds_buses <- sd(buses_rango);ds_buses

#promedio acumulado
prob_buses <- prop.table(frec_model);prob_buses
prob_acum <- cumsum(prob_buses);prob_acum["1998"]

#La probabilidad de que haya buses con mas de 20 años de vida es del 64%

autos <- subset(ParqueAutomotor, ParqueAutomotor$CLASE == "AUTOMOVIL" | ParqueAutomotor$CLASE == "CAMIONETA" | ParqueAutomotor$CLASE == "CAMPERO");autos

#2. Cuál es la probabilidad que el siguiente bus a abordar sea  modelo 2013 o superior?

#promedio de edad
prom_buses

ppois(2013, lambda = prom_buses, lower.tail = FALSE)

# La probabilidad de abordar un bus con modelo 2013 o superior es del 32%

#3. Determinar la probabilidad de que una muestra de 50 buses la media sea superior al modelo 1998
xb <- 1998 #valor buscado
prom_buses #media
ds_buses # desviación
n <- 50 #muestra

z <- (xb - prom_buses)/(ds_buses/sqrt(n));z
1-pnorm(z)

# La probabilidad de que la media seaa superiore a 1998 es de 0.87%

#DISTRIBUCIÓN BINOMIAL

head(ParqueAutomotor)

# Cuál es la probabilidad de que un vehículo tenga registrada novedad
#Total vehículos en Parque Automotor
total_vehiculos <- nrow(ParqueAutomotor);total_vehiculos

# Vehículos que tienen novedad
vehiculo_novedad <- ParqueAutomotor[ParqueAutomotor$CON.NOVEDAD == 'X',];vehiculo_novedad
total_vehiculo_novedad <- nrow(vehiculo_novedad);total_vehiculo_novedad

#Probabilidad de tener novedad
prob_novedad <- total_vehiculo_novedad / total_vehiculos; prob_novedad
prob_no_novedad <- 1 - prob_novedad;prob_no_novedad 

#1. Cuál es la probabilidad de que la proporción muestral de tener novedad exceda 0.18 en una muestra de tamaño n = 1000

prob_novedad #probabilidad media
n <- 1000
prob_esperada <- 0.18 #valor a encontrar
ds <- sqrt ((prob_novedad*(1-prob_novedad))/n);ds

z <- (prob_esperada - prob_novedad)/ds; z

#Probabilidad de excederse P(p>0.18) = p(z>2.00)= 1- p(z>2.00)
pnorm(z)
1 - pnorm(z)
# La probabilidad de excederse a la media de 0.18 es del 2%

#2. Hallar la probabilidad que la proporcion muestral sea entre 0.16 y 0.18
prob_esperada2 <- 0.16 #valor a encontrar
z2 <- (prob_esperada2 - prob_novedad)/ds; z2

#Probabilidad de excederse P(p>0.16) = p(z>0.26)= 1- p(z>0.26)
pnorm(z2)
1 - pnorm(z2)

prob_rango <- (1 - pnorm(z2)) - (1 - pnorm(z)); prob_rango

#La probabilidad de excederse a la median entre 0.16 y 0.18 es del 37%

#3.Cuál es la probabilidad de que la proporción muestral de no tener novedad exceda 0.9 en una muestra de tamaño n = 3000
prob_no_novedad #probabilidad media
m <- 100
prob_esperada3 <- 0.9 #valor a encontrar
ds2 <- sqrt ((prob_no_novedad*(1-prob_no_novedad))/m);ds2

z3 <- (prob_esperada3- prob_no_novedad)/ds2; z3

#Probabilidad de excederse P(p>0.9) = p(z>1.56)= 1- p(z>1.56)
pnorm(z3)
1 - pnorm(z3)
# La probabilidad de excederse a la media de 0.9 es del 6% aproximadamente
