# Proyecto 1

setwd("C:/Users/Deisy\ Salazar\ Parra/Desktop/Info\ Deisy/Maestría\ ISC/Materias/III\ Semestre/Inferencia/Proyecto\ 1/InferenciaEstadisticaProyecto1")
spain <- read.csv("./SP1.csv")

levels(spain$HomeTeam)

#1. Tomar el equipo Barcelona y determinar cantidad de partidos ganados

#Partidos jugados de local por el Barcelona
equipo_local <- subset(spain, spain$HomeTeam == "Barcelona");equipo_local
#Partidos jugados de visitante por el Barcelona
equipo_visi <-  subset(spain, spain$AwayTeam == "Barcelona");equipo_visi

#Partidos ganados como local
local <- equipo_local$FTR  == 'H'
resultados_local <- table(local)
partidos_ganados_local <- sum(resultados_local["TRUE"]) 
partidos_totales_local <- sum(resultados_local)


#Partidos ganados como visitante
visitante <- equipo_visi$FTR == 'A'
resultados_visitante <- table(visitante)
partidos_ganados_visitante <- sum(resultados_visitante["TRUE"])
partidos_totales_visitante <- sum(resultados_visitante)

#Partidos Totales Jugados
partidos_totales <- partidos_totales_local + partidos_totales_visitante; partidos_totales

#Partidos Totales ganados
partidos_totales_ganados <- partidos_ganados_local + partidos_ganados_visitante;partidos_totales_ganados

# Probabilidades
prob_ganar_partido <- partidos_totales_ganados / partidos_totales; prob_ganar_partido
prob_no_ganar <- 1- prob_ganar_partido

# PREGUNTAS DE DISTRIBUCION BINOMIAL
# 1. Probabilidad de que barcelona gane como visitante el proximo partido 
prob_ganar_visitante <- partidos_ganados_visitante / partidos_totales_visitante
prob_no_ganar_visitante <- 1 - prob_ganar_visitante

n <- partidos_totales_visitante + 1
x <- partidos_ganados_visitante + 1
# nCx
combinacion <- factorial(n)/(factorial(x)*factorial(n-x))
respuesta1 <- combinacion*(prob_ganar_visitante^x)*(prob_no_ganar_visitante^(n-x));respuesta1


# 2.Probabilidad de que Barcelona gane 3 de los proximos 4 partidos
n <- partidos_totales + 4
x <- partidos_totales_ganados + 3
combinacion <- factorial(n)/(factorial(x)*factorial(n-x))
respuesta <- combinacion*(prob_ganar_partido^x)*(prob_no_ganar^(n-x)); respuesta

 
# 3. Probabilidad de que Barcelona no gane como local en el proximo partido
prob_no_ganar_local <- (partidos_totales_local - partidos_ganados_local)/ partidos_totales_local
prob_ganar_local <- 1 - prob_perder_local

n <- partidos_totales_local + 1
x <- (partidos_totales_local - partidos_ganados_local) + 1
combinacion <- factorial(n)/(factorial(x)*factorial(n-x))
respuesta <- combinacion*(prob_no_ganar_local^x)*(prob_ganar_local^(n-x)); respuesta

# PREGUNTAS DISTRIBUCION NORMAL

alaves_local <-subset(spain, spain$HomeTeam == "Alaves")
alaves_visitante <-subset(spain, spain$HomeTeam == "Alaves")
goles_local <- alaves_local$FTHG; goles_local
goles_visitante <- alaves_visitante$FTAG; goles_visitante

goles_totales <- c(goles_local, goles_visitante)

frecuencia_goles <- table(goles_totales)

prob_table <- prop.table(frecuencia_goles)
plot(prob_table, type="h")



p <- as.numeric(prob_table)
#esperanza
numero_goles<-c(0:3)
valor_esperado <- weighted.mean(numero_goles,p); valor_esperado
#varianza
library(Weighted.Desc.Stat)
varianza<-w.var(numero_goles,p); varianza 
ds <- w.sd(numero_goles,p); ds

prob_acum <- cumsum(prob_table); prob_acum

# 1. Cuál es la probabilidad de marcar al menos un gol en un partido
prob_acum["1"]

# 2. Cuál es el numero de goles que se marcan en el 80% de los partidos
  
z <- qnorm(0.8)
respuesta <- valor_esperado + ds * z; respuesta


# 3. Cuál es la probabilidad que marquen entre 2 y 3 goles

respuesta  <- prob_acum["3"] - prob_acum["2"]; respuesta
