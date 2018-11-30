# PROYECTO 2 - CAPACIDAD HOSPITALARIA RISARALDA

#Lectura archivo
setwd("C:/Users/Deisy\ Salazar\ Parra/Desktop/Info\ Deisy/Maestría\ ISC/Materias/III\ Semestre/Inferencia")
CapHosp <- read.csv("./Capacidad_Hospitalaria_de_Risaralda.csv")

# 1. Tome un municipio de Risaralda y encuentre la media en camas 
  CapHosp
  levels(CapHosp$tipocapacidad)
  capa_mar <- subset(CapHosp, CapHosp$mpio == "MARSELLA" & CapHosp$tipocapacidad == 'CAMAS'); capa_mar

  # camas por sedes en Marsella
  sedes_mar <- aggregate(capa_mar$cantidadcapacidad,by=list(nombresede = capa_mar$nombresede), FUN=sum);sedes_mar
  
  #Promedio del municipio de Marsella
  media_capa_mar <- mean(sedes_mar$x);media_capa_mar
  
  #Respuesta: La media en camas del municipio de Marsella es de 10

# 2. El municipio seleccionado tiene una capacidad hospitalaria dentro de lo esperado respecto al promedio del departamento?

  #2.a Capacidad hospitalaria, en camas, del departamento
  capa_depto <- subset(CapHosp, CapHosp$dpto == 'Risaralda' & CapHosp$tipocapacidad == 'CAMAS'); capa_depto
  
  #camas por sedes en departamento
  sedes_depto <- aggregate(capa_depto$cantidadcapacidad,by=list(nombresede = capa_depto$nombresede), FUN=sum); sedes_depto
  
  #promedio capacidad departamento
  media_capa_depto <- mean(sedes_depto$x);media_capa_depto
  
  #El Promedio bruto de la capacidad en camas del departamento es 59.36
  
  # Promedio neto - sin valores atípicos 
  # Caja de bigotes
  Q1 <- quantile(sedes_depto$x)[2];Q1
  Q3 <- quantile(sedes_depto$x)[4];Q3
  
  r_iqr <- 1.5*IQR(sedes_depto$x); r_iqr
  
  #Limite inferior
  lim_inf <- Q1 - r_iqr;lim_inf
  
  #Límite superior
  lim_sup <- Q3 + r_iqr;lim_sup
  
  #camas a analizar, quitando valores atípicos
  camas_depto <- sedes_depto$x[sedes_depto$x >= lim_inf & sedes_depto$x <= lim_sup];camas_depto
  
  prom_capa_depto <- mean(camas_depto);prom_capa_depto
  
  #El promedio neto en camas de la capacidad del departamento quitando los outliers es de 31.73
  
  # Prueba de Hipótesis que Marsella tiene capacidad hospitalaria igual al departamento
  # Ha = Marsella No tiene capacidad equivalente a la del departamento
  # Ho = Marsella tiene capacidad equivalente a la del departamento
  
  # Desviación estándar del depto
  # Para el promedio bruto
  ds_depto1 <- sd(sedes_depto$x);ds_depto1
  
  # Para el promedio neto
  ds_depto2 <- sd(camas_depto);ds_depto2
  
  # Prueba de dos colas
  # Ha = Promedio de camas de Marsella es diferente al promedio del departamento 
  alpha <- 0.05 # Certeza del 95%
  alpha_medios <- alpha/2
  
  # Z > Z_alpha_medios o Z < Z_alpha_medios
  
  n <- length(camas_depto); n
  
  # Z = (media_depto - media_marsella) / (desviación_estandar / raíz del total)
  z <- (prom_capa_depto - media_capa_mar) / (ds_depto2 / sqrt(n));z #z=3.75
  
  z1 <- qnorm(alpha_medios);z1 # z1 = -1.95
  z2 <- 1-qnorm(alpha_medios);z2 # z2 = 2.95
  
  # Como Z = 3.75 y es mayor que Z2 = 2.95, entonces se rechaza la hipótesis nula
  # Se acepta la Ha, es decir, que el promedio de camas de Marsella es diferente al promedio de camas del departamento
  
# 3. Tomar una muestra de 25 entidades, estimar la media (capacidad hospitalaria) con intervalo de confianza del 90%
  # A. En general
  # B. Municipios pequeños
  # C. Área metropolitana
  
  #A. Muestra entidades
  muestra_a <- sedes_depto[sample(25,replace=FALSE),];muestra_a
  
  # Caja de bigotes
  Q1 <- quantile(muestra_a$x)[2];Q1
  Q3 <- quantile(muestra_a$x)[4];Q3
  r_iqr <- 1.5*IQR(muestra_a$x); r_iqr
  lim_inf <- Q1 - r_iqr;lim_inf
  lim_sup <- Q3 + r_iqr;lim_sup
  
  #camas a analizar, quitando valores atípicos
  camas_muestra <- muestra_a$x[muestra_a$x >= lim_inf & muestra_a$x <= lim_sup];camas_muestra
  
  prom_capa_muestra <- mean(camas_muestra);prom_capa_muestra
  desvi_muestra <- sd(camas_muestra);desvi_muestra
  n_muestra <- length(camas_muestra);n_muestra
  alpha <- 1 - 0.9 # Intervalo confianza 90%
  
  t = qt(alpha/2,df=n_muestra-1);t
  
  #Intervalo con el 90% de confianza de que los datos estarán ahí:
  prom_capa_muestra + t*desvi_muestra/sqrt(n_muestra) # = 20.3245
  prom_capa_muestra - t*desvi_muestra/sqrt(n_muestra) # = 48.039
  
  # Respuesta A:/ Se puede observar, que el promedio de camas de la muestra (34.182) se encuentra entre el rango
  #               entre 20.324 y 48.039
  
  ##################################################################
  # B. Municipios Pequeños
  capa_depto_b <- subset(CapHosp, CapHosp$dpto=='Risaralda' & CapHosp$mpio!="PEREIRA" & CapHosp$mpio!="DOSQUEBRADAS" & CapHosp$tipocapacidad=='CAMAS');capa_depto_b
  sedes_depto_b <- aggregate(capa_depto_b$cantidadcapacidad,by=list(nombresede = capa_depto_b$nombresede), FUN=sum); sedes_depto_b
  
  media_muni_peque <- mean(sedes_depto_b$x);media_muni_peque # media = 13.41

  desvi_muni_peque <- sd(sedes_depto_b$x);desvi_muni_peque 
  n_muni_peque <- length(sedes_depto_b$nombresede);n_muni_peque
  alpha <- 1 - 0.9 # Intervalo confianza 90%
  
  t_muni_peque = qt(alpha/2,df=n_muni_peque-1);t_muni_peque
  
  #Intervalo con el 90% de confianza de que los datos estarán ahí:
  media_muni_peque + t_muni_peque*desvi_muni_peque/sqrt(n_muni_peque) # 9.12
  media_muni_peque - t_muni_peque*desvi_muni_peque/sqrt(n_muni_peque) # 17.70
  
  
  # Respuesta B: Se observa que la media de los municipios pequeños (13.41), es muy inferior al promedio de la muestra del depto (34.182),
  #              Sin embargo, se encuentra en el rango del intervalo de confianza del 90% entre 9.12 y 17.70
  
  ##################################################################
  # c. Área Metropolitana
  capa_area_metro <- subset(CapHosp, CapHosp$dpto=='Risaralda' & (CapHosp$mpio=="PEREIRA" | CapHosp$mpio=="DOSQUEBRADAS") & CapHosp$tipocapacidad=='CAMAS');capa_area_metro
  
  sedes_area_metro <- aggregate(capa_area_metro$cantidadcapacidad,by=list(nombresede = capa_area_metro$nombresede), FUN=sum); sedes_area_metro
  
  media_area_metro <- mean(sedes_area_metro$x);media_area_metro # media = 90
  
  desvi_area_metro <- sd(sedes_area_metro$x);desvi_area_metro
  n_area_metro <- length(sedes_area_metro$nombresede);n_area_metro
  alpha <- 1 - 0.9 # Intervalo confianza 90%
  
  t_area_metro = qt(alpha/2,df=n_area_metro-1);t_area_metro
  
  #Intervalo con el 90% de confianza de que los datos estarán ahí:
  media_area_metro + t_area_metro*desvi_area_metro/sqrt(n_area_metro) # 52.26
  media_area_metro - t_area_metro*desvi_area_metro/sqrt(n_area_metro) # 127.73
  
  # Respuesta C:/ Se puede observar, que el promedio de camas en el área metropolitana (90) es mucho mayor que el promedio del departamemto
                  # Encontrándose en un intervalo de confianza del 90% entre el rango 52.26 y 127.73 

  ######  
# 4.¿El número de salas en Santa Rosa está en el promedio de los municipios "pequeños" de Risaralda?
  
  # Promedio de salas en Santa Rosa 
  capa_sta_rosa <- subset(CapHosp, CapHosp$mpio == "SANTA ROSA DE CABAL" & CapHosp$tipocapacidad == 'SALAS'); capa_sta_rosa
  
  # salas por sedes en Santa Rosa
  sedes_sta_rosa <- aggregate(capa_sta_rosa$cantidadcapacidad,by=list(nombresede = capa_sta_rosa$nombresede), FUN=sum);sedes_sta_rosa
  
  #Promedio del municipio de Santa Rosa
  media_capa_sta_rosa <- mean(sedes_sta_rosa$x);media_capa_sta_rosa #1.6
  
  # Capacidad hospitalaria, en salas, del departamento
  salas_depto <- subset(CapHosp, CapHosp$dpto == 'Risaralda' & CapHosp$mpio!="PEREIRA" & CapHosp$mpio!="DOSQUEBRADAS" & CapHosp$tipocapacidad == 'SALAS'); salas_depto
  
  #salas por sedes en departamento
  salas_sedes_depto <- aggregate(salas_depto$cantidadcapacidad,by=list(nombresede = salas_depto$nombresede), FUN=sum); salas_sedes_depto
  
  #promedio capacidad de salas departamento
  media_salas_depto <- mean(salas_sedes_depto$x);media_salas_depto
  #El Promedio de la capacidad en salas del departamento es 1.38
  
  #desviación estandar 
  desvi_salas_depto <- sd(salas_sedes_depto$x);desvi_salas_depto
  
  n_salas_depto <- length(salas_sedes_depto$nombresede);n_salas_depto
  
  # Calculamos t-student
  # t = (media_depto - media_sta_rosa)/(desv_depto/sqrt(n))
  
  t_salas <- (media_salas_depto - media_capa_sta_rosa)/(desvi_salas_depto/sqrt(n_salas_depto));t_salas
  
  #Supongamos un intervalo con el 95% de confianza
  media_salas_depto + t_salas*desvi_salas_depto/sqrt(n_salas_depto) #1.16
  media_salas_depto - t_salas*desvi_salas_depto/sqrt(n_salas_depto) #1.6
  
  #Respuesta: El promedio de salas de Santa Rosa 1.6 se encuentra en el promedio de los municipios pequeños
            # con un intervalo de confianza del 95% dentro del rango 1.16 y 1.6, encontrándose en el límite superior. 

 # 5. ¿Dada la variación en el número de ambulancias entre los municipios pequeños, la de Quinchía es igual a la departamental?
  # Promedio de ambulancias en Quinchía
  levels(CapHosp$tipocapacidad)
  ambu_quinchia <- subset(CapHosp, CapHosp$mpio == "QUINCHÃA" & CapHosp$tipocapacidad == 'AMBULANCIAS'); ambu_quinchia
  
  # ambulancias por sede en Quinchía
  sedes_quinchia <- aggregate(ambu_quinchia$cantidadcapacidad,by=list(nombresede = ambu_quinchia$nombresede), FUN=sum);sedes_quinchia
  
  #Promedio de ambulancias municipio de Quinchia
  media_ambu_quinchia <- mean(sedes_quinchia$x);media_ambu_quinchia #2.5
  
  # Capacidad hospitalaria, en ambulancias, del departamento
  ambu_depto <- subset(CapHosp, CapHosp$dpto == 'Risaralda' & CapHosp$mpio!="PEREIRA" & CapHosp$mpio!="DOSQUEBRADAS" & CapHosp$tipocapacidad == 'AMBULANCIAS'); ambu_depto
  
  #Ambulancias por sedes en departamento
  ambu_sedes_depto <- aggregate(ambu_depto$cantidadcapacidad,by=list(nombresede = ambu_depto$nombresede), FUN=sum); ambu_sedes_depto
  
  #promedio capacidad de ambulancias departamento
  media_ambu_depto <- mean(ambu_sedes_depto$x);media_ambu_depto
  #El Promedio de la capacidad en ambulancias del departamento es 3.16
  
  #desviación estandar 
  desvi_ambu_depto <- sd(ambu_sedes_depto$x);desvi_ambu_depto
  
  n_ambu_depto <- length(ambu_sedes_depto$nombresede);n_ambu_depto
  
  # Calculamos t-student
  # t = (media_depto - media_quinchia)/(desv_depto/sqrt(n))
  
  t_ambu <- (media_ambu_depto - media_ambu_quinchia)/(desvi_ambu_depto/sqrt(n_ambu_depto));t_ambu
  
  #Supongamos un intervalo con el 95% de confianza
  media_ambu_depto + t_ambu*desvi_ambu_depto/sqrt(n_ambu_depto) #3.83
  media_ambu_depto - t_ambu*desvi_ambu_depto/sqrt(n_ambu_depto) #2.5
  
  #Respuesta: El promedio de ambulancias de Quinchía 2.5 se encuentra en el promedio de los municipios pequeños 3.16
  # con un intervalo de confianza del 95% dentro del rango 2.5 y 3.83, encontrándose en el límite inferior 
  

  