#Verónica Márquez A00827782
#Pruebas de hipótesis. Exploración de datos de la BD de ENIGH. 



setwd("~/Metodos")
archivo = "concentrado-2018.csv"
datos = read.csv(archivo , header=TRUE)
View(datos)


colIngresoCorriente = 23
colnames(datos)[colIngresoCorriente]
ingresosCor = datos[,colIngresoCorriente]

# Columna "sexo_jefe" = Sexo del jefe del hogar
  # Columna 10
  # Códigos:
  # 1: Hombre
  # 2: Mujer
  summary(datos[   , 10])    
  table(datos[,10])

#Datos generales de los INGRESOS CORRIENTES de los hogares en 2018

  #Número de registros de la columna de Ingreso Corriente
  numDatos = length(ingresosCor); numDatos
  
  #Estadísticos descriptivos del ingreso corriente.
  summary(ingresosCor)
  
  #Media
  mean(ingresosCor)
  
  #Moda
  install.packages("modeest") 
  library(modeest)
  mfv(ingresosCor)
  tabla<- table(ingresosCor)
  tabla
  sort(tabla, decreasing=TRUE)
  
  #Rango Medio
  min(ingresosCor)
  max(ingresosCor)
  (max(ingresosCor) + min(ingresosCor))/2
  
  #Medidas de dispersión 2014
  
  #Rango
  min(ingresosCor)
  max(ingresosCor)
  max(ingresosCor)-min(ingresosCor)
  
  #Varianza
  var(ingresosCor,na.rm=TRUE)
  
  #Desviación estándar
  sd(ingresosCor)
  
  #Rango Intercuartílico
  RI <- ingresosCor
  IQR(RI)
  
  #Coeficiente de variación
  library(FinCal)
  coefficient.variation(sd=sd(ingresosCor), avg= mean(ingresosCor))
  
  #Aportantes al ingreso familiar
  
  # Columna "sexo_jefe" = Sexo del jefe del hogar
  # Columna 10
  # Códigos:
  # 1: Hombre
  # 2: Mujer
  summary(datos[   , 10])    
  table(datos[,10])
  
  #Hombres= 54171 
  #Mujeres = 20476
  x = c(54171, 20476)
  
  barplot(x,  
          xlab= ("Aportantes del ingreso familiar"),
          ylab = "Frecuencia",
          col = c("cornflowerblue","pink"),
          main = "¿Quiénes proveen el ingreso?")
#--------------------------------------------------------------------


  # Pobreza extrema
  #dolar = 21.81 pesos 4 junio . pobreza extrema: ingresos inferiores
  # a 1.90 dolares al día: = 41.439 pesos al dia, =1,243.17 al 
  #mes= 3,729.51 altrimestre
  auxPE = ingresosCor[ingresosCor < 3729.51]
  length(auxPE)
  

  #Pobreza relativa
  #consideran pobre a un hogar en un país si sus ingresos son inferiores 
  #al 50 % del ingreso medio por hogar en ese país
  auxPR = ingresosCor[ingresosCor < mean(ingresosCor)]
  length(auxPR)
  
  #Pobreza moderada
  #una persona en un país de ingresos medio-bajos es moderadamente pobre
  #si sus ingresos están entre 1,90 y 3,20 $ diarios
  #1$= 21.81, entre 41.439 y 69.792 pesos diarios
  #Entre 1,243.17 y 1,522.16352 mensuales
  #Entre 3,729.52 y 4,566.49 trimestrales
  auxPM = ingresosCor[ingresosCor <= 3729.52 & ingresosCor <= 4566.49]
  length(auxPM)
  
  #Pobreza severa
  #porcentaje de personas que viven en hogares con una renta 
  #disponible inferior al 40 % de la renta mediana
  auxPS = ingresosCor[ingresosCor < 0.4*(median(ingresosCor))]  
  length(auxPS)
  
  

  
  
