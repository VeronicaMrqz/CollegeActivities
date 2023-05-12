setwd("~/Metodos Cuant")
archivo = "datos-limpios.csv"
datos = read.csv(archivo , header=TRUE)
View(datos)


#GRÁFICA DE ,LÍNEAS DE COMPARACIÓN DEL ÍNDICE DE ESTADO DE DERECHO.
xs = c(2014, 2015,2016,2017)
ks = c(75,29,65,19,110) #Venezuela,Camboya, México, Noruega, Dinamarca

colores = c("red","blue", "green", "pink", "brown")  
datos1 = datos[ks,c(2,6:9)]
nombres = datos[ks,2]
datos1 = t(datos1[,-1])
matplot(xs,datos1,
        type = c("b"),
        pch=1,col = colores,xlab="Año",
        main= "Comparación del Índice de Estado de Derecho",
        ylab="Indicador",ylim = c(0.1,1))
legend("topleft", legend = nombres, col=colores, pch=1)


#Medidas de tendencia central.
summary(datos)
 

# Variables años
v2012 <- datos[ ,5]
v2012 = na.omit(v2012)

v2014 <- datos[ ,6]
v2015 <- datos[ ,7]
v2016 <- datos[ ,8]
v2017 <- datos[ ,9]


#-------------------------------
# Medidas de Centralización 2014

v2014 = datos[,6]
v2014 = na.omit(v2014)
n = length(v2014); n
sum(v2014)

#Media aritmética o promedio
sum(v2014)/length(v2014)
mean(v2014)  

#Mediana
median(v2014, na.rm=TRUE)

#Moda
install.packages("modeest") 
library(modeest)
mfv(v2014)
tabla<- table(v2014)
tabla
sort(tabla, decreasing=TRUE)

#Rango Medio
min(v2014)
max(v2014)
(max(v2014) + min(v2014))/2

#Medidas de dispersión 2014

#Rango
min(v2014)
max(v2014)
max(v2014)-min(v2014)

#Varianza
var(v2014,na.rm=TRUE)

#Rango Intercuartílico
RI <- v2014
IQR(RI)

#Desviación estándar
sd(v2014, na.rm=TRUE)

#Coeficiente de variación
library(FinCal)
coefficient.variation(sd=sd(v2014), avg= mean(v2014))

#Valores del 10% con el mejor(último) y peor(primero) índice.
percentil10 = v2014
quantile(percentil10, c(.10,.90))

#par(mfrow=c(2,2)) si se quiere observar los 4 Histogramas en 1 ventana.

#Histograma 2014
hist(v2014,
     xlab = "Rango de de IED",
     ylab = "Frecuencia de países",
     col = "gray",
     main = " Histograma del IED del 2014")
abline(v = mean(v2014), col="red", lwd=2, lty=1)
abline(v = median(v2014), col="lightblue", lwd=2, lty=1)    
abline(v = (max(v2014) + min(v2014))/2, col="blue", lwd=2, lty=1)   
abline(v = mfv(v2014), col="brown", lwd=2, lty=1) 

legend(0.5,35,
       lty = c(1,1),
       c("Media","Mediana","Rango Medio", "Moda"),
       col=c("red","lightblue", "blue", "brown"))

#----------------------------

# Medidas de Centralización 2015

v2015 = datos[,7]
v2015 = na.omit(v2015)
n = length(v2015); n
sum(v2015)

#Media aritmética o promedio
sum(v2015)/length(v2015)
mean(v2015)  

#Mediana
median(v2015, na.rm=TRUE)

#Moda
mfv(v2015)
tabla<- table(v2015)
tabla
sort(tabla, decreasing=TRUE)

#Rango Medio
min(v2015)
max(v2015)
(max(v2015) + min(v2015))/2

#Medidas de dispersión 2015

#Rango
min(v2015)
max(v2015)
max(v2015)-min(v2015)

#Varianza
var(v2015,na.rm=TRUE)

#Rango Intercuartílico
RI <- v2015
IQR(RI)

#Desviación estándar
sd(v2015, na.rm=TRUE)

#Coaeficiente de variación
library(FinCal)
coefficient.variation(sd=sd(v2015), avg= mean(v2015))

#Valores del 10% con el mejor(último) y peor(primero) índice.
percentil10 = v2015
quantile(percentil10, c(.10,.90))

#Histograma 2015
hist(v2015,
     xlab = "Rango de de IED",
     ylab = "Frecuencia de países",
     col = "red",
     main = " Histograma del IED del 2015")
abline(v = mean(v2015), col="red", lwd=2, lty=1)
abline(v = median(v2015), col="lightblue", lwd=2, lty=1)    
abline(v = (max(v2015) + min(v2014))/2, col="blue", lwd=2, lty=1)   
abline(v = mfv(v2015), col="brown", lwd=2, lty=1) 

legend(0.62,35,
       lty = c(1,1),
       c("Media","Mediana","Rango Medio", "Moda"),
       col=c("red","lightblue", "blue", "brown"))

#--------------------------------
# Medidas de Centralización 2016

v2016 = datos[,8]
v2016 = na.omit(v2016)
n = length(v2016); n
sum(v2016)

#Media aritmética o promedio
sum(v2016)/length(v2016)
mean(v2016)  

#Mediana
median(v2016, na.rm=TRUE)

#Moda
mfv(v2016)
tabla<- table(v2016)
tabla
sort(tabla, decreasing=TRUE)

#Rango Medio
min(v2016)
max(v2016)
(max(v2016) + min(v2016))/2

#Medidas de dispersión 2016

#Rango
min(v2016)
max(v2016)
max(v2016)-min(v2016)

#Varianza
var(v2016,na.rm=TRUE)

#Desviación estándar
sd(v2016, na.rm=TRUE)

#Rango Intercuartílico
RI <- v2016
IQR(RI)

#Coeficiente de variación
library(FinCal)
coefficient.variation(sd=sd(v2016), avg= mean(v2016))
#sd(v2016)/mean(v2016)-> Da lo mismo

#Valores del 10% con el mejor(último) y peor(primero) índice.
percentil10 = v2016
quantile(percentil10, c(.10,.90))

#Histograma 2016

hist(v2016,
     xlab = "Rango de de IED",
     ylab = "Frecuencia de países",
     col = "lightblue",
     main = " Histograma del IED del 2016")
abline(v = mean(v2016), col="red", lwd=2, lty=1)
abline(v = median(v2016), col="lightblue", lwd=2, lty=1)    
abline(v = (max(v2016) + min(v2014))/2, col="blue", lwd=2, lty=1)   
abline(v = mfv(v2016), col="brown", lwd=2, lty=1) 

legend(0.62,35,
       lty = c(1,1),
       c("Media","Mediana","Rango Medio", "Moda"),
       col=c("red","lightblue", "blue", "brown"))
#-----------------------------

# Medidas de Centralización 2017-2018

v2017 = datos[,9]
v2017 = na.omit(v2017)
n = length(v2017); n
sum(v2017)

#Media aritmética o promedio
sum(v2017)/length(v2017)
mean(v2017)  

#Mediana
median(v2017, na.rm=TRUE)

#Moda
mfv(v2017)
tabla<- table(v2017)
tabla
sort(tabla, decreasing=TRUE)

#Rango Medio
min(v2017)
max(v2017)
(max(v2017) + min(v2017))/2

#Medidas de dispersión 2017-2018

#Rango
min(v2017)
max(v2017)
max(v2017)-min(v2017)

#Varianza
var(v2017,na.rm=TRUE)

#Desviación estándar
sd(v2017, na.rm=TRUE)

#Rango Intercuartílico
RI <- v2017
IQR(RI)

#Coeficiente de variación
library(FinCal)
coefficient.variation(sd=sd(v2017), avg= mean(v2017))
#sd(v2017)/mean(v2017)-> Da lo mismo

#Valores del 10% con el mejor(último) y peor(primero) índice.
percentil10 = v2017
quantile(percentil10, c(.10,.90))

#Histograma 2017
hist(v2017,
     xlab = "Rango de de IED",
     ylab = "Frecuencia de países",
     col = "pink",
     main = " Histograma del IED del 2017-2018")
abline(v = mean(v2017), col="red", lwd=2, lty=1)
abline(v = median(v2017), col="lightblue", lwd=2, lty=1)    
abline(v = (max(v2017) + min(v2017))/2, col="blue", lwd=2, lty=1)   
abline(v = mfv(v2017), col="brown", lwd=2, lty=1) 

legend(0.62,35,
       lty = c(1,1),
       c("Media","Mediana","Rango Medio", "Moda"),
       col=c("red","lightblue", "blue", "brown"))

#--------------------------

setwd("~/Metodos Cuant")
archivo2 = "variaciones.csv"
resultados = read.csv(archivo2 , header=TRUE)
View(resultados)

#---------------------------------

#Diagrama de bigotes

# Diagrama de bigotes
boxplot(datos[, c(5,8,9)])

soloIndicadores = datos[ , c(5,8,9)]
colnames(soloIndicadores) = c("2012(3)","2016","2017(8)")
boxplot(soloIndicadores,
        col = c("cadetblue","cornflowerblue", "cyan3"),
        xlab= "año",
        ylab="índice",
        main="BoxPlot del IED")

#Valores del 10% de los países con el peor y el mejor índice 

percentil2012 = v2012
d2012 = quantile(percentil2012, c(.10,.90))
quantile(percentil2012, c(.10,.90))

percentil2016 = v2016
d2016= quantile(percentil10, c(.10,.90))
quantile(percentil10, c(.10,.90))

percentil2017 = v2017
d2017= quantile(percentil2017, c(.10,.90))
quantile(percentil2017, c(.10,.90))

#Apreciación en diagrama de caja.
boxplot(d2012, d2016, d2017,
                xlab = "año",
                ylab= "índice",
                col = c("cadetblue","cornflowerblue", "cyan3"),
                main="índices del 10% de los peores y mejores")






