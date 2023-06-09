setwd("~/Metodos Cuant")
archivo = "datos-limpios.csv"
datos = read.csv(archivo , header=TRUE)
View(datos)


#GR�FICA DE ,L�NEAS DE COMPARACI�N DEL �NDICE DE ESTADO DE DERECHO.
xs = c(2012, 2014,2015,2016,2017)
ks = c(75,29,65,19,110) #Venezuela,Camboya, M�xico, Noruega, Dinamarca

colores = c("red","blue", "green", "pink", "brown")  
datos1 = datos[ks,c(2,5:9)]
nombres = datos[ks,2]
datos1 = t(datos1[,-1])
matplot(xs,datos1,
        type = c("b"),
        pch=1,col = colores,xlab="A�o",
        main= "Comparaci�n del �ndice de Estado de Derecho",
        ylab="Indicador",ylim = c(0.1,1))
legend("topleft", legend = nombres, col=colores, pch=1)

#Medidas de tendencia central.
summary(datos)


# Variables a�os
v2012 <- datos[ ,5]
v2014 <- datos[ ,6]
v2015 <- datos[ ,7]
v2016 <- datos[ ,8]
v2017 <- datos[ ,9]


# Medidas de Centralizaci�n 2012-2013

v2012 = datos[,5]
v2012 = na.omit(v2012)
n = length(v2012); n
sum(v2012)

#Media aritm�tica o promedio
sum(v2012)/length(v2012)
mean(v2012)  

#Mediana
median(v2012, na.rm=TRUE)

#Moda
install.packages("modeest") 
library(modeest)
mfv(v2012)
tabla<- table(v2012)
tabla
sort(tabla, decreasing=TRUE)

#Rango Medio
min(v2012)
max(v2012)
(max(v2012) + min(v2012))/2

#Medidas de dispersi�n 2014

#Rango
min(v2012)
max(v2012)
max(v2012)-min(v2012)

#Varianza
var(v2012,na.rm=TRUE)

#Rango Intercuart�lico
RI <- v2012
IQR(RI)

#Desviaci�n est�ndar
sd(v2012, na.rm=TRUE)

#Coeficiente de variaci�n
library(FinCal)
coefficient.variation(sd=sd(v2012), avg= mean(v2012))

#Valores del 10% con el mejor(�ltimo) y peor(primero) �ndice.
percentil10 = v2012
quantile(percentil10, c(.10,.90))

#-------------------------------------

# Medidas de Centralizaci�n 2014

v2014 = datos[,6]
v2014 = na.omit(v2014)
n = length(v2014); n
sum(v2014)

#Media aritm�tica o promedio
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

#Medidas de dispersi�n 2014

#Rango
min(v2014)
max(v2014)
max(v2014)-min(v2014)

#Varianza
var(v2014,na.rm=TRUE)

#Rango Intercuart�lico
RI <- v2014
IQR(RI)

#Desviaci�n est�ndar
sd(v2014, na.rm=TRUE)

#Coeficiente de variaci�n
library(FinCal)
coefficient.variation(sd=sd(v2014), avg= mean(v2014))

#Valores del 10% con el mejor(�ltimo) y peor(primero) �ndice.
percentil10 = v2014
quantile(percentil10, c(.10,.90))


#----------------------------

# Medidas de Centralizaci�n 2015

v2015 = datos[,7]
v2015 = na.omit(v2015)
n = length(v2015); n
sum(v2015)

#Media aritm�tica o promedio
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

#Medidas de dispersi�n 2015

#Rango
min(v2015)
max(v2015)
max(v2015)-min(v2015)

#Varianza
var(v2015,na.rm=TRUE)

#Rango Intercuart�lico
RI <- v2015
IQR(RI)

#Desviaci�n est�ndar
sd(v2015, na.rm=TRUE)

#Coaeficiente de variaci�n
library(FinCal)
coefficient.variation(sd=sd(v2015), avg= mean(v2015))

#Valores del 10% con el mejor(�ltimo) y peor(primero) �ndice.
percentil10 = v2015
quantile(percentil10, c(.10,.90))


#--------------------------------
# Medidas de Centralizaci�n 2016

v2016 = datos[,8]
v2016 = na.omit(v2016)
n = length(v2016); n
sum(v2016)

#Media aritm�tica o promedio
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

#Medidas de dispersi�n 2016

#Rango
min(v2016)
max(v2016)
max(v2016)-min(v2016)

#Varianza
var(v2016,na.rm=TRUE)

#Desviaci�n est�ndar
sd(v2016, na.rm=TRUE)

#Rango Intercuart�lico
RI <- v2016
IQR(RI)

#Coeficiente de variaci�n
library(FinCal)
coefficient.variation(sd=sd(v2016), avg= mean(v2016))
#sd(v2016)/mean(v2016)-> Da lo mismo

#Valores del 10% con el mejor(�ltimo) y peor(primero) �ndice.
percentil10 = v2016
quantile(percentil10, c(.10,.90))


#-----------------------------

# Medidas de Centralizaci�n 2017-2018

v2017 = datos[,9]
v2017 = na.omit(v2017)
n = length(v2017); n
sum(v2017)

#Media aritm�tica o promedio
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

#Medidas de dispersi�n 2017-2018

#Rango
min(v2017)
max(v2017)
max(v2017)-min(v2017)

#Varianza
var(v2017,na.rm=TRUE)

#Desviaci�n est�ndar
sd(v2017, na.rm=TRUE)

#Rango Intercuart�lico
RI <- v2017
IQR(RI)

#Coeficiente de variaci�n
library(FinCal)
coefficient.variation(sd=sd(v2017), avg= mean(v2017))
#sd(v2017)/mean(v2017)-> Da lo mismo

#Valores del 10% con el mejor(�ltimo) y peor(primero) �ndice.
percentil10 = v2017
quantile(percentil10, c(.10,.90))



#------------------------------------------
#Histogramas
par(mfrow=c(2,3)) 

#Histograma 2012-2013
hist(v2012,
     xlab = "Rango de de IED",
     ylab = "Frecuencia de pa�ses",
     col = "cornflowerblue",
     main = "2012(13)")
abline(v = mean(v2017), col="red", lwd=2, lty=1)
abline(v = median(v2017), col="lightblue", lwd=2, lty=1)    
abline(v = (max(v2017) + min(v2017))/2, col="blue", lwd=2, lty=1)   
abline(v = mfv(v2017), col="brown", lwd=2, lty=1) 

legend(0.62,35,
       lty = c(1,1),
       c("Media","Mediana","Rango Medio", "Moda"),
       col=c("red","lightblue", "blue", "brown"))




#Histograma 2014
hist(v2014,
     xlab = "Rango de de IED",
     ylab = "Frecuencia de pa�ses",
     col = "gray",
     main = "2014")
abline(v = mean(v2014), col="red", lwd=2, lty=1)
abline(v = median(v2014), col="lightblue", lwd=2, lty=1)    
abline(v = (max(v2014) + min(v2014))/2, col="blue", lwd=2, lty=1)   
abline(v = mfv(v2014), col="brown", lwd=2, lty=1) 

legend(0.5,35,
       lty = c(1,1),
       c("Media","Mediana","Rango Medio", "Moda"),
       col=c("red","lightblue", "blue", "brown"))


#Histograma 2015
hist(v2015,
     xlab = "Rango de de IED",
     ylab = "Frecuencia de pa�ses",
     col = "red",
     main = "2015")
abline(v = mean(v2015), col="red", lwd=2, lty=1)
abline(v = median(v2015), col="lightblue", lwd=2, lty=1)    
abline(v = (max(v2015) + min(v2014))/2, col="blue", lwd=2, lty=1)   
abline(v = mfv(v2015), col="brown", lwd=2, lty=1) 

legend(0.62,35,
       lty = c(1,1),
       c("Media","Mediana","Rango Medio", "Moda"),
       col=c("red","lightblue", "blue", "brown"))



#Histograma 2016

hist(v2016,
     xlab = "Rango de de IED",
     ylab = "Frecuencia de pa�ses",
     col = "lightblue",
     main = "2016")
abline(v = mean(v2016), col="red", lwd=2, lty=1)
abline(v = median(v2016), col="lightblue", lwd=2, lty=1)    
abline(v = (max(v2016) + min(v2014))/2, col="blue", lwd=2, lty=1)   
abline(v = mfv(v2016), col="brown", lwd=2, lty=1) 

legend(0.62,35,
       lty = c(1,1),
       c("Media","Mediana","Rango Medio", "Moda"),
       col=c("red","lightblue", "blue", "brown"))

#Histograma 2017-2018
hist(v2017,
     xlab = "Rango de de IED",
     ylab = "Frecuencia de pa�ses",
     col = "pink",
     main = " 2017(18)")
abline(v = mean(v2017), col="red", lwd=2, lty=1)
abline(v = median(v2017), col="lightblue", lwd=2, lty=1)    
abline(v = (max(v2017) + min(v2017))/2, col="blue", lwd=2, lty=1)   
abline(v = mfv(v2017), col="brown", lwd=2, lty=1) 

legend(0.62,35,
       lty = c(1,1),
       c("Media","Mediana","Rango Medio", "Moda"),
       col=c("red","lightblue", "blue", "brown"))
#------------------------------------------------------

#Diagrama de caja.
par(mfrow=c(1,2)) 


soloIndicadores = datos[ , c(5:9)]
colnames(soloIndicadores) = c("2012(3)","2014","2015", "2016","2017(8)")
boxplot(soloIndicadores,
        col = c("cadetblue","cornflowerblue", "cyan3","aquamarine3","aquamarine2"),
        xlab= "a�o",
        ylab="�ndice",
        main="BoxPlot del IED")

#Valores del 10% de los pa�ses con el peor y el mejor �ndice 

percentil2012 = v2012
d2012 = quantile(percentil2012, c(.10,.90))
quantile(percentil2012, c(.10,.90))

percentil2014 = v2014
d2014 = quantile(percentil2012, c(.10,.90))
quantile(percentil2014, c(.10,.90))

percentil2015 = v2015
d2015= quantile(percentil10, c(.10,.90))
quantile(percentil2015, c(.10,.90))

percentil2016 = v2016
d2016= quantile(percentil10, c(.10,.90))
quantile(percentil2016, c(.10,.90))

percentil2017 = v2017
d2017= quantile(percentil2017, c(.10,.90))
quantile(percentil2017, c(.10,.90))

#Apreciaci�n en diagrama de caja.
boxplot(d2012,d2014, d2015, d2016, d2017,
        xlab = "a�o",
        ylab= "�ndice",
        col = c("cadetblue","cornflowerblue", "cyan3", "aquamarine3", "aquamarine2"),
        main="10% peores y mejores")
