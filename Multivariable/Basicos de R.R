#crear un escalar (variables)

a<- 2

#Crear un vector
v<- c(2,3,4)

#ABRIR UN EXCEL

#Instalar paquete para abrir excel
install.packages("readxl")

#Llamar libreria
library(readxl)

#Codigo si tienes un proyecto configurado
datos <- read_excel("DatosM.xlsx", sheet= "1.1")

#Grafico de dispersion
plot(datos$V1~ datos$V2)

#coeficiente de correlacion
cor.test(datos$V1, datos$V2)

#si alfa >= valor p, se rechaza H0 en favor de Ha

#H0: rho=0
#H1: rho =! 0

#Como alfa=0.05 < que valor p de 0.6569 nos quedamos con H0 la correlacion es 0

# -1 >= r>= 1
# |r| >= 0.9 relacion fuerte
# 0.6 <= r <= 0.9 relacion moderada
# |r| < 0.6 relacion debil




