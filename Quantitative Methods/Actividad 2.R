setwd("~/Metodos Cuant")

archivo = "datos-limpios.csv"

datos = read.csv(archivo , header=TRUE)

View(datos)


#GRÁFICA DE ,LÍNEAS DE COMPARACIÓN DEL ÍNDICE DE ESTADO DE DERECHO.
  xs = c(2013,2014,2015,2016,2018)

  ks = c(110, 19, 65, 29, 75,21, 107) #Venezuela,Camboya, México, Noruega, Dinamarca,Canadá y EUA.
 
  colores = c("red","blue","magenta", "yellow","green", "gray", "purple")  
  datos1 = datos[ks,c(2,5:9)]
  nombres = datos[ks,2]
  datos1 = t(datos1[,-1])
  matplot(xs,datos1,
          type = c("b"),
          pch=1,col = colores,xlab="Año",
          main= "Comparación de Índice de Estado de Derecho",
          ylab="Indicador",ylim = c(0.1,1))
  legend("topleft", legend = nombres, col=colores, pch=1)

  
#Medidas de tendencia central.

  summary(datos)
  
  v2018 <- datos[ ,9]
  
  # Histograma
  hist(v2018, 
      xlab = "Rango de de IED",
      ylab = "Frecuencia de países",
      col = "lightblue",
      main = " Histograma del IED del 2017-2018")
  
  # Medidas de Centralización

    v2018 = datos[,9]
    
    v2018 = na.omit(v2018)
    
    n = length(v2018); n
    
    sum(v2018)
    
    #Media aritmética o promedio
    sum(v2018)/length(v2018)
    mean(v2018)  
    
    #Mediana
    median(v2018, na.rm=TRUE)
    
    #Moda
    tabla<- table(v2018)
    tabla
    sort(tabla, decreasing=TRUE)
    #Por lo tanto la moda es 0.47
    
    #Rango Medio
    min(v2018)
    max(v2018)
    (max(v2018) + min(v2018))/2

    
  #Medidas de dispersión
    
    #Rango
    min(v2018)
    max(v2018)
    max(v2018)-min(v2018)

    #Varianza
    var(v2018,na.rm=TRUE)

    #Desviación estándar
    sd(v2018, na.rm=TRUE)

    #Coeficiente de variación
    install.packages("FinCal")
    library(FinCal)
    coefficient.variation(sd=sd(v2018), avg= mean(v2018))
    sd(v2018)/mean(v2018)
    
    #Rango Intercuartílico
    RI <- v2018
    IQR(RI)
    
    #Valores del 10% con el mejor(último) y peor(primero) índice.
    percentil10 = v2018
    quantile(percentil10, c(.10,.90))
    quantile(v2018, prob=seq(0, 1, length = 11))#Sale lo mismo
    
    #No entendí
    install.packages("StatMeasures")
    library(StatMeasures)
    decile(v2018, decreasing = TRUE) 
    