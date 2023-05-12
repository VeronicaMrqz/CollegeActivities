# Ubicar el directorio de trabajo. 
# HAY QUE CAMBIARLO
setwd("~/Metodos Cuant")

# Nombre del archivo con la base de datos
archivo = "datos-limpios.csv"

# Comando para leer un archivo en el formato CSV
datos = read.csv(archivo , header=TRUE)
# La declarión "header=TRUE" es opcional y
# se pone si las columnas tienen un nombre

# Como visualizar la base
{
  # Comando para aparecer una pestaña adicional con los datos
  View(datos)
  
  # Para ver una parte del inicio
  head ( datos )
  
  # Sólo cierto número de los primeros
  head(datos , 3)
}

# Cómo se accede a los datos?
{
  # A uno en particular
  # nombreDeLaTabla[ registro o renglón, columna ]
  datos[1,3]
  
  # Varios
  datos[   1  ,  c(  5, 6, 9  ) ]
  
  datos[ c(1,65) ,  c(1,5,6,9)]
  
  datos[c(1,65), c(1,5:9)]
  
  datos[ 1:4 , 5:9] 
  
  # Usando "comodin"
  datos[ 1:4 ,     ]
}


# Estadísticos Descriptivos
{
  summary(datos)
  
  # Tomando la columna 6 de todos los registros
  v2014 <- datos[   , 6 ]
  
  # Histograma
  hist(v2014, # Opcionales)
       xlab = "Rango",
       ylab = "Frecuencia",
       col = "pink",
       main = " Histograma del 2014"
  )
  
  # Diagrama de bigotes
  boxplot(  v2014 )
  
  # Diagrama de Bigotes De varios
  soloIndicadores = datos[   , 5:9]
  colnames(soloIndicadores) = c("2012(3)","2014","2015","2016","2017(8)")
  boxplot(soloIndicadores,
          col = c("pink","green","yellow","lightblue","gray"),
          main="Indicadores")
  
}

# Medidas de Centralización
{
  # Tomando la columna 6 de todos los registros
  v2014 = datos[,6]
  
  # Para omitir los datos faltante
  v2014 = na.omit(v2014)
  
  # Cuantos datos son
  n = length(v2014); n
  # Cuanto da la suma de todos ellos
  sum(v2014)
  
  # La suma de todos ellos entre el número de datos da el promedio
  sum(v2014)/length(v2014)
  
  # Comando para obtenerlo  
  mean(v2014)  
  
  ## Rango Medio
  {
    # El valor más pequeño en los datos
    min(v2014)
    
    # El valor más grande en los datos
    max(v2014)
  
    # El rango MEDIO  
    (max(v2014) + min(v2014))/2
    
    hist(v2014,
         col = "pink",
         xlab="2014",
         ylab="Frecuencia",
         main="Hisograma")
    abline(v = mean(v2014), col="red", lwd=2, lty=1)    
    abline(v = (max(v2014) + min(v2014))/2, col="blue", lwd=2, lty=1)    
    legend(0.62,35,
           lty = c(1,1),
           c("Media","Rango Medio"),
           col=c("red","blue"))
  }
  
  # Varianza
  {
    # Tomando la columna 6 de todos los registros
    v2014 = datos[,6]
    
    # Para omitir los datos faltante
    v2014 = na.omit(v2014)
    
    # Cuantos datos son
    n = length(v2014); n
    
    # Desviación estándar
    media = mean(v2014)    
    sqrt(sum((v2014-media)^2)/(n-1))
    sd(v2014)  
    
    # Coeficiente de Variación
    sd(v2014)/mean(v2014)
  }
  
  v2014o <- sort(v2014)
  # Mediana= Valor de la posición 50 en los datos ordenados
  v2014o[50]
  # Posición del primer cuartil
  v2014o[25]
  plot(x=v2014,y=rep(0,n))
  abline(v = quantile(v2014,0.25), col="red", lwd=2, lty=1) 
  abline(v = quantile(v2014,0.50), col="blue", lwd=2, lty=1) 
  abline(v = quantile(v2014,0.75), col="blue", lwd=2, lty=1) 
  
}

# Medidas de Forma
{
  install.packages("moments")
  library(moments)
  # Tomando la columna 6 de todos los registros
  v2014 = datos[,6]
  
  # Para omitir los datos faltante
  v2014 = na.omit(v2014)
  
  skewness(v2014)
  kurtosis(v2014)
  
}

# Medidas de Correlación
{
  v = na.omit(datos[,c(6,7)])  
  cor(v[,1],v[,2],method="spearman")
  cor(v[,1],v[,2],method="kendall")
  lm(formula=v[,1]~v[,2])
  summary(lm(formula=v[,1]~v[,2]))
}

# Graficas de Lineas
{
    # Titulos de los años
    xs = c(2013,2014,2015,2016,2018)
    
    # Cuando es el de uno solo
    nPais = 65
    nombrePais = datos[nPais,2]
    plot(xs, datos[nPais,5:9],
         type = "l",
         col = "red",
         xlab = "Año",
         ylab = "Indicador",
         main = paste0("Indicador de ",nombrePais),
         pch=2)  
    
    # Cuando son de varios  
    # Puede cambiarlo por ks = c(65, 2, 8)
    ks = sample(1:113,3)
    
    #sample(1:113,3) aleatorio ¿Cómo escojo a 3 que yo quiero?
    
    ks = c(65, 2, 8)
    # Si cambia ks, ajuste la lista de colores
    colores = c("red","blue","magenta")  
    datos1 = datos[ks,c(2,5:9)]
    nombres = datos[ks,2]
    datos1 = t(datos1[,-1])
    matplot(xs,datos1,
            type = c("b"),
            pch=1,col = colores,xlab="Año",
            ylab="Indicador",ylim = c(0.1,1))
    legend("topleft", legend = nombres, col=colores, pch=1)
    
  }
  
  