setwd("~/Metodos Cuant")

archivo = "datos-limpios.csv"

datos= read.csv(archivo, header=TRUE)

View(datos)

# Graficas de Lineas
{
  # Titulos de los años
  xs = c(2013,2014,2015,2016,2018)
  
  # Cuando es el de uno solo
  library(ggplot2)
  
  nPais = 65
  nombrePais = datos[nPais,2]
  plot(xs,datos[nPais,5:9],
       type = "l",
       col = "red",
       xlab = "Año",
       ylab = "Indicador",
       main = paste0("Indicador de ",nombrePais),
       pch=2)  +
    geom_line(plot(xs,datos[5,5:9],
                   type = "l",
                   col = "red",
                   xlab = "Año",
                   ylab = "Indicador",
                   main = paste0("Indicador de ",nombrePais),
                   pch=2))
  
  
 
  
    
  }


