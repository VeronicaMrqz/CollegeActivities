#Verónica Márquez. A00827782


#Cargar base de datos.
setwd("~/Metodos")
archivo = "concentrado-2018.csv"
datos = read.csv(archivo , header=TRUE)
View(datos)
329-744-674


#Poblacion sin los límites extremos.
  colIngresoCorriente = 23
 
  colnames(datos)[colIngresoCorriente]
  ingresosCor = datos[   , colIngresoCorriente]
  
  resumen = summary(ingresosCor); resumen
  
  primerCuartil = resumen[2]; primerCuartil 
  tercerCuartil = resumen[5]; tercerCuartil
  rangoIntercuartil = tercerCuartil-primerCuartil; rangoIntercuartil 
  
  limiteSup = tercerCuartil + 1.5*rangoIntercuartil
  limiteInf = primerCuartil - 1.5*rangoIntercuartil
  
  #población reducida. 
  popTrabajar = ingresosCor[(ingresosCor < limiteSup) & (ingresosCor > limiteInf)]
  hist(popTrabajar) #distribución normal
  
  summary(popTrabajar)
  muPop = mean(popTrabajar); muPop #Media de la población
  
  
  m = length(popTrabajar); m  
  
  # Tamaño de la muestra
  nMuestra = 300   
  registrosMuestra = sample(1:m,nMuestra) #sacar muestra de la selección
  
  muestra = popTrabajar[registrosMuestra]
  summary(muestra)
  xBarra = mean(muestra); xBarra
  sMuestra = sd(muestra); sMuestra
  
  alfa = 0.05 #(1-.95) % confianza
  zc = qnorm(1-alfa/2); zc
  
  tc = qt(1-alfa/2,nMuestra-1); tc 
  
  # Intervalo 
  c(xBarra-sMuestra/sqrt(nMuestra)*zc,  #Se distribuye normal.
    xBarra+sMuestra/sqrt(nMuestra)*zc)  
  c(xBarra-sMuestra/sqrt(nMuestra)*tc, #No se distribuye normalmente 
    xBarra+sMuestra/sqrt(nMuestra)*tc)
  muPop    
  
  t.test(muestra, mu = muPop, alternative = "two.sided") #Pruba de hipótesis
