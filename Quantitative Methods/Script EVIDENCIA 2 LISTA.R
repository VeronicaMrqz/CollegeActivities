######################################
            #EVIDENCIA 2
######################################
#Verónica Márquez. A00827782 
#Arturo Uriel Quilantan. A00825104 
#José Ángel García. A001379000 
#Estefanía Portador Reyes. A00829062


#Cargar base de datos.
setwd("~/Metodos cuantitativos")
archivo = "concentrado-2018.csv"
datos = read.csv(archivo , header=TRUE)
View(datos)

##############################
  #INTERVALOS DE CONFIANZA
##############################

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
  
c(xBarra-sMuestra/sqrt(nMuestra)*tc, #No se distribuye normalmente 
  xBarra+sMuestra/sqrt(nMuestra)*tc)
muPop    

t.test(muestra, mu = muPop, alternative = "two.sided") #Pruba de hipótesis


########################################
  #PRUEBA DE HIPÓTESIS DE HOGARES POBRES      
########################################

{
  p0 = 0.43 #Dato obtenido de Forbes
  limite = 0.6*37012
  alfa = 0.05
  zAlfa = qnorm(1-alfa); zAlfa
  
  # Tamaño de la muestra
  n = 300
  muestra = sample(popTrabajar,n)
  cualesCumplen = muestra [muestra < limite]
  pGorrito = length(cualesCumplen)/n; pGorrito
  z0 = (pGorrito-p0)/sqrt(p0*(1-p0)/n); z0
  
  # Regla de decisión basada en la región de rechazo
  # Extremos de la región de decisión
  C2 = p0 - zAlfa*sqrt(p0*(1-p0)/n); C2
  C2 < z0 
  
  # Regla de decisión basada en z0
  z0 < zAlfa
  
  # Regla de decisión basada en el pValor
  pValor = 1-pnorm(-z0); pValor
  pValor > alfa
}
  
  
  #############################################################  
  # Prueba de hipótesis para proporciones (JEFES DE FAMILIA)       
  #############################################################  
    
    
    # Afirmamos que el 25% de los hogares en México tiene como jefe de familia una mujer
    # pPoblacional = 0.25
    {
      p0 = 0.25
      
      alfa = 0.05
      zAlfaEn2 = qnorm(1-alfa/2); zAlfaEn2
      
      # Tamaño de la muestra
      n = 300
      muestra = sample(misDatos,n)
      pGorrito = sum(muestra == 2)/n; pGorrito
      
      #Muestra como esta la poblacion
      datosP = table(muestra) ; datosP 
      datosP[2]/length(muestra)
      
      # Regla de decisión basada en la región de rechazo
      # Extremos de la región de decisión
      C1 = p0 - zAlfaEn2*sqrt(p0*(1-p0)/n); C1
      C2 = p0 + zAlfaEn2*sqrt(p0*(1-p0)/n); C2
      pGorrito < C1
      pGorrito > C2
      # Regla de decisión basada en z0
      z0 = (pGorrito-p0)/sqrt(p0*(1-p0)/n); z0
      z0 < -zAlfaEn2
      z0 >  zAlfaEn2    
      
      # Regla de decisión basada en el pValor
      pValor = pnorm(z0); pValor
    }
   