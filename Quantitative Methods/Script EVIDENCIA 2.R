######################################
            #EVIDENCIA 2
######################################


#Cargar base de datos.
setwd("~/Metodos")
archivo = "concentrado-2018.csv"
datos = read.csv(archivo , header=TRUE)
View(datos)

#INTERVALOS DE CONFIANZA

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
hist(popTrabajar,
     xlab = "Ingresos",
     ylab = "Frecuencia",
     col = "cornflowerblue",
     main = "Distribución de ingresos de la población reducida") #distribución normal

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
  #PRUEBA DE HIPÓTESIS DE HOGARES POBRES       #lo que falta por hacer
########################################

# Trabajo con una muestra
{
  # Fijando un nivel de confianza
  {
    alfa = 0.05
    # Prueba de dos colas
  }
  limite = 0.6*medianaPop
  
  # Intervalo de confianza
  {
    nMuestra = 300
    muestra = sample(popTrabajar,nMuestra)
    cualesCumplen = muestra[muestra < limite]
    pGorrito = length(cualesCumplen)/nMuestra
    pGorrito
    zc = qnorm(1-alfa/2); zc
    intervalo = pGorrito + zc*sqrt(pGorrito*(1-pGorrito)/nMuestra)*c(-1,1)
    intervalo
    
    
    # Para prueba de usa cola
    zc = qnorm(1-alfa); zc
    # H0: México no es pobre: pPob <= 0.20
    # Alternativa: pPop > 0.2 
    pp = 0.20 # Consideración de la linea de Pobreza
    # Que cuando un país tiene por arriba de ese porcentaje de hogares
    # se considera pobre.
    
    z0 = (pGorrito-pp)/sqrt(pp*(1-pp)/n); z0 
    
    # Valor de p
    pValor = pnorm(z0); valorP
    
    # Extremo izquierdo 
    C1 = pp - zc*sqrt(pp*(1-pp)/n); C1
    # Extremo derecho 
    C2 = pp + zc*sqrt(pp*(1-pp)/n); C2
    
    # Regla de decisión: Caer en la región de rechazo
    # La región de rechazo es (C2,infinito): Cae allí?
    pGorrito > C2
    c(pGorrito,C2)
    # Regla de decisión: Tener un z0 abajo de zc
    z0 > zc
    
    # Regla de decisión: el valor p es menor que alfa?
    pValor < alfa
  }
  
  
  #############################################################  
  # Prueba de hipótesis para proporciones (JEFES DE FAMILIA)       #falta también, lo incluimos?
  #############################################################  
  {
    # Recuerde: la columna 10 contiene el sexo del jefe de familia.
    misDatos = datos[,10]
    table(misDatos)
    pPoblacional = sum(misDatos==2) / length(misDatos); pPoblacional
    ### z0 = (pGorrito-p0)/sqrt(p0*(1-p0)/n); z0
    
    
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
    
    # Afirmamos que A LO MAS ES 25% de los hogares en México tiene como jefe de familia una mujer
    # pPoblacional <= 0.25
    {
      p0 = 0.25
      
      alfa = 0.05
      zAlfa = qnorm(1-alfa); zAlfa
      
      # Tamaño de la muestra
      n = 300
      muestra = sample(misDatos,n)
      pGorrito = sum(muestra == 2)/n; pGorrito
      z0 = (pGorrito-p0)/sqrt(p0*(1-p0)/n); z0
      
      # Regla de decisión basada en la región de rechazo
      # Extremos de la región de decisión
      C2 = p0 - zAlfa*sqrt(p0*(1-p0)/n); C2
      C2 < z0
      
      # Regla de decisión basada en z0
      z0 < zAlfa
      
      # Regla de decisión basada en el pValor
      pValor = pnorm(-z0); pValor
      pValor < alfa
    }
    
    # Afirmamos que AL MENOS EL 25% de los hogares en México tiene como jefe de familia una mujer
    # pPoblacional >= 0.25
    {
      p0 = 0.25
      
      alfa = 0.05
      zAlfa = qnorm(1-alfa); zAlfa
      # Tamaño de la muestra
      n = 300
      muestra = sample(misDatos,n)
      pGorrito = sum(muestra == 2)/n; pGorrito
      
      # Regla de decisión basada en la región de rechazo
      # Extremos de la región de decisión
      C1 = p0 - zAlfa*sqrt(p0*(1-p0)/n); C1
      pGorrito < C1
      
      # Regla de decisión basada en z0
      z0 = (pGorrito-p0)/sqrt(p0*(1-p0)/n); z0
      z0 > zAlfa
      
      # Regla de decisión basada en el pValor
      pValor = pnorm(-z0); pValor
      pValor < alfa
    }
  }    
  
  
  