#DISTRIBUCIÓN BINOMIAL

# Puntual
# dbinom(x(número de éxitos), n(número de ensayos), p(probabilidad de éxito))

dbinom(3, 6, 0.2)
X = 0:6
Y = dbinom(X, 6, 0.3)
plot(X,Y)

# Acumulada
#pbinom(x(numero hasta acumular), n(número de ensayos), p(probabilidad de éxito)
pbinom(2,1,0.01)


# Distribución Poisson

  # Puntual
#dpois(x, lambda(mu))
  dpois(2,lambda=2.5)
  
  X = 0:10
  Y = dpois(X,lambda=2.5)
  plot(X,Y)
  
  # Acumulada
  #ppois(hasta x, lambda)
  ppois(4,lambda=6)
  
  # Inversa
  qpois(0.6,lambda=2.5)
  
  
  # Distribuciones Continuas
  
   # Distribución Normal
    
      # Puntual
  #dnorm(valor requerido, mu, desviación estandar)
      # dnorm(x,mean, sd)
      dnorm(11.5,0, 1)
      
      X = seq(-5,5,by=0.1)
      Y = dnorm(X,mean=0.0,sd=1.0)
      plot(X,Y,type="l")
      
      # Acumulada
      pnorm(0,0,0.3)
    
    
    # Distribución Uniforme
    
      # Puntual
      dunif(1.2,min=1,max=3)
      
      X = seq(-1,3,by=0.1)
      Y = dunif(X,min=0.0,max=2.0)
      plot(X,Y,type="l")
      
      # Acumulada
      # ¿Cuál es la probabilidad en la uniforme de [0,2]
      # hasta el valor x=1.3?
      punif(1.3,min=0.0,max=2.0)
      
      # Inversa
      # ¿Hasta qué valor de x la probabilidad
      # acumulada en la uniforme en [0,2] es 0.6?
      qunif(0.6,min=0.0,max=2.0)
   