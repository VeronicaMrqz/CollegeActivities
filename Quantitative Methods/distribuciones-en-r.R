   #################################
#
# Distribuciones de Probabilidad en R
# 
#

#################################
# Distribuciones Discretas
{
  # Distribución Binomial
  {
    # Fuente: 
    {
      #https://www.tutorialspoint.com/r/r_binomial_distribution.htm
    }
    # Puntual
    dbinom(4, 6, 0.3)
    X = 0:6
    Y = dbinom(X, 6, 0.3)
    plot(X,Y)
    
    # Acumulada
    pbinom(4,6,0.3)
  }
  
  # Distribución Hipergeométrica
  {
    # Fuente: 
    {
      #https://stat.ethz.ch/R-manual/R-patched/library/stats//html/Hypergeometric.html
    }
    # Puntual
    dhyper(
      x = número de bolas blancas tomadas,
      m = numero de bolas blancas en la urna,
      n = número de bolas negras en la urna,
      k = número de bolas tomadas de la urna
    )
    dhyper(1,5,5,1)
    
    # Acumulada
    phyper(2,5,5,3)
    dhyper(0,5,5,3)+dhyper(1,5,5,3)+dhyper(2,5,5,3)
  }
  
  # Distribución Poisson
  {
    # Fuente: 
    {
      #http://www.r-tutor.com/elementary-statistics/probability-distributions/poisson-distribution
    }
    # Puntual
    dpois(2,lambda=2.5)
    
    X = 0:10
    Y = dpois(X,lambda=2.5)
    plot(X,Y)
    
    # Acumulada
    ppois(4,lambda=2.5)
    
    # Inversa
    qpois(0.6,lambda=2.5)
  }
}

#################################
# Distribuciones Continuas
{
  # Distribución Normal
  {
    # Fuente: 
    {
      #https://www.tutorialspoint.com/r/r_normal_distribution.htm
    }
    # Puntual
    # dnorm(x,mean, sd)
    dnorm(1.,1,0.5)
    
    X = seq(-5,5,by=0.1)
    Y = dnorm(X,mean=0.0,sd=1.0)
    plot(X,Y,type="l")
    
    # Acumulada
    pnorm(0,0,0.3)
  }
  
  # Distribución Uniforme
  {
    # Fuente: 
    {
      ?dunif
    }
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
  }
}
