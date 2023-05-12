#######################
# LA POBREZA EN MEXICO
#######################
# Sitio: https://www.inegi.org.mx/programas/enigh/nc/2018/default.html
# De: "Datos abiertos"
# Archivo "conjunto_de_datos_enigh_2018_ns_csv.zip"
# Al descomprimirlo,
# 1) De la carpeta "conjunto_de_datos_concentradohogar_enigh_2018_ns"
# 2) De allí y en la carpeta "conjunto_de_datos"
# 3) El archivo "conjunto_de_datos_concentradohogar_enigh_2018_ns.csv"
#    se copia a un directorio de trabajo con el nombre "concentrado-2018.csv"
# 4) Para ubicacion geográfica: copiar de "catalogos" 
#    los archivos "ubic_geo.csv" y "tam_loc.csv" al directorio de trabajo
# 

# Cargar la base de datos
{
  setwd("C:/usuarios/uresti/cursos/ma1023/Scitps-R/ENIGH")
  archivo = "concentrado-2018.csv"
  datos = read.csv(archivo , header=TRUE)
  head(datos)
  # Nombres de las Columnas
  colnames(datos)
  
  # Algunas columnas de interés
  {# Columna "tam_loc" = Tamaño de la localidad
    # Columna 4
    # Códigos:
    # 1: Localidades con 100 000 y más habitantes
    # 2: Localidades con 15 000 a 99 999 habitantes
    # 3: Localidades con 2 500 a 14 999 habitantes
    # 4: Localidades con menos de 2 500 habitantes
    summary(datos[,4])
    # Tabla de totales
    table(datos[,4])
  }
  
  {# Columna "sexo_jefe" = Sexo del jefe del hogar
    # Columna 10
    # Códigos:
    # 1: Hombre
    # 2: Mujer
    summary(datos[   , 10])    
    table(datos[,10])
  }
  
  {# Columna "educa_jefe" =  jefe del hogar
    # Columna 12
    # Códigos:
    # 1	Sin instrucción
    # 2	Preescolar
    # 3	Primaria incompleta
    # 4	Primaria completa
    # 5	Secundaria incompleta
    # 6	Secundaria completa
    # 7	Preparatoria incompleta
    # 8	Preparatoria completa
    # 9	Profesional incompleta
    # 10	Profesional completa
    # 11	Posgrado
    table(datos[,12])
  }
}


# Clase del 11 de Junio
{
  # La población completa
  {
    colIngresoCorriente = 23
    # Corroborar el nomber le la columna
    #  colnames(datos) da la lista con los nombres de las columnas
    colnames(datos)[colIngresoCorriente]
    ingresosCor = datos[   , colIngresoCorriente]
    hist(ingresosCor)
    resumen = summary(ingresosCor); resumen
  }
  
  # La poblacion reducida (quitando los extremos)
  {
    primerCuartil = resumen[2]; primerCuartil
    tercerCuartil = resumen[5]; tercerCuartil
    rangoIntercuartil = tercerCuartil-primerCuartil; rangoIntercuartil 
    limiteSup = tercerCuartil + 1.5*rangoIntercuartil
    limiteInf = primerCuartil - 1.5*rangoIntercuartil
    popTrabajar = ingresosCor[(ingresosCor < limiteSup) & (ingresosCor > limiteInf)]
    hist(popTrabajar)  
    summary(popTrabajar)
    muPop = mean(popTrabajar); muPop
    medianaPop = median(popTrabajar)
  }

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
    
    # Prueba de Hipótesis. Ejemplo de la clase. 
    # El problema de la votacion
    {
       n = 2000
       m = 1550
       alfa = 0.05
       zc = qnorm(1-alfa/2); zc
       
       pp = 0.80 
       pGorrito = m/n
       z0 = (pGorrito-pp)/sqrt(pp*(1-pp)/n); z0 
       
       # Valor de p
       pValor = pnorm(z0); valorP
       
       # Extremo izquierdo 
       C1 = pp-zc*sqrt(pp*(1-pp)/n); C1
       
       # Regla de decisión: Caer en la región de rechazo
       # La región de rechacho es (-infinito,C1): Cae allí?
       pGorrito < C1
       
      # Regla de decisión: Tener un z0 abajo de zc
       z0 < zc
  
       # Regla de decisión: el valor p es menor que alfa?
       pValor < alfa
       
    }    
    
    # Prueba de Hipótesis. Ejemplo de la clase. 
    # El problema de "Pollos Deliciosos"
    {
      n = 100
      m = 82
      alfa = 0.1
      zc = qnorm(1-alfa/2); zc
      
      pp = 0.90 
      pGorrito = m/n
      z0 = (pGorrito-pp)/sqrt(pp*(1-pp)/n); z0 
      
      # Valor de p
      pValor = pnorm(z0); valorP
      
      # Extremo izquierdo 
      C1 = pp-zc*sqrt(pp*(1-pp)/n); C1
      
      # Regla de decisión: Caer en la región de rechazo
      # La región de rechacho es (-infinito,C1): Cae allí?
      pGorrito < C1
      
      # Regla de decisión: Tener un z0 abajo de zc
      z0 < zc
      
      # Regla de decisión: el valor p es menor que alfa?
      pValor < alfa
      
    }    
    
    # El problema de los fumadores. No se concluyó
    {
      install.packages("Rlab")
      require(Rlab)
      X = rbern(200,p=0.3)
    }
      
  }
}  

#############################################################  
# Prueba de hipótesis para proporciones
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