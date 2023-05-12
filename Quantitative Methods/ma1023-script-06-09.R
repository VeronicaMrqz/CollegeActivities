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
  setwd("~/Metodos")
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
    hist(datos[,10])
    plot(datos[,10], main = "Gráfico de barras 1",
         xlab = "Género", ylab = "Frecuencia")
    Hombres= 54171 
    Mujeres = 20476
    x = c(54171, 20476)
    etiq = c("hombre", "mujer")
    
   barplot(x,  
          xlab= ("Aportantes del ingreso familiar"),
          ylab = "Frecuencia",
          col = c("cornflowerblue","pink"),
          main = "¿Quiénes proveen el ingreso?")
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

# Intervalos de Confianza
{
  qnorm(0.975)
}
#  Seleccionar una variable de trabajo
{
  colIngresoCorriente = 23
  # Corroborar el nomber le la columna
  #  colnames(datos) da la lista con los nombres de las columnas
  colnames(datos)[colIngresoCorriente]
  ingresosCor = datos[   , colIngresoCorriente]
  hist(ingresosCor)
  resumen = summary(ingresosCor); resumen
  primerCuartil = resumen[2]; primerCuartil
  tercerCuartil = resumen[5]; tercerCuartil
  rangoIntercuartil = tercerCuartil-primerCuartil; rangoIntercuartil 
  limiteSup = tercerCuartil + 1.5*rangoIntercuartil
  limiteInf = primerCuartil - 1.5*rangoIntercuartil
  popTrabajar = ingresosCor[(ingresosCor < limiteSup) & (ingresosCor > limiteInf)]
  hist(popTrabajar)  
  summary(popTrabajar)
  muPop = mean(popTrabajar); muPop

  m = length(popTrabajar); m  
  
  # Tamaño de la muestra
  nMuestra = 300   
  registrosMuestra = sample(1:m,nMuestra)
  
  muestra = popTrabajar[registrosMuestra]
  summary(muestra)
  xBarra = mean(muestra); xBarra
  sMuestra = sd(muestra); sMuestra
  
  alfa = 0.05
  zc = qnorm(1-alfa/2); zc
  
  tc = qt(1-alfa/2,nMuestra-1); tc

  # Intervalo 
  c(xBarra-sMuestra/sqrt(nMuestra)*zc,
     xBarra+sMuestra/sqrt(nMuestra)*zc)
  c(xBarra-sMuestra/sqrt(nMuestra)*tc,
    xBarra+sMuestra/sqrt(nMuestra)*tc)
  muPop    
  
  t.test(muestra, mu = muPop, alternative = "two.sided")
}

{
  install.packages("Rmisc")
  library(Rmisc)
  CI(muestra,0.95)
}

# Datos generales de la variable seleccionada
{
  datos[,numDatos = length(ingresosCor); numDatos
  summary(ingresosCor)
  mean(ingresosCor)
  sd(ingresosCor)
}

# Trabajo sobre una muestra de la variable seleccionada
{
  nMuestra = 1000
  registrosMuestra = sample(1:numDatos,nMuestra)
  muestra = ingresosCor[registrosMuestra]; muestra
  xbarra = mean(muestra)
  s = sd(ingresosCor)
  xbarra + s/sqrt(nMuestra)*1.96
  xbarra - s/sqrt(nMuestra)*1.96
  qnorm(0.95)
  # par(mfrow=c(1,1))
  hist(muestra)
}

##########################
# Como buscar en los datos
{
  pobInteres = datos[,10]
  datosP = table(pobInteres)
  datosP[2]/length(pobInteres)
  nMuestra = 100
  muestra = sample(  pobInteres  ,  nMuestra)
  enMuestra = table(muestra); enMuestra
  pG = enMuestra[2]/nMuestra; pG
  
  alfa = 0.05
  zc = qnorm(1-alfa/2); zc
  ancho = zc*sqrt(pG*(1-pG)/nMuestra)
  c(pG-ancho,pG+ancho)
}


