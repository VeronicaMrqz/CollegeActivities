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
  
}

#  Seleccionar una variable de trabajo
{
  colIngresoCorriente = 23
  # Corroborar el nomber le la columna
  #  colnames(datos) da la lista con los nombres de las columnas
  colnames(datos)[colIngresoCorriente]
  
  ingresosCor = datos[,colIngresoCorriente]
  
}

# Datos generales de la variable seleccionada
{
  numDatos = length(ingresosCor); numDatos
  summary(ingresosCor)
  mean(ingresosCor)
  sd(ingresosCor)
}

# Trabajo sobre una muestra de la variable seleccionada
{
  nMuestra = 40
  registrosMuestra = sample(1:numDatos,nMuestra)
  registrosMuestra
  muestra = ingresosCor[registrosMuestra]; muestra
  summary(muestra)
  mean(muestra)
  sd(ingresosCor)
  # par(mfrow=c(1,1))
  hist(muestra)
}

# Como buscar en los datos
{
  # Cómo filtrar los datos que están por abajo de un valor
  aux = ingresosCor[ingresosCor < 20000]
  length(aux)
  
  # Como filtrar los datos que están por arriba de un valor
  aux = ingresosCor[ingresosCor > 30000]  
  length(aux)
}