#
{
  directorioDatos <- "C:/usuarios/uresti/cursos/ma1023/conjunto_de_datos_concentradohogar_enigh_2018_ns/conjunto_de_datos"
  nombreArchivo <- "conjunto_de_datos_concentradohogar_enigh_2018_ns.csv"
  setwd(directorioDatos)
  datosENIGH <- read.csv(nombreArchivo,header = TRUE )
  nrow(datosENIGH)
  ncol(datosENIGH)
}

{
  directorioCatalogos <- "C:/usuarios/uresti/cursos/ma1023/conjunto_de_datos_concentradohogar_enigh_2018_ns/catalogos";
  setwd(directorioCatalogos)
  ubicacionGeoArchivo <- "ubic_geo.csv"
  ubicacionGeoDatos <- read.csv(ubicacionGeoArchivo)
  colnames(ubicacionGeoDatos)
  estados <- unique(ubicacionGeoDatos[,3])
  posicion <- 1:length(estados)
  estadosP <- data.frame(posicion=posicion,estado=estados)
  norte <- c(3,4,9,6,11,20,26,27,29)
  municipiosNorte <- ubicacionGeoDatos[ubicacionGeoDatos[,"desc_ent"] %in% estados[norte],]
  municipiosN <- municipiosNorte[,1]
  aux <- datosENIGH[datosENIGH[,3] %in% municipiosN, ]
  colnames(aux)[60]
  mean(aux[,60])
}
gastoCe <- datosENIGH[ , "cereales"]
hist(gastoCe,
     xlab = "Gasto en Cereales",
     ylab="Frecuencia",
     main="Histograma de Frecuencias"
     )
mean(gastoCe)

