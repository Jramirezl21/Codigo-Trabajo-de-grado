### Objetivo 2
##Sub muestras de cada nivel de capacidad
##Submuestras por acp
lista_clasifACP <- split(Datos, Datos$DifCapAPC)
DesACPLEVE <- (lista_clasifACP$LEVE)[,c(29:73)]
DesACPMODERADA <- (lista_clasifACP$MODERADA)[,c(29:73)]
DesACPSEVERA <- (lista_clasifACP$SEVERA)[,c(29:73)]
DesACPNINGUNA <- (lista_clasifACP$NINGUNA)[,c(29:73)]



# Crear una función para calcular la cantidad de varianza explicada por el PCA
calcular_varianza_pca <- function(data) {
  # Realizar el PCA
  pca_result <- PCA(data, scale.unit = TRUE)
  
  # Extraer la cantidad de varianza explicada por cada componente principal
  var_exp <- pca_result$eig[,"percentage of variance"]
  
  return(var_exp)
}

# Número de submuestras
num_submuestras <- 1000

# Tamaño de cada submuestra
tam_submuestra <- 3000

# Lista para almacenar la cantidad de varianza explicada de cada submuestra
varExp_LeveD <- vector("list", num_submuestras)
carFact_LeveD<-  vector("list", num_submuestras)

calcular_cargas_pca <- function(data) {
  # Realizar el PCA
  pca_resultado <- PCA(data, scale.unit = TRUE)
  
  # Extraer la cantidad de varianza explicada por cada componente principal
  cargas <- pca_resultado$var$coord
  
  return(cargas)
}



# Ciclo para generar y analizar cada submuestra
for (i in 1:num_submuestras) {
  # Generar una submuestra aleatoria
  submuestra <- DesACPLEVE[sample(nrow(DesACPLEVE), tam_submuestra, replace = T), ]
  
  # Calcular la cantidad de varianza explicada por el PCA de la submuestra actual
  varExp_LeveD[[i]] <- calcular_varianza_pca(submuestra)
  carFact_LeveD[[i]] <- calcular_cargas_pca(submuestra)
}
