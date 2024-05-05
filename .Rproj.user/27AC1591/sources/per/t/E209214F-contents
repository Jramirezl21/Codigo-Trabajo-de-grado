#Librerias ----
library(whomds)
library(EFAtools)
library(psych)
library(haven)
library(polycor)
library(ggcorrplot)
library(GPArotation)
library(psych)
library(lavaan)
library(polycor)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(ade4)
library(FactoMineR)
library(missMDA)
library(dplyr)

Datos <- read.csv("datos.csv")
DatosCap <- Datos[,c(5:26)]
# Instalar y cargar las librerías necesarias
install.packages("FactoMineR")
library(FactoMineR)

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
varianza_explicada <- vector("list", num_submuestras)
cargas_fact <-  vector("list", num_submuestras)

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
  submuestra <- DatosCap[sample(nrow(DatosCap), tam_submuestra, replace = FALSE), ]
  
  # Calcular la cantidad de varianza explicada por el PCA de la submuestra actual
  varianza_explicada[[i]] <- calcular_varianza_pca(submuestra)
  cargas_fact [[i]] <- calcular_cargas_pca(submuestra)
}

#Segunda submuestra

# Tamaño de cada submuestra
tam_submuestra2 <- 1750
varianza_explicada2 <- vector("list", num_submuestras)
cargas_fact2 <-  vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  # Generar una submuestra aleatoria
  submuestra2 <- DatosCap[sample(nrow(DatosCap), tam_submuestra2, replace = FALSE), ]
  
  # Calcular la cantidad de varianza explicada por el PCA de la submuestra actual
  varianza_explicada2[[i]] <- calcular_varianza_pca(submuestra2)
  cargas_fact2[[i]] <- calcular_cargas_pca(submuestra2)
}
indice_maximo <- which.max(sapply(varianza_explicada, `[`, 1))
varianza_explicada[[730]]
indice_maximo2 <- which.max(sapply(varianza_explicada2, `[`, 1))
varianza_explicada2[[697]]
### Tercera y cuarta submuestra
rm(tamano_muestra3)
# Tamaño de la muestra y número de submuestras
tamano_submuestra <- 2377
lista_submuestras <- vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  muestra <- sample(1:4754, tamano_submuestra, replace = FALSE)
  lista_submuestras[[i]] <- DatosCap[muestra, ]
}

var_acp3 <- vector("list",num_submuestras)
cargas_acp3 <- vector("list", num_submuestras)

for (i in 1:num_submuestras) {
  var_acp3[[i]] <- PCA(lista_submuestras[[i]], graph = FALSE)$eig
  cargas_acp3[[i]] <- PCA(lista_submuestras[[i]], graph = FALSE)$var$coord
}

indice_max_varianza3 <- which.max(sapply(var_acp3, `[`, 1))
var_acp3[[778]]
cargas_acp3[[778]]

individuos_seleccionados <- rownames(lista_submuestras[[778]])
individuos_faltantes <- setdiff(1:4754, individuos_seleccionados)
nueva_muestra <- DatosCap[individuos_faltantes, ]
acp4 <- PCA(nueva_muestra)
var_acp4 <- acp4$eig
cargas_acp4 <- acp4$var$coord
##### Comparacion varianzas explicadas
varianza_explicada[[730]]
varianza_explicada2[[697]]
var_acp3[[778]]
var_acp4 <- acp4$eig
#Seleccionar el de mayor varianza explicada, la muestra 2
cargas1 <-  data.frame(cargas_fact[[730]])$Dim.1
cargas2 <- data.frame(cargas_fact2[[697]])$Dim.1
cargas3 <- data.frame(cargas_acp3[[778]])$Dim.1
cargas4 <- data.frame(cargas_acp4)$Dim.1

puntuaciones1 <- rowSums(cargas1* DatosCap)
puntuaciones2 <- rowSums(cargas2* DatosCap)
puntuaciones3 <- rowSums(cargas3* DatosCap)
puntuaciones4 <- rowSums(cargas4* DatosCap)

#Se selecciona el intervalo mas grande en puntuaciones
vector1 <- c(min(puntuaciones1),max(puntuaciones1));max(puntuaciones1)-min(puntuaciones1)
vector2 <- c(min(puntuaciones2),max(puntuaciones2));max(puntuaciones2)-min(puntuaciones2)
vector3 <- c(min(puntuaciones3),max(puntuaciones3));max(puntuaciones3)-min(puntuaciones3)
vector4 <- c(min(puntuaciones4),max(puntuaciones4));max(puntuaciones4)-min(puntuaciones4)
#Se selecciona el indicador o puntuaciones 2
DatosCap$PuntajeCap <- puntuaciones2 

#kmeansDatosCap <- kmeans(DatosCap, 4, iter.max = 1000, nstart = 1)
#plot(puntuaciones2 , col = kmeans$cluster, main = "Clustering con K-means", xlab = "Índice", ylab = "Valor")
#points(clusters$centers, col = 1:k, pch = 8, cex = 2)
#legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 8, cex = 0.8)


#fviz_cluster(kmeansDatosCap,data=DatosCap)
fviz_cluster(kmeansPun2 ,data=DatosCap)

kmeansPun2 <- kmeans(puntuaciones2 , 4, iter.max = 1000, nstart = 1)
clustersPun2 <- data.frame(cbind(kmeansPun2$cluster,puntuaciones2))
#clustersDatosCap <- data.frame(cbind(kmeansDatosCap$cluster,puntuaciones2))

# Calcular el mínimo de puntación para cada clasificación
###Puntajes de corte
minimos_por_clasificacion <- aggregate(clustersPun2$puntuaciones2, 
                                       by = list(clasificacion = clustersPun2$V1), FUN = min)

# Renombrar las columnas
colnames(minimos_por_clasificacion) <- c("Custer", "Punto_Corte")
minimos_por_clasificacion$Clasificacion <- c("LEVE","GRAVE","SEVERA","NINGUNA")

# Calcular el mínimo de puntación para cada clasificación
#minimos_por_clasificacionCap <- aggregate(clustersDatosCap$puntuaciones2, 
#                                      by = list(clasificacion = clustersDatosCap$V1), FUN = min)

# Renombrar las columnas
#colnames(minimos_por_clasificacionCap) <- c("Clasificacion", "Minimo_Puntaje")

##Tomo los cluster por puntaciones unicamente
#max_por_clasificacion <- aggregate(clustersPun2$puntuaciones2, 
#                     by = list(clasificacion = clustersPun2$V1),FUN = max)

# Obtener centroides

fviz_cluster(kmeansPun2 ,data=DatosCap)
hist(puntuaciones2)
#Transformacion del puntaje
#################################
#Puntaje euristico
DatosCap$PuntajeCapH <- Puntaje_euristico
Puntaje_heuristico <- rowSums(DatosCap[,c(1:22)])
hist(Puntaje_euristico )
cor_spearman <- cor(puntuaciones2,Puntaje_euristico , method = "spearman")
plot(puntuaciones2~Puntaje_euristico)
#Cluster puntaje heuristico

kmeansHeuris <- kmeans(Puntaje_euristico,4, iter.max = 1000, nstart = 1)
fviz_cluster(kmeansHeuris,data=DatosCap)
clustersHeuris<- data.frame(cbind(kmeansHeuris$cluster,Puntaje_euristico))
minimos_por_clasificacionHeuris <- aggregate(clustersHeuris$Puntaje_euristico, 
                                       by = list(clasificacion = clustersHeuris$V1), FUN = min)

colnames(minimos_por_clasificacionHeuris) <- c("Cluster", "Punto_Corte")
minimos_por_clasificacionHeuris$Clasificacion <- c("LEVE","MODERADA","SEVERA","NINGUNA")


#Clasificaciones Dificultad en Capacidad 
clustersHeuris$V1#HEURIS 3 GRAVE 2 MODERADA 1 LEVE 4 NINGUNA
clustersPun2$V1#ACP 2 GRAVE 3 MODERADA 1 LEVE 4 NINGUNA

DifCapAPC <- ifelse(clustersPun2$V1 == 1, "LEVE",
                    ifelse(clustersPun2$V1 == 2, "SEVERA",
                    ifelse(clustersPun2$V1 == 3, "MODERADA",
                    ifelse(clustersPun2$V1 == 4, "NINGUNA", NA))))

DifCapHeuris <- ifelse(clustersHeuris$V1 == 1, "LEVE",
                    ifelse(clustersHeuris$V1 == 2, "MODERADA",
                    ifelse(clustersHeuris$V1 == 3, "SEVERA",
                    ifelse(clustersHeuris$V1== 4, "NINGUNA", NA))))
Datos <- cbind(Datos,puntuaciones2,DifCapAPC,Puntaje_heuristico,DifCapHeuris)


