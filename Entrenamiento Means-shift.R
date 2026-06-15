rm(list = ls())

##########################################################################################
###########################################################################################
######################## Proyectos de valorizacion (Entrenamiento)  ######################
############################################################################################
#########################################################################################



########################### Paquetes a importar ########################################
library(readxl)
library(randomForest)
library(class)
library(dplyr)
library(ggplot2)
library(tidyverse)

library(LPCM)


## Directorio 
setwd("C:/Users/MAYELIN/Downloads/R-Studio")

dir()

# llamamos a la base 

Base_buena<- read_excel("MUESTRA_INNOM.xlsx")

###### Limpieza de la Base 

# Ver los outliers
boxplot(Base_buena$RATIO_TOTAL)

# Calculos de los Cuartiles 
Q1 <- quantile(Base_buena$RATIO_TOTAL, 0.25)
Q3 <- quantile(Base_buena$RATIO_TOTAL, 0.75)
IQR_val <- Q3 - Q1
limite_inferior <- Q1 - 1.5 * IQR_val
limite_superior <- Q3 + 1.5 * IQR_val

# Saber donde quienes son los outliers 
filas_con_outliers_iqr <- Base_buena[Base_buena$RATIO_TOTAL < limite_inferior | Base_buena$RATIO_TOTAL > limite_superior, ]
# Hay 72 Outliers

# Saber las filas con outliers
print(filas_con_outliers_iqr)

# Si solo quieres los números de fila:
indices_outliers_iqr <- which(Base_buena$RATIO_TOTAL < limite_inferior | Base_buena$RATIO_TOTAL > limite_superior)
print(indices_outliers_iqr)

#Pertenecen a las filas 

#  8  41  43  46  56  60  62  63  68  83  85  91  96 106 109 118 126 127 136 140 146 149 152
#  153 156 166 174 178 180 182 188 190 198 217 234 235 237 246 258 267 277 282 284 287 297 303
#  305 307 310 311 312 315 317 323 325 328 333 339 347 372 377 389 399 464 472 473 477 484 485
# 487 492 493

# eliminamos 
filas_con_outliers_iqr

Base_buena<- Base_buena[-c(8,  41,  43,  46,  56,  60,  62,  63,  68,  83,  85,  91,  96, 106, 109, 118, 126, 127, 136, 140, 146, 149, 152,
                           153, 156, 166, 174, 178, 180, 182, 188, 190, 198, 217, 234, 235, 237, 246, 258, 267, 277, 282, 284, 287, 297, 303,
                           305, 307, 310, 311, 312, 315, 317, 323, 325, 328, 333, 339, 347, 372, 377, 389, 399, 464, 472, 473, 477, 484, 485,
                           487, 492, 493),]

# Comprobar el boxplot
boxplot(Base_buena$RATIO_TOTAL)




# Asumimos que tus datos están limpios en data_kmeans
modelo_ms <- ms(data_kmeans, h = 0.1)  # h = bandwidth, hay que probar valores

# Ver clusters detectados
table(modelo_ms$cluster.label)

# Visualizar
plot(data_kmeans, col = modelo_ms$cluster.label, pch = 19, main = "Clustering con Mean Shift")














Promedios_Rubro <- Base_buena %>%
  group_by(RUBRO) %>%    # calculo del promedios del ratio total por rubro 
  summarise(ratio_centro = mean(RATIO_TOTAL, na.rm = TRUE))

# left_join
Base_buena <- Base_buena %>%
  left_join(Promedios_Rubro, by = "RUBRO")


################## Modelo Mean Shift ###############################################

# Cargar la librería dplyr si aún no está cargada
library(dplyr)
library(ggplot2)

# 1. Preparar los datos: extraer los ratios únicos por rubro
datos_ratio <- Base_buena %>%
  group_by(RUBRO) %>%
  summarise(ratio_medio = mean(RATIO_TOTAL, na.rm = TRUE)) %>%
  pull(ratio_medio)

# Convertir a una matriz para cálculos de distancia
datos_matriz <- as.matrix(datos_ratio)

# 2. Definir la función de distancia (distancia euclidiana unidimensional)
distancia_euclidiana <- function(x, y) {
  return(sqrt(sum((x - y)^2)))
}

# 3. Implementar el algoritmo Mean Shift
mean_shift <- function(data, bw, tolerancia = 1e-6, max_iter = 100) {
  n_puntos <- nrow(data)
  centros <- data # Inicializar los centros con los puntos de datos
  historial_centros <- list(centros)
  
  for (iter in 1:max_iter) {
    nuevos_centros <- matrix(0, nrow = n_puntos, ncol = ncol(data))
    pesos <- matrix(0, nrow = n_puntos, ncol = 1)
    
    for (i in 1:n_puntos) {
      punto <- data[i, ]
      vecinos <- data[apply(data, 1, function(x) distancia_euclidiana(punto, x) < bw), , drop = FALSE]
      
      if (nrow(vecinos) > 0) {
        nuevos_centros[i, ] <- colMeans(vecinos)
      } else {
        nuevos_centros[i, ] <- punto # Si no hay vecinos, el centro se queda igual
      }
    }
    
    # Verificar convergencia
    cambio_maximo <- max(apply(abs(nuevos_centros - centros), 1, max))
    if (cambio_maximo < tolerancia) {
      break
    }
    
    centros <- nuevos_centros
    historial_centros[[iter + 1]] <- centros
  }
  
  # Identificar centros únicos (modos)
  centros_unicos_indices <- unique(round(centros, digits = 6), MARGIN = 1)
  modos <- centros[match(centros_unicos_indices, round(centros, digits = 6), nomatch = 0), ]
  
  # Asignar cada punto al modo más cercano
  asignaciones <- apply(data, 1, function(punto) {
    distancias <- apply(modos, 1, function(modo) distancia_euclidiana(punto, modo))
    return(which.min(distancias))
  })
  
  return(list(modos = modos, asignaciones = asignaciones, historial_centros = historial_centros))
}

# 4. Establecer diferentes valores de ancho de banda para experimentar
bandwidths_a_probar <- c(sd(datos_ratio) / 2, sd(datos_ratio), 0.1, 0.15, 0.2)

# 5. Iterar sobre los diferentes anchos de banda y mostrar los resultados
for (bw in bandwidths_a_probar) {
  cat(paste("\n--- Intentando con ancho de banda:", bw, "---\n"))
  
  resultado_ms <- tryCatch({
    mean_shift(datos_matriz, bw)
  }, error = function(e) {
    cat(paste("Error:", e$message, "\n"))
    return(NULL)
  })
  
  if (!is.null(resultado_ms)) {
    print("Centros de los clústeres (modos):")
    print(resultado_ms$modos)
    cat("Número de clústeres encontrados:", nrow(resultado_ms$modos), "\n")
    
    Base_agrupada_ms <- Base_buena %>%
      group_by(RUBRO) %>%
      summarise(ratio_medio = mean(RATIO_TOTAL, na.rm = TRUE)) %>%
      mutate(cluster_ms = factor(resultado_ms$asignaciones))
    print("\nAgrupamiento de rubros:")
    print(Base_agrupada_ms)
    
    # Visualización para cada ancho de banda
    p <- ggplot(Base_agrupada_ms, aes(x = ratio_medio, y = factor(1), color = cluster_ms)) +
      geom_point(size = 5) +
      geom_vline(xintercept = resultado_ms$modos, linetype = "dashed", color = "black") +
      labs(title = paste("Agrupamiento Mean Shift (BW =", round(bw, 2), ")"),
           x = "Ratio Total Medio",
           y = "") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    print(p)
  }
}

# Puedes seleccionar el ancho de banda que mejor se ajuste a tu interpretación de los clústeres.
# Una vez que decidas un ancho de banda, puedes volver a ejecutar el algoritmo con ese valor específico
# para obtener el agrupamiento final.

# Ejemplo de cómo ejecutar con un ancho de banda específico (comenta los bucles si lo haces):
# bandwidth_final <- 0.15 # Elige el ancho de banda que consideres mejor
# resultado_ms_final <- mean_shift(datos_matriz, bandwidth_final)
# print("\n--- Agrupamiento Final con Ancho de Banda:", bandwidth_final, "---\n")
# print("Centros de los clústeres (modos):")
# print(resultado_ms_final$modos)
# Base_agrupada_ms_final <- Base_buena %>%
#   group_by(RUBRO) %>%
#   summarise(ratio_medio = mean(RATIO_TOTAL, na.rm = TRUE)) %>%
#   mutate(cluster_ms = factor(resultado_ms_final$asignaciones))
# print("\nAgrupamiento de rubros:")
# print(Base_agrupada_ms_final)
#
# p_final <- ggplot(Base_agrupada_ms_final, aes(x = ratio_medio, y = factor(1), color = cluster_ms)) +
#   geom_point(size = 5) +
#   geom_vline(xintercept = resultado_ms_final$modos, linetype = "dashed", color = "black") +
#   labs(title = paste("Agrupamiento Mean Shift (BW =", round(bandwidth_final, 2), ")"),
#        x = "Ratio Total Medio",
#        y = "") +
#   theme_minimal() +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# print(p_final)

distancias <- apply(modos, 1, function(modo) distancia_euclidiana(punto, modo))


install.packages("LPCM")
library(LPCM)

# Ejecutar Mean Shift con una librería
resultado_libreria <- meanShift(datos_matriz, h = bandwidth) # 'h' es el ancho de banda

# Analizar los resultados de la librería
print("Centros de los clústeres (Librería):")
print(resultado_libreria$centers)

Base_agrupada_libreria <- Base_buena %>%
  group_by(RUBRO) %>%
  summarise(ratio_medio = mean(RATIO_TOTAL, na.rm = TRUE)) %>%
  mutate(cluster_libreria = factor(resultado_libreria$cluster.id))
print("\nAgrupamiento de rubros (Librería):")
print(Base_agrupada_libreria)

# Visualización con la librería
ggplot(Base_agrupada_libreria, aes(x = ratio_medio, y = factor(1), color = cluster_libreria)) +
  geom_point(size = 5) +
  geom_vline(xintercept = resultado_libreria$centers, linetype = "dashed", color = "black") +
  labs(title = paste("Agrupamiento Mean Shift (Librería, BW =", round(bandwidth, 2), ")"),
       x = "Ratio Total Medio",
       y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Corregir columna anidada




##########################################################################################
###########################################################################################
######################## Proyectos de valorizacion (Entrenamiento)  ######################
############################################################################################
#########################################################################################



########################### Paquetes a importar ########################################
library(readxl)
library(randomForest)
library(class)
library(dplyr)
library(ggplot2)
library(tidyverse)

library(LPCM)


## Directorio 
setwd("C:/Users/MAYELIN/Downloads/R-Studio")

dir()

# llamamos a la base 

Base_buena<- read_excel("MUESTRA_INNOM.xlsx")

###### Limpieza de la Base 

# Ver los outliers
boxplot(Base_buena$RATIO_TOTAL)

# Calculos de los Cuartiles 
Q1 <- quantile(Base_buena$RATIO_TOTAL, 0.25)
Q3 <- quantile(Base_buena$RATIO_TOTAL, 0.75)
IQR_val <- Q3 - Q1
limite_inferior <- Q1 - 1.5 * IQR_val
limite_superior <- Q3 + 1.5 * IQR_val

# Saber donde quienes son los outliers 
filas_con_outliers_iqr <- Base_buena[Base_buena$RATIO_TOTAL < limite_inferior | Base_buena$RATIO_TOTAL > limite_superior, ]
# Hay 72 Outliers

# Saber las filas con outliers
print(filas_con_outliers_iqr)

# Si solo quieres los números de fila:
indices_outliers_iqr <- which(Base_buena$RATIO_TOTAL < limite_inferior | Base_buena$RATIO_TOTAL > limite_superior)
print(indices_outliers_iqr)

#Pertenecen a las filas 

#  8  41  43  46  56  60  62  63  68  83  85  91  96 106 109 118 126 127 136 140 146 149 152
#  153 156 166 174 178 180 182 188 190 198 217 234 235 237 246 258 267 277 282 284 287 297 303
#  305 307 310 311 312 315 317 323 325 328 333 339 347 372 377 389 399 464 472 473 477 484 485
# 487 492 493

# eliminamos 
filas_con_outliers_iqr

Base_buena<- Base_buena[-c(8,  41,  43,  46,  56,  60,  62,  63,  68,  83,  85,  91,  96, 106, 109, 118, 126, 127, 136, 140, 146, 149, 152,
                           153, 156, 166, 174, 178, 180, 182, 188, 190, 198, 217, 234, 235, 237, 246, 258, 267, 277, 282, 284, 287, 297, 303,
                           305, 307, 310, 311, 312, 315, 317, 323, 325, 328, 333, 339, 347, 372, 377, 389, 399, 464, 472, 473, 477, 484, 485,
                           487, 492, 493),]

# Comprobar el boxplot
boxplot(Base_buena$RATIO_TOTAL)














#################################################################################################333
###################################################################################################3
#######################################################################################################
#########################################################################################################
#######################################################################################################





# Crear matriz deratio_centro# Crear matriz de input para clustering
data_ms <- Base_buena[, c("RATIO_TOTAL", "ratio_centro")]

# Eliminar filas con problemas
data_ms <- na.omit(data_ms)
data_ms <- data_ms[is.finite(rowSums(data_ms)), ]

library(LPCM)

modelo_ms <- LPCM::ms(data_ms, h = 0.008, main ="Modelo Means-shift ") 


# 3) Agregar cluster y mapear a rubros
Base_buena$cluster_ms <- modelo_ms$cluster.label
mapping <- Base_buena %>%
  group_by(cluster_ms) %>%
  summarise(rubro_pred = RUBRO[which.max(table(RUBRO))]) %>%
  deframe()

Base_buena$rubro_predicho <- mapping[as.character(Base_buena$cluster_ms)]


table(Base_buena$rubro_predicho)


df<- data.frame(Base_buena$RUBRO, Base_buena$rubro_predicho)

df$Coincidencia <- ifelse(df$Base_buena.RUBRO == df$Base_buena.rubro_predicho, 1, 0)
sum(df$Coincidencia)




############################### BASE del TESTEO ##############################################

library(dplyr)

# Asegurarse de tener cluster_ms y RATIO_TOTAL en Base_buena
centroides <- Base_buena %>%
  group_by(cluster_ms) %>%
  summarise(centroide = mean(RATIO_TOTAL, na.rm = TRUE)) %>%
  ungroup()

Base_mala <- readxl::read_excel("EMPRESAS_MIXTAS.xlsx")  # Cargar una base donde esten las bases mixtas 



# Función para encontrar el cluster más cercano
asignar_cluster <- function(ratio) {
  distancias <- abs(centroides$centroide - ratio)
  cluster_mas_cercano <- centroides$cluster_ms[which.min(distancias)]
  return(cluster_mas_cercano)
}

# Aplicar a cada empresa
Base_mala$cluster_ms <- sapply(Base_mala$RATIO_TOTAL, asignar_cluster)


Base_mala$rubro_asignado <- mapping[as.character(Base_mala$cluster_ms)]


























# Aplicar clustering (podés probar distintos valores de h)
modelo_ms <- ms(data_ms, h = 0.01)  # h = bandwidth

# Ver a qué cluster pertenece cada fila
clustering <- modelo_ms$cluster.label

# Añadir resultado a la base original
Base_buena_filtrada <- Base_buena[rownames(data_ms), ]  # sincronizar filas
Base_buena_filtrada$Cluster_MeanShift <- clustering


table(Base_buena_filtrada$Cluster_MeanShift, Base_buena_filtrada$RUBRO)








library(ggplot2)

ggplot(Base_buena_filtrada, aes(x = RATIO_TOTAL, y = ratio_centro, color = as.factor(Cluster_MeanShift))) +
  geom_point(size = 2) +
  labs(title = "Mean Shift Clustering", color = "Cluster") +
  theme_minimal()


install.packages("LPCM")
library(LPCM)
library(tibble)



# 1) Prepara la matriz
data_ms <- Base_buena[, c("RATIO_TOTAL")]
data_ms <- na.omit(data_ms)

# 2) Mean Shift
modelo_ms <- ms(data_ms, h = 0.1)

# 3) Agregar cluster y mapear a rubros
Base_buena$cluster_ms <- modelo_ms$cluster.label
mapping <- Base_buena %>%
  group_by(cluster_ms) %>%
  summarise(rubro_pred = RUBRO[which.max(table(RUBRO))]) %>%
  deframe()

Base_buena$rubro_predicho <- mapping[as.character(Base_buena$cluster_ms)]

# 4) Para la base mala, usas predict() en Python o la lógica de nearest-centroid en R:
#    (calcular distancia a cada centroide de cluster_ms y asignar el rubro_predicho)

























