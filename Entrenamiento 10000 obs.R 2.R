###################################################################################################
#####################################################################################################
###########################    Base de Entrenamiento    #############################################
#####################################################################################################
####################################################################################################


# Para 10000 obs

rm(list = ls()) ###ESTE SI

# Librearia a cargar
library(readxl)
library(randomForest)
library(class)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(LPCM)
library(FactoMineR)
library(factoextra)
library(naniar)
library(scales)
library(ggfortify)
library(LPCM)
library(ggplot2)
library(scales)
library(gridExtra)
library(dplyr)
library(dbscan)
library(fpc)
library(dbscan)

## Directorio 
setwd("C:/Users/MAYELIN/Downloads/R-Studio")

dir()

# Cargar la base de datos
Base <- read_excel("ENTRENAMIENTO_INNOM 1.xlsx")

#Descripcion de la Base
colnames(Base)
table(Base$RUBRO)
unique(Base$RUBRO)
colnames(Base)
sum(is.na(Base))


#####################################  TRATAMIENTO DE LA BASE (Valores Perdidos)  ############################

#Suma de NA por columnas 
colSums(is.na(Base))
cat("Número de valores faltantes (NA) por columna:\n")
print(colSums(is.na(Base)))

# Porcentaje de valores faltantes por columna
cat("\nPorcentaje de valores faltantes (NA) por columna:\n")
print(colMeans(is.na(Base)) * 100)

#Visualizacion de los Na por Columna 
gg_miss_var(Base) + labs(y = "Número de valores faltantes",
       title = "Distribución de Valores Faltantes por Variable")


# idenficar las filas donde esta los NA para remplazarlas por el promedio

filas_con_algun_na <- which(apply(is.na(Base), 1, any))
filas_con_algun_na

# Calcular el promedio de CANTIDAD_SOCIOS por RUBRO
promedio_socios_por_rubro <- Base %>%
  group_by(RUBRO) %>%
  summarise(Promedio_Cantidad_Socios = mean(CANTIDAD_SOCIOS, na.rm = TRUE),
            Conteo_Rubro = n())

# Unir la base de datos original con los promedios por rubro
Base_imputada <- Base %>%
  left_join(promedio_socios_por_rubro, by = "RUBRO") %>%
  mutate(CANTIDAD_SOCIOS = ifelse(is.na(CANTIDAD_SOCIOS), Promedio_Cantidad_Socios, CANTIDAD_SOCIOS)) %>%
  select(-Promedio_Cantidad_Socios) # Eliminar la columna temporal del promedio


#Volvemos a remplezar la base 
Base <- Base_imputada
Base<- Base[,-c(12)]
rm(Base_imputada)

#Verificar que ya no hay NA
gg_miss_var(Base) + labs(y = "Número de valores faltantes",
                         title = "Distribución de Valores Faltantes por Variable")

sum(is.na(Base))


#check

#################################  TRATAMIENTO DE LA BASE (ouliers)   ########################################


# Columnas que son numericas
columnas_analizar <- c("CANTIDAD_TRABAJADOR", "CANTIDAD_SOCIOS", "INGRESOS",
                       "ACTIVOS_TOTALES", "RATIO_ING_COST", "PROMEDIO_5AT",
                       "EDAD_SOCIEDAD")

# ver boxplot por variables para hacerse una idea de la distribución
boxplot(Base$CANTIDAD_TRABAJADOR, main= "Boxplot Cantidad de Trabajadores"  )
boxplot(Base$CANTIDAD_SOCIOS, main= "Boxplot Cantidad de Socios ")
boxplot(Base$INGRESOS, main= "Boxplot Ingresos" )
boxplot(Base$ACTIVOS_TOTALES, main= "Boxplot Act. Totales")
boxplot(Base$RATIO_ING_COST, main= "Boxplot Ratio Ingreso/costo")
boxplot(Base$PROMEDIO_5AT, main= "Boxplot Promedio 5AT")
boxplot(Base$EDAD_SOCIEDAD,, main= "Boxplot Edad Sociedades") # más equilibrado


# ver plot distribucion de las variables
plot(Base$CANTIDAD_TRABAJADOR, main = "Distribución Cantidad de Trabajores ")
plot(Base$CANTIDAD_SOCIOS,main = "Distribución Cantidad de Socios ") 
plot(Base$INGRESOS,main = "Distribución Ingresos ") 
plot(Base$ACTIVOS_TOTALES,main = "Distribución Act Totales ") 
plot(Base$RATIO_ING_COST, main = "Distribución Ingreso Costo ") 
plot(Base$PROMEDIO_5AT, main = "Distribución Promedio5_AT ") 
plot(Base$EDAD_SOCIEDAD,main = "Distribución Edad Sociedades ")



########################### Crear funcion que que remplezara las ouliers por el valor maximo de la caja 

# Función para obtener los límites del IQR
obtener_limites_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  limite_inferior <- q1 - 1.5 * iqr
  limite_superior <- q3 + 1.5 * iqr
  return(list(inferior = limite_inferior, superior = limite_superior))
}

# Función para aplicar la winsorización y mostrar los límites
winsorizar_y_mostrar_limites <- function(data, columna) {
  if (is.numeric(data[[columna]])) {
    limites <- obtener_limites_iqr(data[[columna]])
    limite_inferior <- limites$inferior
    limite_superior <- limites$superior
    
    data[[columna]] <- ifelse(data[[columna]] < limite_inferior, limite_inferior,
                              ifelse(data[[columna]] > limite_superior, limite_superior, data[[columna]]))
    
    cat(sprintf("Columna: '%s'\n", columna))
    cat(sprintf("  Límite Inferior (IQR): %f\n", limite_inferior))
    cat(sprintf("  Límite Superior (IQR): %f\n", limite_superior))
    cat("----------------------------------------\n")
  } else {
    cat(sprintf("La columna '%s' no es numérica y no se winsorizó.\n", columna))
    cat("----------------------------------------\n")
  }
  return(data)
}

# Columnas a las que aplicaremos la winsorización
columnas_a_winsorizar <- columnas_analizar

# Aplicar la winsorización y mostrar los límites para cada columna
for (col in columnas_a_winsorizar) {
  Base <- winsorizar_y_mostrar_limites(Base, col)
}

# Mostrar un resumen de las columnas winsorizadas (opcional)
summary(Base[, columnas_a_winsorizar])



# ver boxplot por variables para hacerse una idea de la distribución
boxplot(Base$CANTIDAD_TRABAJADOR, main= "Boxplot Cantidad de Trabajadores"  )
boxplot(Base$CANTIDAD_SOCIOS, main= "Boxplot Cantidad de Socios ")
boxplot(Base$INGRESOS, main= "Boxplot Ingresos" )
boxplot(Base$ACTIVOS_TOTALES, main= "Boxplot Act. Totales")
boxplot(Base$RATIO_ING_COST, main= "Boxplot Ratio Ingreso/costo")
boxplot(Base$PROMEDIO_5AT, main= "Boxplot Promedio 5AT")
boxplot(Base$EDAD_SOCIEDAD, main= "Boxplot Edad Sociedades") # más equilibrado


# ver distribucion de las variables
plot(Base$CANTIDAD_TRABAJADOR, main = "Distribución Cantidad de Trabajores ")# sacar 4
plot(Base$CANTIDAD_SOCIOS,main = "Distribución Cantidad de Socios ") # sacar 2
plot(Base$INGRESOS,main = "Distribución Ingresos ") # sacar 3 ouliers 
plot(Base$ACTIVOS_TOTALES,main = "Distribución Act Totales ") #Sacar 2
plot(Base$RATIO_ING_COST, main = "Distribución Ingreso Costo ") #Sacar 3
plot(Base$PROMEDIO_5AT, main = "Distribución Promedio5_AT ") #Sacar 3
plot(Base$EDAD_SOCIEDAD,main = "Distribución Edad Sociedades ") #Sacar 3


#-----------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#----------------------------   APLICAREMOS LOS MODELOS MEAN-SHIFT Y DBSCAN   --------------------------------
#------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------



#  M E A N   -    S H I F T
# -----------------------------  

# 1. Seleccionar las columnas predictoras
vars <- c("CANTIDAD_TRABAJADOR", "CANTIDAD_SOCIOS", "INGRESOS", 
          "ACTIVOS_TOTALES", "COMUNA", "RATIO_ING_COST", 
          "PROMEDIO_5AT", "EDAD_SOCIEDAD")

data_ms <- Base[, vars]

# 2. Trataremos la variable COMUNA como una dummy 
unique(data_ms$COMUNA) #hay 191 comunas por ende exisitran 191 columnas dummies
data_ms$COMUNA <- as.factor(data_ms$COMUNA)
dummies <- model.matrix(~ COMUNA - 1, data = data_ms)
data_ms <- cbind(data_ms[, -which(names(data_ms) == "COMUNA")], dummies)


# 3. Estandarizar la base
data_scaled <- scale(data_ms)

# 4. Limpiar datos: quitar NAs o infinitos (Paso Extra)
data_scaled <- na.omit(data_scaled)
data_scaled <- data_scaled[is.finite(rowSums(data_scaled)), ]

# 5. Aplicar clustering Mean Shift (aplicamos el modelo)
modelo_ms <- ms(data_scaled, h = 0.1)  #con un  h=0.1 # se demora este codigo son 198

# 6. Asignar etiquetas de cluster predichos a la  Base original
Base$cluster_ms <- NA
Base$cluster_ms[as.numeric(rownames(data_scaled))] <- modelo_ms$cluster.label
Base$cluster_ms <- as.factor(Base$cluster_ms)

# 7. Mapear clusters a rubros
mapping <- Base %>%
  filter(!is.na(cluster_ms)) %>%
  group_by(cluster_ms) %>%
  summarise(rubro_pred = RUBRO[which.max(table(RUBRO))]) %>%
  deframe()
# etiquetamos a la base original a los rubros predichos
Base$rubro_predicho <- mapping[as.character(Base$cluster_ms)]

# 8. Evaluar precisión del modelo
df <- data.frame(RUBRO = Base$RUBRO, rubro_predicho = Base$rubro_predicho)
df$Coincidencia <- ifelse(df$RUBRO == df$rubro_predicho, 1, 0)

sum(df$Coincidencia) # prediche 922 Rubros 

#calcular los match
precision <- sum(df$Coincidencia, na.rm = TRUE) / sum(!is.na(df$Coincidencia))
precision*100

#calcular la precision
cat("Precisión:", sprintf("%.2f", precision*100), "%\n")


# 9. Calcular pureza promedio de los clusters
pureza <- Base %>%
  filter(!is.na(cluster_ms)) %>%
  group_by(cluster_ms) %>%
  summarise(pureza = max(table(RUBRO)) / n()) %>%
  summarise(pureza_promedio = mean(pureza)) %>%
  pull(pureza_promedio)


cat("Pureza promedio de los clusters:", round(pureza*100, 4), "%\n")
         


#Para visualizar los datos, se debe hacer un ajuste 


#  ---------------------------GRAFICO 1 --------------------------------------------------------

# Crear matriz de confusión
tabla_confusion <- table(Base$RUBRO, Base$rubro_predicho)

# Convertir a formato largo para ggplot
df_confusion <- as.data.frame(tabla_confusion)
colnames(df_confusion) <- c("RUBRO_REAL", "RUBRO_PREDICHO", "FREQ")

# Grafico 1
ggplot(df_confusion, aes(x = RUBRO_PREDICHO, y = RUBRO_REAL, fill = FREQ)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FREQ), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Matriz de Confusión: Rubro Real vs Rubro Predicho",
       x = "Rubro Predicho",
       y = "Rubro Real") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------- GRAFICO 2 ----------------------------------------------------------

# Calcular porcentaje de coincidencia por rubro
por_rubro <- Base %>%
  filter(!is.na(rubro_predicho)) %>%
  mutate(Coincidencia = RUBRO == rubro_predicho) %>%
  group_by(RUBRO) %>%
  summarise(Total = n(),
            Correctos = sum(Coincidencia),
            Porcentaje = Correctos / Total)

# GrafICO 2
ggplot(por_rubro, aes(x = reorder(RUBRO, -Porcentaje), y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 1)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Precisión por Rubro (RUBRO vs rubro_predicho) MEAN-SHIFT",
       x = "Rubro Real",
       y = "Porcentaje de coincidencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ------------------------ Grafico 3 -------------------------------------------

pca_model <- prcomp(data_scaled)

plot_real <- autoplot(pca_model, data = Base[as.numeric(rownames(data_scaled)), ],
                      colour = 'RUBRO', 
                      main = "Rubro Real (proyección PCA)") +
  theme_minimal()

plot_pred <- autoplot(pca_model, data = Base[as.numeric(rownames(data_scaled)), ],
                      colour = 'rubro_predicho', 
                      main = "Rubro Predicho (proyección PCA)") +
  theme_minimal()

# Comparar ambos graficos 
gridExtra::grid.arrange(plot_real, plot_pred, ncol = 2)



#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################


#         M O D E L O       D B S C A N 
#---------------------------------------------



library(dbscan)
library(dplyr)
library(ggplot2)

# 1. Copiar la base
Base_db <- Base

# 2. Convertir comuna a factor numérico
Base_db$COMUNA <- as.numeric(as.factor(Base_db$COMUNA))

# 3. Seleccionar solo las variables numéricas para clustering
vars_dbscan <- c("CANTIDAD_TRABAJADOR", "CANTIDAD_SOCIOS", "INGRESOS", 
                 "ACTIVOS_TOTALES", "COMUNA", "RATIO_ING_COST", 
                 "PROMEDIO_5AT", "EDAD_SOCIEDAD")

data_dbscan <- Base_db[, vars_dbscan]

# 4. Escalar
data_scaled <- scale(data_dbscan)

# 5. Eliminar filas problemáticas
data_scaled <- na.omit(data_scaled)
data_scaled <- data_scaled[is.finite(rowSums(data_scaled)), ]

# 6. DBSCAN
modelo_dbscan <- dbscan(data_scaled, eps = 0.55, minPts = 1)
modelo_dbscan <- dbscan::dbscan(data_scaled, eps = 0.55, minPts = 1)
Base_db$cluster_dbscan <- modelo_dbscan$cluster

# 7. Mapping de rubro dominante por cluster
mapping_db <- Base_db %>%
  filter(!is.na(cluster_dbscan)) %>%
  group_by(cluster_dbscan) %>%
  summarise(rubro_pred = names(sort(table(RUBRO), decreasing = TRUE)[1]))

# 8. Predecir rubros
Base_db <- Base_db %>%
  left_join(mapping_db, by = "cluster_dbscan") %>%
  mutate(coincide = RUBRO == rubro_pred)

# 9. Evaluar
df1 <- data.frame(RUBRO = Base_db$RUBRO, rubro_predicho = Base_db$rubro_pred)
df1$Coincidencia <- ifelse(df1$RUBRO == df1$rubro_predicho, 1, 0)

precision_dbscan <- sum(df1$Coincidencia, na.rm = TRUE) / sum(!is.na(df1$Coincidencia))
cat("Precisión:", sprintf("%.2f", precision_dbscan * 100), "%\n")

# ---------------------------------------------------------------------------------------
# 📊 GRAFICO 1: Matriz de confusión
confusion_matrix <- table(Rubro_Real = df1$RUBRO, Rubro_Predicho = df1$rubro_predicho)
df_confusion_DBSCAN <- as.data.frame(confusion_matrix)
colnames(df_confusion_DBSCAN) <- c("RUBRO_REAL", "RUBRO_PREDICHO", "FREQ")

ggplot(df_confusion_DBSCAN, aes(x = RUBRO_PREDICHO, y = RUBRO_REAL, fill = FREQ)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FREQ), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "Matriz de Confusión: Rubro Real vs Rubro Predicho MODELO DBSCAN",
       x = "Rubro Predicho",
       y = "Rubro Real") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------------------------------------------------------------------
# 📈 GRAFICO 2: Precisión por rubro
precision_rubro <- df1 %>%
  group_by(RUBRO) %>%
  summarise(
    total = n(),
    coincidencias = sum(Coincidencia, na.rm = TRUE),
    precision = coincidencias / total
  )

ggplot(precision_rubro, aes(x = reorder(RUBRO, precision), y = precision)) +
  geom_bar(stat = "identity", fill = "#0073C2FF") +
  geom_text(aes(label = sprintf("%.1f%%", precision * 100)), 
            vjust = -0.5, size = 3.5) +
  labs(title = "Precisión del modelo DBSCAN por rubro real",
       x = "Rubro",
       y = "Precisión") +
  theme_minimal() +
  coord_flip()



# ---------------------------------------Ordenar la base en uno ----------------------------------

colnames(Base)

colnames(Base)<- c("RUT_SOCIEDAD","DV_SOCIEDAD", "RUBRO", "CANTIDAD_TRABAJADOR",
                   "CANTIDAD_SOCIOS", "INGRESOS","ACTIVOS_TOTALES","COMUNA",
                   "RATIO_ING_COST", "PROMEDIO_5AT", "EDAD_SOCIEDAD","cluster_ms",
                   "rubro_predicho_ms")

Base$cluster_dbscan<- Base_db$cluster_dbscan
Base$rubro_predicho_db<- Base_db$rubro_pred



# match de ambos modelos juntos 

comparar<- data.frame(Base$rubro_predicho_ms, Base$rubro_predicho_db)
colnames(comparar)<- c("rubro_predicho_ms","rubro_predicho_db")
comparar$Coincidencia <- ifelse(comparar$rubro_predicho_ms == comparar$rubro_predicho_db, 1, 0)
sum(comparar$Coincidencia)


presicion_ms_dbscan<- sum(comparar$Coincidencia)/1000
cat("Presición ambos modelos :", sprintf("%.2f", presicion_ms_dbscan * 100), "%\n")




#-------------------------------------------------------------------------------------------------------

########## Cargar la Base de Testeo    ###########################################################
#rm(Base_clean,Base_nueva,vars, vars_ms, vars_ms_sin_comuna, data_nueva_scaled, data_scaled, centroides_ms)

#-------------------------------------------------------------------------------------------------------


# 1. Leer y preprocesar la nueva base de datos
nueva_base <- read_excel("MUESTRA2_INNOM.xlsx") %>%
  mutate(COMUNA = as.numeric(as.factor(COMUNA)))

nueva_base<- read.csv("")


# 2. Definir las variables a utilizar
variables_modelo <- c("CANTIDAD_TRABAJADOR", "CANTIDAD_SOCIOS", "INGRESOS",
                      "ACTIVOS_TOTALES", "COMUNA", "RATIO_ING_COST",
                      "PROMEDIO_5AT", "EDAD_SOCIEDAD")

# Filtrar la nueva base solo con las variables necesarias
nueva_base_filtrada <- nueva_base %>%
  select(all_of(variables_modelo))

# 3. Escalar las variables numéricas (excepto COMUNA)
variables_para_escalar <- setdiff(variables_modelo, "COMUNA")
data_nueva_escalada <- nueva_base_filtrada %>%
  select(all_of(variables_para_escalar)) %>%
  scale()

# Guardar los atributos de escalamiento para usarlos luego
escala_atributos <- attributes(data_nueva_escalada)[c("scaled:center", "scaled:scale")]

data_nueva_escalada <- as.data.frame(data_nueva_escalada) %>%
  bind_cols(nueva_base_filtrada %>% select(COMUNA)) # Re-incorporar COMUNA


# 4. Preparar los centroides de los clusters existentes
base_limpia <- Base %>%
  filter(!is.na(cluster_ms)) %>%
  mutate(across(all_of(variables_modelo), as.numeric)) # Asegurar tipo numérico

# Manejar los warnings de coerción a numérico
if (any(sapply(base_limpia[,variables_modelo], function(x) any(is.na(x))))) {
  warning("Se introdujeron NAs por coerción en la base de datos original (Base).  Revise los datos.")
}


centroides <- base_limpia %>%
  group_by(cluster_ms) %>%
  summarise(across(all_of(setdiff(variables_modelo, "COMUNA")), mean, na.rm = TRUE)) %>%
  ungroup()

valores_centroides <- centroides %>%
  select(-cluster_ms)

# Escalar los valores de los centroides usando la misma escala que la nueva base
centroides_escalados <- scale(valores_centroides, 
                              center = escala_atributos[["scaled:center"]],
                              scale = escala_atributos[["scaled:scale"]]) %>%
  as.data.frame() %>%
  bind_cols(centroides %>% select(cluster_ms))

# 5. Función para asignar cluster basado en la distancia euclidiana
asignar_cluster <- function(fila, centroides_escalados) {
  distancias <- apply(centroides_escalados %>% select(-cluster_ms), 1, function(centroide) {
    sum((fila - centroide)^2)
  })
  cluster_mas_cercano <- centroides_escalados$cluster_ms[which.min(distancias)]
  return(cluster_mas_cercano)
}

# Asignar el cluster a la nueva base de datos
data_nueva_escalada$cluster_ms <- apply(data_nueva_escalada %>% select(-COMUNA), 1, asignar_cluster, centroides_escalados = centroides_escalados)

# 6. Mapear el cluster al rubro predicho
mapeo_rubros <- Base %>%
  filter(!is.na(cluster_ms)) %>%
  group_by(cluster_ms) %>%
  summarise(rubro_predicho = names(sort(table(RUBRO), decreasing = TRUE)[1])) %>%
  ungroup()

nueva_base_filtrada$cluster_ms <- data_nueva_escalada$cluster_ms # Añadir la asignación de cluster a la base filtrada

base_nueva_con_prediccion <- nueva_base_filtrada %>%
  left_join(mapeo_rubros, by = "cluster_ms")

# 7. Visualizar la tabla de rubros predichos
table(base_nueva_con_prediccion$rubro_predicho)


#----------------- Grafico para ver los Rubros Predichos -------------------------------------------------------

# Asegúrate de que 'base_nueva_con_prediccion' esté disponible
if (!exists("base_nueva_con_prediccion")) {
  stop("La base de datos 'base_nueva_con_prediccion' no está definida.  Ejecuta el código de clustering primero.")
}

# 1. Gráfico de barras del conteo de rubros predichos
ggplot(base_nueva_con_prediccion, aes(x = rubro_predicho)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + # Etiquetas con el conteo
  labs(title = "Conteo de Rubros Predichos",
       x = "Rubro Predicho",
       y = "Cantidad de Empresas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas del eje x

library(openxlsx)

write.xlsx(base_nueva_con_prediccion, file = "PrediccionesPrueba.xlsx")































