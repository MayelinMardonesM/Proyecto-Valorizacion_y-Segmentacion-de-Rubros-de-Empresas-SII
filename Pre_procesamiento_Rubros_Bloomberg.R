
##                           Procesamiento de Datos de Bloomberg




rm(list = ls())

#Paquetes a Cargar
library(readxl)
library(tidyverse)
library(naniar)
library(dplyr)
library(ggplot2)
library(readxl)
library(openxlsx)
library(dplyr)


## Directorio 
setwd("C:/Users/MAYELIN/Downloads/R-Studio")

dir()

# Cargar la base de  datos 

Base<- read_excel("Rendimiento_Bonos_001.xlsx", sheet = "Bonds")

table(Base$`BICS Level 1`)
table(Base$`BICS Level 2`)

#Comportamiento de los Rubros 

barplot(table(Base$`BICS Level 1`))
barplot(table(Base$`BICS Level 2`))


unique(Base$`BICS Level 2`) #79

#Procesamiento para los NA

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


#Ver la Distribucion de los Datos 

boxplot(Base$`Debt/Assets`)
boxplot(Base$`Debt / Common Equity`)
boxplot(Base$`Net Debt to EBITDA`)
boxplot(Base$EBITDA) #No esta en esta base , hay que incluirla 
boxplot(Base$`Rev Growth`) # No esta en esta base, hay qeu incluirla 

plot(Base$`Debt/Assets`)
plot(Base$`Debt / Common Equity`)
plot(Base$`Net Debt to EBITDA`)
plot(Base$EBITDA)
plot(Base$`Rev Growth`)


# Calcularesmos el promedio por grupo Columna " (Bics 2)

promedios_por_grupo <- Base %>%
  group_by(`BICS Level 2`) %>%
  summarise(across(c("Debt/Assets", "Debt / Common Equity", 
                     "Net Debt to EBITDA"), 
                   ~ round(mean(., na.rm = TRUE), 2)))


# Mostrar tabla
print(promedios_por_grupo)

# Variables que deseas imputar
vars_a_imputar <- c("Debt/Assets", "Debt / Common Equity", "Net Debt to EBITDA")

# Imputar por PROMEDIO por columna 
Base_imputada <- Base %>%
  group_by(`BICS Level 2`) %>%
  mutate(across(all_of(vars_a_imputar), 
              ~ replace(., is.na(.), mean(., na.rm = TRUE))))


B_Filtrada <- Base_imputada %>% filter(`BICS Level 2` == "Life Insurance")

#Ver la Distribucion de los Datos 

boxplot(Base_imputada$`Debt/Assets`)
boxplot(Base_imputada$`Debt / Common Equity`)
boxplot(Base_imputada$`Net Debt to EBITDA`)
boxplot(Base_imputada$EBITDA)
boxplot(Base_imputada$`Rev Growth`)

plot(Base_imputada$`Debt/Assets`)
plot(Base_imputada$`Debt / Common Equity`)
plot(Base_imputada$`Net Debt to EBITDA`)
plot(Base_imputada$EBITDA)
plot(Base_imputada$`Rev Growth`)


#Vista del Boxplot
# Lista de columnas numéricas
vars_boxplot <- c("Debt/Assets", "Debt / Common Equity", "Net Debt to EBITDA")

# Función para extraer estadísticas tipo boxplot
resumen_boxplot <- function(x) {
  stats <- fivenum(x, na.rm = TRUE)  # Mín, Q1, Mediana, Q3, Máx
  names(stats) <- c("Min", "Q1", "Mediana", "Q3", "Max")
  return(stats)
}

# Aplicar a cada columna
boxplot_stats <- sapply(Base_imputada[vars_boxplot], resumen_boxplot)

# Transponer para que queden columnas como variables
boxplot_stats_df <- as.data.frame(t(boxplot_stats))
print(round(boxplot_stats_df, 2))

# PARA IDENTIFICAR LAS OUTLIERS
library(dplyr)

# 1. Definir función para detectar outliers usando IQR
identificar_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lim_inf <- q1 - 1.5 * iqr
  lim_sup <- q3 + 1.5 * iqr
  return(x < lim_inf | x > lim_sup)
}

# 2. Variables numéricas a analizar
vars_outliers <- c("Debt/Assets", "Debt / Common Equity", 
                   "Net Debt to EBITDA")

# 3. Crear columnas que indican si un valor es outlier (TRUE/FALSE)
Base_outliers <- Base_imputada %>%
  mutate(across(all_of(vars_outliers), 
                identificar_outliers,
                .names = "outlier_{.col}"))

# 4. Crear un dataframe solo con las filas que tienen al menos un outlier
df_outliers_solo <- Base_outliers %>%
  filter(if_any(starts_with("outlier_"), ~ . == TRUE)) # 1012 ouliers ; 20.28%

# 5. Mostrar las primeras filas del dataframe
print(head(df_outliers_solo))

# SON 1854 en % son 37,8%
colnames(Base_imputada)

B_Filtrada<-Base_imputada %>% filter("BICS Level 2"== "Life Insurance")

B_Filtrada <- Base %>% filter(`BICS Level 2` == "Life Insurance")


dir()
#Guardamos el Procedimiento 

write.xlsx(Base_imputada, file = "BASE_PARA_TRA_2.xlsx")




library(openxlsx)

# Cargar el archivo existente
wb <- loadWorkbook("Rendimiento_Bonos_001.xlsx")

# Nombre de la hoja nueva
nombre_hoja <- "Bonds_Procesados"

# Si la hoja ya existe, elimínala (opcional pero útil para evitar duplicados)
if (nombre_hoja %in% names(wb)) {
  removeWorksheet(wb, nombre_hoja)
}

# Agregar y escribir datos
addWorksheet(wb, nombre_hoja)
writeData(wb, sheet = nombre_hoja, Base_imputada)

# Guardar el archivo sobrescribiéndolo
saveWorkbook(wb, "Rendimiento_Bonos_001.xlsx", overwrite = TRUE)


