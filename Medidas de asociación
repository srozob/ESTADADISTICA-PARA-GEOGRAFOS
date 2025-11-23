library(ggplot2)
library(dplyr)

## 2. VARIABLES CUALITATIVAS ========
## 2.1 Tablas de doble entrada =======

sexo <- c("F", "M", "F", "F", "M", "M", "F", "M", "F", "F",
          "M", "F", "F", "M", "F")

programa <- c("Bio", "Bio", "Ing", "Soc", "Ing", "Bio", "Soc",
              "Ing", "Ing", "Bio", "Bio", "Soc", "Ing", "Soc", "Bio")

tabla <- table(sexo, programa)
tabla
## La tabla cruzada muestra cómo se distribuye la frecuencia
## entre categorías de sexo y programa académico.

prop_filas <- prop.table(tabla, 1)
prop_columnas <- prop.table(tabla, 2)

##Gráficos perfiles fila y columna 
df_prop_fila <- as.data.frame(prop_filas)
df_prop_col <- as.data.frame(prop_columnas)

ggplot(df_prop_fila, aes(x = sexo, y = Freq, fill = programa)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Perfiles por fila", y = "Proporción")

## Muestra la composición porcentual de programas dentro de cada sexo.

ggplot(df_prop_col, aes(x = programa, y = Freq, fill = sexo)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Perfiles por columna", y = "Proporción")

## Muestra la participación relativa de cada sexo dentro de cada programa.

## 3. VARIABLES CUANTITATIVAS =======
## Datos de ejemplo
peso <- c(52, 54, 55, 57, 59, 60, 61, 63, 64, 65,
          66, 68, 69, 70, 72)

temperatura <- c(12.3, 12.5, 12.6, 12.7, 12.9, 13.0, 13.1, 13.2,
                 13.3, 13.4, 13.6, 13.8, 13.9, 14.0, 14.2)

df <- data.frame(peso, temperatura)

## 3.1 GRÁFICOS =======
ggplot(df, aes(x = temperatura, y = peso)) +
  geom_point() +
  labs(title = "Peso vs. Temperatura")
## A mayores temperaturas,mayores valores de peso.

## 3.2 COVARIANZA =======
cov_peso_temp <- cov(peso, temperatura)
cov_peso_temp

## La covarianza es positiva, lo cual indica que ambas variables
## tienden a aumentar juntas.

## 3.3 COEFICIENTE DE CORRELACIÓN =======
cor_peso_temp <- cor(peso, temperatura)
cor_peso_temp
## El coeficiente de correlación es cercano a 1, indicando una
## relación lineal fuerte y positiva entre peso y temperatura.

## 3.4 MATRIZ DE DISPERSIÓN =======
pairs(df, main = "Matriz de diagramas de dispersión")

## El gráfico confirma una tendencia lineal marcada entre
## las dos variables: puntos ascendentes y concentrados.
