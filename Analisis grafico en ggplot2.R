# ANALISIS GRAFICO EN GGPLOT20

library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

setwd("/cloud/project/Data")

EDIT <- read.table(file = "EDIT.txt", header = TRUE)

n <- nrow(EDIT)

# 1. VARIABLES CUALITATIVAS ==============
# - Describir la variable binaria “ID” (¿Tiene departamento de I+D?)
# - Calcular porcentajes
# - Representar la distribución con barra y sectores
# - Identificar la proporción de empresas con y sin I+D.

tabla_id <- 100 * table(EDIT$ID) / n
names(tabla_id) <- c("Sí", "No")

df_id <- data.frame(
  Respuesta = names(tabla_id),
  Porcentaje = as.numeric(tabla_id)
)

# Diagrama de barras
# Interpreta la magnitud relativa y permite comparar categorías.
g_barras <- ggplot(df_id, aes(x = reorder(Respuesta, -Porcentaje), 
                              y = Porcentaje, fill = Respuesta)) +
  geom_bar(stat = "identity", alpha = 0.85, width = 0.6) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(title = "Diagrama de Barras",
       subtitle = "Empresas con Departamento de I+D",
       x = "¿Tiene Departamento de I+D?",
       y = "Porcentaje (%)") +
  ylim(0, 100) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray40", size = 11),
        legend.position = "none",
        panel.grid.major.x = element_blank())

#  Diagrama de sectores
# Útil para representar participación relativa.
g_pie <- ggplot(df_id, aes(x = "", y = Porcentaje, fill = Respuesta)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Respuesta, "\n", round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 5, color = "white") +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(title = "Diagrama de Sectores") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5))

grid.arrange(g_barras, g_pie, ncol = 2)
# La gran mayoría de empresas NO tiene departamento de I+D. 
# La barra y el gráfico de sectores coinciden y muestran esta asimetría.

# 2. TABLA DE CONTINGENCIA (CUALITATIVAS X CUALITATIVAS) =========
# Cruzamos: I+D (Sí/No) vs Disminución en el pago de impuestos (Alta/Media/Nula)
# - Detectar patrones de asociación
# - Ver si la decisión de tener I+D se relaciona con mayores reducciones tributarias

tabla_contingencia <- round(100 * table(EDIT$ID, EDIT$Disminucion) / n, 2)
rownames(tabla_contingencia) <- c("Sí", "No")
colnames(tabla_contingencia) <- c("Alta", "Media", "Nula")

df_contingencia <- tabla_contingencia %>%
  as.data.frame() %>%
  rename(I_D = Var1, Disminucion = Var2, Porcentaje = Freq)

# Barras agrupadas
# Facilitan comparar categorías dentro de cada nivel de la variable explicada.
g_contingencia <- ggplot(df_contingencia, 
                         aes(x = Disminucion, y = Porcentaje, fill = I_D)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("steelblue", "coral"), 
                    labels = c("Sí tiene I+D", "No tiene I+D")) +
  labs(title = "Análisis de Contingencia",
       subtitle = "I+D vs Disminución de impuestos",
       x = "Disminución en pago de impuestos",
       y = "Porcentaje (%)",
       fill = "Departamento I+D") +
  ylim(0, 70) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray40", size = 11),
        panel.grid.major.x = element_blank())

print(g_contingencia)

# Para las disminuciones ALTA y MEDIA predomina el grupo que NO tiene I+D.
# En “Nula” se observa una diferencia todavía más marcada.
# No hay evidencia visual de que tener I+D esté asociado a recibir mayor
# reducción tributaria. La estructura es bastante homogénea entre categorías.
# 3. VARIABLES CUANTITATIVAS: DISTRIBUCIÓN Y LOCALIZACIÓN =====
# variable Ventas (miles de millones)
#   Histograma + curva de densidad
#   Histograma log-transformado
#   Boxplot con jitter
# Examinar forma, dispersión, outliers y simetría.

# Histograma con densidad
g_hist <- ggplot(EDIT, aes(x = Ventas)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    fill = "#4A90E2",
    color = "white",
    alpha = 0.8
  ) +
  geom_density(
    color = "#E74C3C",
    linewidth = 1.2,   # Corrección: reemplaza size por linewidth
    alpha = 0.6
  ) +
  labs(
    title = "Distribución de Ventas con Curva de Densidad",
    x = "Ventas (miles de millones)",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 13)

# Mostrar gráfico
g_hist
# La distribución es claramente asimétrica a la derecha.
# Existen valores extremadamente altos comparados con la mayoría.
# La densidad suavizada confirma la larga cola hacia la derecha.

# Histograma en escala logarítmica
g_hist_log <- ggplot(EDIT, aes(x = Ventas)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "#4A90E2",
                 color = "white",
                 alpha = 0.8) +
  geom_density(color = "#E74C3C", size = 1.2, alpha = 0.6) +
  scale_x_log10() +
  labs(title = "Distribución de Ventas (Escala Logarítmica)",
       x = "Ventas (log10)",
       y = "Densidad") +
  theme_minimal(base_size = 13)

print(g_hist_log)

# La escala log reduce la asimetría y permite visualizar mejor el cuerpo principal
# de la distribución. La transformación revela que la mayoría de valores son pequeños
# y los outliers no dominan la gráfica.

# Boxplot con jitter
g_boxplot <- ggplot(EDIT, aes(x = "", y = Ventas)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", alpha = 0.7, size = 1.2) +
  geom_jitter(width = 0.1, alpha = 0.2, color = "darkblue", size = 1) +
  labs(title = "Diagrama de Caja",
       subtitle = "Con puntos individuales (jitter)",
       y = "Ventas (miles de millones)") +
  coord_flip() +
  theme_minimal()

print(g_boxplot)

# El boxplot confirma fuerte asimetría positiva.
# Existen numerosos outliers muy por encima del rango intercuartílico.
# Es poco claro, pero podemos solucionar haciendo zoom

# BOXLOT CON ZOOM EN EL CUERPO DE LOS DATOS 

g_boxplot_zoom <- ggplot(EDIT, aes(x = "", y = Ventas)) +
  geom_boxplot(fill = "#4A90E2", color = "#08306B", alpha = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.25, color = "#08306B", size = 1.7) +
  coord_cartesian(ylim = c(0, quantile(EDIT$Ventas, 0.95))) +   # Zoom al 95%
  labs(
    title = "Diagrama de Caja con Zoom",
    subtitle = "Se mejora la visualización sin eliminar outliers",
    y = "Ventas (miles de millones)",
    x = ""
  ) +
  theme_minimal(base_size = 14)

g_boxplot_zoom

# 4. ANÁLISIS MULTIVARIADO DE VARIABLES CUANTITATIVAS

datos <- read.table(file = "sinteticos.txt", header = TRUE)

df_long <- datos %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Histogramas + densidad
g_multiple <- ggplot(df_long, aes(x = Valor, fill = Variable)) +
  geom_histogram(bins = 25, alpha = 0.7, color = "white") +
  geom_density(aes(y = ..count..), color = "darkred", size = 1, alpha = 0.3) +
  facet_wrap(~Variable, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("steelblue", "coral", "lightgreen", "gold")) +
  labs(title = "Análisis Multivariado",
       subtitle = "Histogramas y densidades para 4 variables",
       x = "Valor",
       y = "Frecuencia") +
  theme_minimal()

print(g_multiple)


# Cada variable tiene una forma distinta:
# Algunas son simétricas, otras claramente sesgadas.
# Se observan diferentes rangos y dispersión.
# Los histogramas permiten notar diferencias estructurales entre variables.

# Boxplots comparativos
g_box_multiple <- ggplot(df_long, aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(alpha = 0.7, color = "darkgray", size = 1) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.8) +
  scale_fill_manual(values = c("steelblue", "coral", "lightgreen", "gold")) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  labs(title = "Comparación de Boxplots",
       subtitle = "Con puntos individuales superpuestos",
       x = "Variable",
       y = "Valor") +
  theme_minimal()

print(g_box_multiple)

# Los boxplots permiten comparar localización (mediana), variabilidad (RIQ)
# y presencia de valores atípicos para cada variable.
# Cada variable presenta estructura distinta, confirmando que no son comparables
# en escala ni forma. La visualización facilita identificar cuál es más dispersa
# o presenta más outliers.
