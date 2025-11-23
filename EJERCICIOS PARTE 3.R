## 2.4 Moda ==========
## La moda de un conjunto de datos agrupados por intervalos corresponde al
##(a los) valor(res) que maximiza(n) la distribución de frecuencias, la cual se calcula como: 
# Md(x) = l_{k-1} + a_k * ( (n_k - n_{k-1}) / (2n_k - n_{k-1} - n_{k+1}) )
## 2.4.1 Ejercicio ========
### CONUJUNTO MATERIALES
li <- 15:23  # limite inferior de los intervalos
ls <- 16:24  # limite superior de los intervalos
yj <- (li+ls)/2  # marca de clase
print(yj)
length(yj) # numero de intervalos
nj <- c(2, 5, 29, 76, 118, 96, 83, 37, 4)  # frecuencia absoluta
sum(nj)  # tamaño de la muestra
hj <- nj/sum(nj)  # frecuencia relativa
print(hj)
## Calcular e interpretar la moda del peso del conjunto de datos asociados con la muestra de materiales.
# índice del intervalo modal k
k <- which.max(nj)
print(k)
# l_{k-1}  → límite inferior del intervalo modal
lk_1 <- li[k]       
# a_k → amplitud del intervalo
ak <- ls[1] - li[1]   # = 1
# n_k, n_{k-1}, n_{k+1}
nk   <- nj[k]
nk_1 <- nj[k - 1]
nk1  <- nj[k + 1]
# fórmula
Moda <- lk_1 + ak * ((nk - nk_1) / (2*nk - nk_1 - nk1))
Moda
cat("\n========== INTERPRETACIÓN DE LA MODA ==========\n")
cat("Moda:", round(Moda, 2), "kg\n")
cat("\nInterpretación:\n")
cat("El peso más frecuente en la muestra de materiales es", 
    round(Moda, 2), "kg.\n")
cat("Esto significa que este es el valor más representativo y frecuente en la distribución de pesos:",
    round(Moda, 2), "kg.\n")
## 3 Medidas de localización
## Las medidas de localización o percetiles son valores que delimitan superiormente
## Como casos particulares se tienen los cuartiles (percentiles 25, 50 y 75; la mediana es el percentil 50 o cuartil 2).
## 3.3 Ejercicio
## Calcular e interpretar los cuartiles del peso del conjunto de datos asociados con la muestra de materiales.
# ======== CÁLCULO DE PERCENTILES - DATOS AGRUPADOS
li <- 15:23
ls <- 16:24
nj <- c(2, 5, 29, 76, 118, 96, 83, 37, 4)

# Calcular frecuencia acumulada
Nj <- cumsum(nj)
n <- sum(nj)
ak <- ls[1] - li[1]

# Función para calcular percentiles
calcular_percentil <- function(t, li, nj, Nj, n, ak) {
  posicion <- (n * t) / 100
  k <- which(Nj >= posicion)[1]
  lk_1 <- li[k]
  nk <- nj[k]
  Nk_1 <- if (k == 1) 0 else Nj[k - 1]
  percentil <- lk_1 + ak * ((posicion - Nk_1) / nk)
  return(percentil)
}

# Calcular cuartiles
Q1 <- calcular_percentil(25, li, nj, Nj, n, ak)
Q2 <- calcular_percentil(50, li, nj, Nj, n, ak)
Q3 <- calcular_percentil(75, li, nj, Nj, n, ak)

# ======== CONSTRUCCIÓN DE DATOS EXPANDIDOS ========
marca <- (li + ls) / 2
datos_expand <- rep(marca, nj)

# Crear dataframe para ggplot2
df_datos <- data.frame(peso = datos_expand)

cat("\n========== CUARTILES CALCULADOS ==========\n")
cat("Q1 (25%):", round(Q1, 2), "kg\n")
cat("Q2 (50%):", round(Q2, 2), "kg\n")
cat("Q3 (75%):", round(Q3, 2), "kg\n\n")

dev.new(width = 14, height = 6)
par(mfrow = c(1, 2))

# Gráfico con hist()
hist(
  datos_expand,
  main = "Distribución del peso - BASE R hist()",
  xlab = "Peso (kg)",
  ylab = "Frecuencia",
  col = "lightblue",
  border = "gray",
  breaks = 9
)
abline(v = Q1, col = "red", lwd = 2, lty = 2)
abline(v = Q2, col = "blue", lwd = 2, lty = 2)
abline(v = Q3, col = "darkgreen", lwd = 2, lty = 2)
legend(
  "topright",
  legend = c(paste("Q1 =", round(Q1, 2)), 
             paste("Q2 =", round(Q2, 2)), 
             paste("Q3 =", round(Q3, 2))),
  col = c("red", "blue", "darkgreen"),
  lwd = 2,
  lty = 2,
  cex = 0.9
)
# Boxplot básico
boxplot(
  datos_expand,
  main = "Boxplot - BASE R",
  ylab = "Peso (kg)",
  col = "lightgray",
  horizontal = FALSE
)

library(ggplot2)
# Crear gráfico con ggplot2
g <- ggplot(df_datos, aes(x = peso)) +
  # Histograma con densidad
  geom_histogram(
    aes(y = ..density..),  # Normalizar por densidad
    bins = 9,              # Mismo número de intervalos
    fill = "lightblue",
    color = "gray30",
    alpha = 0.7
  ) +
  # Agregar densidad suave (kernel density)
  geom_density(
    color = "darkblue",
    size = 1,
    alpha = 0.3,
    fill = "skyblue"
  ) +
  # Líneas verticales para cuartiles
  geom_vline(
    aes(xintercept = Q1, color = "Q1 (25%)"),
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(
    aes(xintercept = Q2, color = "Q2 (50%)"),
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(
    aes(xintercept = Q3, color = "Q3 (75%)"),
    linetype = "dashed",
    size = 1
  ) +
  # Etiquetas y títulos
  labs(
    title = "Distribución del peso de materiales - GGPLOT2",
    subtitle = paste("Cuartiles: Q1 =", round(Q1, 2), "kg | Q2 =", round(Q2, 2), 
                     "kg | Q3 =", round(Q3, 2), "kg"),
    x = "Peso (kg)",
    y = "Densidad",
    color = "Cuartiles"
  ) +
  # Escala de colores manual
  scale_color_manual(
    values = c("Q1 (25%)" = "red", "Q2 (50%)" = "blue", "Q3 (75%)" = "darkgreen")
  ) +
  # Tema limpio
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "topright",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

print(g)
# ======== BOXPLOT CON GGPLOT2 ========
boxplot_gg <- ggplot(df_datos, aes(x = "Peso", y = peso)) +
  geom_boxplot(
    fill = "lightblue",
    color = "darkblue",
    alpha = 0.7,
    size = 1
  ) +
  geom_jitter(
    width = 0.1,
    alpha = 0.3,
    color = "darkblue"
  ) +
  labs(
    title = "Boxplot - GGPLOT2",
    subtitle = "Con visualización de puntos individuales",
    y = "Peso (kg)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    panel.grid.major.x = element_blank()
  )
print(boxplot_gg)

