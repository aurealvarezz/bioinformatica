# Trabajo final Bioinformática - Curso 25/26 Aurea Alvarez Alvarez NP:142778
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
install.packages("UsingR")
library(UsingR)
datos_biomed=read.csv("datos_biomed.csv", header=TRUE, sep=",")
head(datos_biomed)
str(datos_biomed)


# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
#La función head() nos permite ver las primeras filas de un data frame
cat("\n=== Primeras filas del dataset ===\n")
head(datos_biomed)
#La función summary() proporciona una visión general de cada variable
cat("\n=== Resumen estadístico de las variables ===\n")
summary(datos_biomed)
#La función dim() devuelve las dimensiones del objeto, es decir, su número de filas y columnas
cat("\n === Dimensiones del dataset (filas, columnas)====\n")
dim(datos_biomed)
#La función str() permite visualizar la estructura interna del conjunto de datos
cat("\n=== Estructura del dataset ===\n")
str(datos_biomed)
#Saber cuántas variables tenemos
cat("\n== Número de variables ===\n")
num_variables <- ncol(datos_biomed)
num_variables
#Saber cuántos tratamiento hay
cat("\n== Número de tratamientos distinto ===\n")
unique(datos_biomed$tratamiento)
num_tratamientos <- length (unique(datos_biomed$tratamiento))
num_tratamientos


# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
install.packages("ggplot2")
library(ggplot2)

#Crear el boxplots con ggplot2
ggplot(datos_biomed, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) + geom_boxplot()
labs(title = "Boxplot de Glucosa por Tratamiento", x = "Tratamiento", y = "Glucosa (mg/dl)") + theme_minimal()



# 4. Realiza un violin plot (investiga qué es). (1 pt)
# Un violín plot es un gráfico que combina las características del boxplot con la densidad de los datos,
# permitiendo ver la mediana, el rango intercuartílico y la distribución, lo que es ideal para comparar grupos.
library(ggplot2)

# Violin plot para Glucosa por tratamiento
ggplot(datos_biomed, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) + 
  geom_violin(trim = FALSE, alpha = 0.7) + 
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  labs(title = "Distribución de Glucosa por Tratamiento", 
       x = "Tratamiento", 
       y = "Glucosa (mg/dl)") + 
  theme_minimal()

# Violin plot para Presión por tratamiento
ggplot(datos_biomed, aes(x = Tratamiento, y = Presion, fill = Tratamiento)) + 
  geom_violin(trim = FALSE, alpha = 0.7) + 
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  labs(title = "Distribución de Presión por Tratamiento", 
       x = "Tratamiento", 
       y = "Presión (mmHg)") + 
  theme_minimal()

# Violin plot para Colesterol por tratamiento
ggplot(datos_biomed, aes(x = Tratamiento, y = Colesterol, fill = Tratamiento)) + 
  geom_violin(trim = FALSE, alpha = 0.7) + 
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  labs(title = "Distribución de Colesterol por Tratamiento", 
       x = "Tratamiento", 
       y = "Colesterol (mg/dL)") + 
  theme_minimal()

# Se usa trim = FALSE para mostrar la distribución completa sin recortes.
# El parámetro alpha = 0.7 aplica transparencia al gráfico, lo que mejora su visualización.
# Al incluir el geom_boxplot dentro del violin plot se nos permite destacar la mediana y los cuartiles.


# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
#"Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
# Gráfico de dispersión Glucosa vs Presión
plot(datos_biomed$Glucosa, datos_biomed$Presion, 
     col = ifelse(datos_biomed$Tratamiento == "Placebo", "blue", 
                 ifelse(datos_biomed$Tratamiento == "FarmacoA", "red", "green")), 
     pch = 19, 
     main = "Relación entre Glucosa y Presión", 
     xlab = "Glucosa (mg/dL)", 
     ylab = "Presión (mmHg)")

# Poner una leyenda en la parte inferior derecha
legend("bottomright", 
       legend = c("Placebo", "FarmacoA", "FarmacoB"), 
       col = c("blue", "red", "green"), 
       pch = 19, 
       title = "Tratamiento")

# La función "bottomright" nos define la ubicación de la leyenda y pch determina la forma de los puntos.
# Cada número corresponde a un símbolo distinto; por ejemplo, pch=19 representa un círculo.

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
#Colesterol vs Presión por tratamiento. (1 pt)
# Un facet Grid es un gráfico que divide la visualización en varios paneles según la variable categórica,
# mostrando cada subconjunto de datos por separado, lo que facilita la comparación entre variables.
library(ggplot2)

ggplot(datos_biomed, aes(x = Presion, y = Colesterol, color = Tratamiento)) + 
  geom_point(size = 3) + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_grid(. ~ Tratamiento) + 
  labs(title = "Relación entre Colesterol y Presión por Tratamiento", 
       x = "Presión (mmHg)", 
       y = "Colesterol (mg/dL)") + 
  theme_minimal()

# La función geom_point se utiliza para graficar los puntos de dispersión, mientras que facet_grid crea un panel
# separado para cada tratamiento.
# La función geom_smooth añade una línea de tendencia; el argumento method= "lm" ajusta una recta y se = FALSE
# evita mostrar la banda de error alrededor de la línea.

# 7. Realiza un histogramas para cada variable. (0.5 pts)
# Con hist() genero un histograma para cada variable usando $ para acceder a la columna deseada
# del conjunto de datos.

# Para mostrar los tres gráficos todos juntos
par(mfrow = c(1,3)) # Para que cuando ejecute me salgan los tres gráficos en una misma ventana

hist(datos_biomed$Glucosa, main = "Histograma de Glucosa", xlab = "Glucosa (mg/dL)", col = "lightblue", border = "black")

hist(datos_biomed$Presion, main = "Histograma de Presión", xlab = "Presión (mmHg)", col = "lightgreen", border = "black")

hist(datos_biomed$Colesterol, main = "Histograma de Colesterol", xlab = "Colesterol (mg/dL)", col = "lightpink", border = "black")


# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
# La función factor() transforma una variable categórica; en este caso, se aplica a la columna "tratamiento".

datos_biomed$Tratamiento <- factor(datos_biomed$Tratamiento)

# Comprobar que el cambio que acabamos de hacer se ha aplicado
str(datos_biomed)
levels(datos_biomed$Tratamiento)


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
# Se utiliza aggregate() para obtener la media y la desviación estándar de glucosa según cada tratamiento.
media_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos_biomed, FUN = mean)
desv_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos_biomed, FUN = sd)

# Mostrar los resultados
cat("Media de Glucosa por Tratamiento:\n")
print(media_glucosa)

cat("\nDesviación Estándar de Glucosa por Tratamiento:\n")
print(desv_glucosa)

# Esto también se puede lograr en un único paso usando una función definida por el usuario.
estadisticas_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos_biomed, 
                                 FUN = function(x) c(Media = mean(x), Desviacion = sd(x)))
cat("\nEstadísticas completas de Glucosa por Tratamiento:\n")
print(estadisticas_gluco

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de placebo en una variable llamada placebo. (1 pt)
# Creo nuevos data frames que incluyen únicamente las filas correspondientes a cada tratamiento:
# uno para "Placebo", otro para "FarmacoA" y otro para "FarmacoB", separando así los datos por grupo.

placebo <- subset(datos_biomed, Tratamiento == "Placebo")
farmacoA <- subset(datos_biomed, Tratamiento == "FarmacoA")
farmacoB <- subset(datos_biomed, Tratamiento == "FarmacoB")

# Estas funciones se utilizan para comprobar que los datos se hayan separado correctamente,
# mostrando las primeras filas de cada data frame.

cat("Primeras filas del grupo Placebo:\n")
head(placebo)

cat("\nPrimeras filas del grupo FarmacoA:\n")
head(farmacoA)

cat("\nPrimeras filas del grupo FarmacoB:\n")
head(farmacoB)

# También se puede verificar con las dimensiones de cada dataset
cat("\nDimensiones de cada grupo:\n")
cat("Placebo:", dim(placebo), "\n")
cat("FarmacoA:", dim(farmacoA), "\n")
cat("FarmacoB:", dim(farmacoB), "\n")



# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
# Test de normalidad para cada tratamiento y variable
cat("=== Test de Normalidad Shapiro-Wilk ===\n") #TENGO QUE VER EL P VALUE

cat("\n=== Glucosa ===\n")
shapiro.test(placebo$Glucosa)
shapiro.test(farmacoA$Glucosa)
shapiro.test(farmacoB$Glucosa)

cat("\n=== Presión ===\n")
shapiro.test(placebo$Presion)
shapiro.test(farmacoA$Presion)
shapiro.test(farmacoB$Presion)

cat("\n=== Colesterol ===\n")
shapiro.test(placebo$Colesterol)
shapiro.test(farmacoA$Colesterol)
shapiro.test(farmacoB$Colesterol)

# Dado que los p-values obtenidos son mayores a 0.05 (indicando normalidad), se procede a comparar
# las medidas entre tratamientos usando la función t-test.
# Como t-test solo compara dos grupos a la vez, se realizarán las comparaciones:
# FarmacoA vs Placebo, FarmarcoB vs Placebo y FarmacoA vs FarmacoB.

cat("\n=== Comparativa de Medias (T-test) ===\n")

# Comparar el Farmaco A con el Placebo
cat("\n=== FarmacoA vs Placebo ===\n")
datos_ap <- subset(datos_biomed, Tratamiento %in% c("FarmacoA", "Placebo"))
t.test(Glucosa ~ Tratamiento, data = datos_ap)
t.test(Presion ~ Tratamiento, data = datos_ap)
t.test(Colesterol ~ Tratamiento, data = datos_ap)

# Comparar el Farmaco B con el Placebo
cat("\n=== FarmacoB vs Placebo ===\n")
datos_bp <- subset(datos_biomed, Tratamiento %in% c("FarmacoB", "Placebo"))
t.test(Glucosa ~ Tratamiento, data = datos_bp)
t.test(Presion ~ Tratamiento, data = datos_bp)
t.test(Colesterol ~ Tratamiento, data = datos_bp)

# Comparar el Farmaco A con el Farmaco B
cat("\n=== FarmacoA vs FarmacoB ===\n")
datos_ab <- subset(datos_biomed, Tratamiento %in% c("FarmacoA", "FarmacoB"))
t.test(Glucosa ~ Tratamiento, data = datos_ab)
t.test(Presion ~ Tratamiento, data = datos_ab)
t.test(Colesterol ~ Tratamiento, data = datos_ab)


# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
# El ANOVA lo que permite es evaluar si existen diferencias significativas entre los tres tratamientos.

anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos_biomed)
summary(anova_glucosa)

# Al ejecutar la función, se observa que Pr(>F) = 0.1. Como este valor es mayor a 0.05,
# se concluye que no hay grandes diferencias significativas entre los tratamientos en los niveles de glucosa.
