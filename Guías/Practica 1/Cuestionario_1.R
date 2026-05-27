# Cuestionario 1 (Práctica 1)

library(Lock5Data)
data("FloridaLakes")
help("FloridaLakes") 
data <- FloridaLakes

# 1. Indique cuántas variables hay en este dataset.
ncol(data)

# 4. Realice un histograma de la alcalinidad. Asegúrese de que se trate de un histograma de densidad.
# ¿Cuál es el máximo valor de la escala densidad? (utilice 4 decimales)
hist_alk <- hist(data$Alkalinity,
                 probability = TRUE)
names(hist_alk)
cat(round(max(hist_alk$density), 4))

# 5. En qué intervalo se alcanza?
i <- which.max(hist_alk$density)
hist_alk$breaks[i]        # límite inferior
hist_alk$breaks[i + 1]    # límite superior

# 6. Cuál espera que sea mayor para esta muestra de valores de alcalinidad, la media muestral o la mediana?
round(mean(data$Alkalinity), 4)
round(median(data$Alkalinity), 4)

# 10. Calcule la media podada al 20% (4 decimales)
round(mean(data$Alkalinity, trim = 0.2), 4)

# 12. Estime la probabilidad de que la alcalinidad sea menor o igual a 40mg/L
mean(data$Alkalinity <= 40)
# o sino 
sum(data$Alkalinity <= 40)/nrow(data)

# 13. boxplot de la alcalinidad
boxplot(data$Alkalinity)

# 14. densidad de la alcalinidad. 
plot(density(data$Alkalinity))
