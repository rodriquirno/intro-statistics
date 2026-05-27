# ================================
# Visualización simple de la distribución Beta
# ================================

# Limpiamos el entorno (opcional)
rm(list = ls())

# Secuencia de valores entre 0 y 1
x <- seq(0, 1, length.out = 1000)

# --------------------------------
# 1) Algunas distribuciones Beta típicas
# --------------------------------

# Dibujamos una ventana con 2 filas y 2 columnas
par(mfrow = c(2, 2))

# Beta(1,1): uniforme
plot(x, dbeta(x, 1, 1), type = "l", lwd = 2,
     main = "Beta(1,1): uniforme",
     xlab = "x", ylab = "densidad")

# Beta(2,5): masa más cerca de 0
plot(x, dbeta(x, 2, 5), type = "l", lwd = 2,
     main = "Beta(2,5): más cerca de 0",
     xlab = "x", ylab = "densidad")

# Beta(5,2): masa más cerca de 1
plot(x, dbeta(x, 5, 2), type = "l", lwd = 2,
     main = "Beta(5,2): más cerca de 1",
     xlab = "x", ylab = "densidad")

# Beta(0.5,0.5): forma en U
plot(x, dbeta(x, 0.5, 0.5), type = "l", lwd = 2,
     main = "Beta(0.5,0.5): forma en U",
     xlab = "x", ylab = "densidad")


par(mfrow = c(1,1))








############################################################
# CLASE PRACTICA  - SIMULACION DE ESTIMADORES EN R
#
# Queremos comparar tres estimadores de theta para una
# variable aleatoria con densidad:
#
#   f(x) = theta * x^(theta - 1),   0 < x < 1
#
# En la guía se indica que esta distribución es una Beta(theta, 1),
# así que en R podemos generar datos usando:
#   rbeta(n, shape1 = theta, shape2 = 1)
#
# Vamos a estudiar, mediante simulación:
#   1) el sesgo de cada estimador
#   2) el error cuadrático medio (ECM) de cada estimador
#
# Los estimadores que vamos a comparar son:
#
#   - Estimador de momentos:
#       theta_M = Xbarra / (1 - Xbarra)
#
#   - Estimador de máxima verosimilitud:
#       theta_MV = n / (-sum(log(Xi)))
#
#   - Estimador insesgado:
#       theta_I = (n - 1) / (-sum(log(Xi)))
#
############################################################


############################################################
# 0) LIMPIAR EL ENTORNO DE TRABAJO
############################################################

# Esto elimina los objetos guardados en memoria
rm(list = ls())


############################################################
# 1) FIJAR LOS PARAMETROS DEL PROBLEMA
############################################################

# Valor verdadero del parametro theta
theta_real <- 2

# Cantidad de simulaciones para cada tamaño muestral
N <- 1000

# Tamaños muestrales que vamos a estudiar
n_valores <- c(5, 10, 15, 30, 50, 75, 100, 150, 300, 500)

# Fijamos una semilla para que los resultados sean reproducibles
set.seed(123)


############################################################
# 2) GENERAR Y MIRAR UNA SOLA MUESTRA
############################################################

# Antes de hacer muchas simulaciones, conviene entender qué pasa
# con una sola muestra.

# Generamos una muestra de tamaño 10 de una Beta(theta, 1)
x <- rbeta(n = 10, shape1 = theta_real, shape2 = 1)

# Mostramos la muestra
x

# Calculamos la media muestral
mean(x)


############################################################
# 3) CALCULAR LOS 3 ESTIMADORES PARA ESA MUESTRA
############################################################

# Guardamos el tamaño muestral en una variable
n <- length(x)

# 3.1) Estimador de momentos
theta_M <- mean(x) / (1 - mean(x))

# 3.2) Estimador de maxima verosimilitud
theta_MV <- n / (-sum(log(x)))

# 3.3) Estimador insesgado
theta_I <- (n - 1) / (-sum(log(x)))

# Mostramos los tres resultados
theta_M
theta_MV
theta_I


############################################################
# 4) CREAR UNA FUNCION QUE CALCULE LOS 3 ESTIMADORES
############################################################

# Una función nos permite reutilizar el mismo cálculo muchas veces.
# La función recibe una muestra x y devuelve los 3 estimadores.

estimar_theta <- function(x) {
  
  # Tamaño de la muestra
  n <- length(x)
  
  # Estimador de momentos
  theta_M <- mean(x) / (1 - mean(x))
  
  # Estimador de maxima verosimilitud
  theta_MV <- n / (-sum(log(x)))
  
  # Estimador insesgado
  theta_I <- (n - 1) / (-sum(log(x)))
  
  # Devolvemos los resultados en un vector con nombres
  return(c(Momentos = theta_M,
           MV = theta_MV,
           Insesgado = theta_I))
}

# Probamos la función con la muestra generada antes
estimar_theta(x)


############################################################
# 5) HACER MUCHAS SIMULACIONES PARA UN SOLO TAMAÑO MUESTRAL
############################################################

# Vamos a arrancar con n = 10, para ver bien qué estamos haciendo.

n <- 10

# Creamos una matriz vacía para guardar los resultados de las N simulaciones
# Tendrá N filas (una por simulación) y 3 columnas (una por estimador)
resultados_n10 <- matrix(NA, nrow = N, ncol = 3)

# Les ponemos nombre a las columnas
colnames(resultados_n10) <- c("Momentos", "MV", "Insesgado")

# Bucle de simulación
for (i in 1:N) {
  
  # Generamos una muestra aleatoria de tamaño n
  x <- rbeta(n = n, shape1 = theta_real, shape2 = 1)
  
  # Calculamos los tres estimadores y los guardamos en la fila i
  resultados_n10[i, ] <- estimar_theta(x)
}

resultados_n10
# Miramos las primeras filas
head(resultados_n10)


############################################################
# 6) CALCULAR SESGO Y ECM PARA ESE TAMAÑO MUESTRAL
############################################################

# Recordemos:
#
# sesgo estimado = promedio de las estimaciones - valor verdadero
#
# ECM estimado = promedio de (estimacion - valor verdadero)^2

# 6.1) Sesgo estimado
sesgo_n10 <- colMeans(resultados_n10) - theta_real
sesgo_n10

# 6.2) ECM estimado
ecm_n10 <- colMeans((resultados_n10 - theta_real)^2)
ecm_n10


############################################################
# 7) REPETIR EL PROCESO PARA TODOS LOS TAMAÑOS MUESTRALES
############################################################

# Ahora hacemos exactamente lo mismo, pero para todos los valores de n
# guardados en n_valores.

# Creamos un data frame vacío para ir guardando los resultados
resumen <- data.frame()

# Recorremos todos los tamaños muestrales
for (n in n_valores) {
  
  # Matriz vacía para guardar las estimaciones en las N simulaciones
  resultados <- matrix(NA, nrow = N, ncol = 3)
  colnames(resultados) <- c("Momentos", "MV", "Insesgado")
  
  # Hacemos las N simulaciones
  for (i in 1:N) {
    
    # Generamos una muestra de tamaño n
    x <- rbeta(n = n, shape1 = theta_real, shape2 = 1)
    
    # Calculamos los tres estimadores
    resultados[i, ] <- estimar_theta(x)
  }
  
  # Calculamos sesgo para cada estimador
  sesgos <- colMeans(resultados) - theta_real
  
  # Calculamos ECM para cada estimador
  ecms <- colMeans((resultados - theta_real)^2)
  
  # Guardamos los resultados en formato "largo":
  # una fila por estimador y por tamaño muestral
  resumen <- rbind(resumen,
                   data.frame(n = n,
                              Estimador = c("Momentos", "MV", "Insesgado"),
                              Sesgo = sesgos,
                              ECM = ecms))
}

# Miramos las primeras filas del resumen
head(resumen)


############################################################
# 8) VER LA TABLA COMPLETA DE RESULTADOS
############################################################

resumen


############################################################
# 9) GRAFICOS BASICOS EN R BASE
############################################################

# Primero hacemos gráficos simples con funciones básicas de R.
# Después vamos a hacer una versión más prolija con ggplot2.


############################################################
# 9A) GRAFICO DEL SESGO
############################################################

# Separamos los datos por estimador
datos_M  <- resumen[resumen$Estimador == "Momentos", ]
datos_MV <- resumen[resumen$Estimador == "MV", ]
datos_I  <- resumen[resumen$Estimador == "Insesgado", ]

# Creamos el gráfico vacío
plot(datos_M$n, datos_M$Sesgo,
     type = "b",                    # puntos y líneas
     pch = 16,                      # tipo de punto
     lty = 1,                       # tipo de línea
     col = "blue",
     ylim = range(resumen$Sesgo),   # eje y que cubra todos los sesgos
     xlab = "Tamaño muestral n",
     ylab = "Sesgo estimado",
     main = "Sesgo de los estimadores")

# Agregamos los otros dos estimadores
lines(datos_MV$n, datos_MV$Sesgo, type = "b", pch = 17, lty = 2, col = "red")
lines(datos_I$n,  datos_I$Sesgo,  type = "b", pch = 15, lty = 3, col = "darkgreen")

# Agregamos una línea horizontal en y = 0
abline(h = 0, lwd = 2, col = "gray40")

# Agregamos leyenda
legend("topright",
       legend = c("Momentos", "MV", "Insesgado"),
       col = c("blue", "red", "darkgreen"),
       pch = c(16, 17, 15),
       lty = c(1, 2, 3),
       bty = "n")


############################################################
# 9B) GRAFICO DEL ECM
############################################################

plot(datos_M$n, datos_M$ECM,
     type = "b",
     pch = 16,
     lty = 1,
     col = "blue",
     ylim = range(resumen$ECM),
     xlab = "Tamaño muestral n",
     ylab = "ECM estimado",
     main = "ECM de los estimadores")

lines(datos_MV$n, datos_MV$ECM, type = "b", pch = 17, lty = 2, col = "red")
lines(datos_I$n,  datos_I$ECM,  type = "b", pch = 15, lty = 3, col = "darkgreen")

legend("topright",
       legend = c("Momentos", "MV", "Insesgado"),
       col = c("blue", "red", "darkgreen"),
       pch = c(16, 17, 15),
       lty = c(1, 2, 3),
       bty = "n")


############################################################
# 10) GRAFICOS MAS LINDOS CON GGPLOT2
############################################################

# Si el paquete no está instalado, descomentar la siguiente línea:
# install.packages("ggplot2")

library(ggplot2)


############################################################
# 10A) GRAFICO DEL SESGO CON GGPLOT2
############################################################

grafico_sesgo <- ggplot(resumen,
                        aes(x = n, y = Sesgo, color = Estimador)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Sesgo estimado según el tamaño muestral",
       x = "Tamaño muestral n",
       y = "Sesgo estimado") +
  theme_minimal(base_size = 13)

grafico_sesgo


############################################################
# 10B) GRAFICO DEL ECM CON GGPLOT2
############################################################

grafico_ecm <- ggplot(resumen,
                      aes(x = n, y = ECM, color = Estimador)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Error cuadrático medio según el tamaño muestral",
       x = "Tamaño muestral n",
       y = "ECM estimado") +
  theme_minimal(base_size = 13)

grafico_ecm

