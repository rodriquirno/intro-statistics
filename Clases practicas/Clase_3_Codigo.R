# Ejercicio 3
# Limpiamos el entorno (opcional)
rm(list = ls())

intervalo <- function(datos, sigma, nivel){
  n <- length(datos)
  a <- mean(datos) - qnorm((1 + nivel)/2) * (sigma/sqrt(n))
  b <- mean(datos) + qnorm((1 + nivel)/2) * (sigma/sqrt(n))
  c(a,b)
}

# a)
set.seed(42)
datos_normales <- rnorm(5, 4, 3)

# b) 
ic <- intervalo(datos_normales, 3, 0.95)
ic           

# c)
N <- 1000

cobertura <- mean(replicate(N, {
  d  <- rnorm(5, 4, 3)
  ic <- intervalo(d, 3, 0.95)
  4 >= ic[1] & 4 <= ic[2]
}))
cobertura

# d)
intervalo_s <- function(datos, nivel) {
  s <- sqrt(var(datos))
  intervalo(datos, s, nivel)
}

cobertura_s <- mean(replicate(N, {
  d  <- rnorm(5, 4, 3)
  ic <- intervalo_s(d, 0.95)
  4 >= ic[1] & 4 <= ic[2]
}))
cobertura_s

# al usar s,  el intervalo queda más angosto de lo que debería
# lo correcto seria usar la t de Student

# e)
intervalo_t <- function(datos, nivel) {
  n  <- length(datos)
  tt <- qt((1 + nivel)/2, n - 1) # calculamos quantil de t de Student
  a  <- mean(datos) - tt * sqrt(var(datos)/n)
  b  <- mean(datos) + tt * sqrt(var(datos)/n)
  c(a,b)
}

cobertura_t <- mean(replicate(N, {
  d  <- rnorm(5, 4, 3)
  ic <- intervalo_t(d, 0.95)
  4 >= ic[1] & 4 <= ic[2]
}))
cobertura_t

