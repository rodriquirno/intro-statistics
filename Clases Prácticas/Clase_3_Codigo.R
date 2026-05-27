# Ejercicio 3
intervalos <- function(datos, sigma, nivel){
  alpha <- 1 - nivel
  n <- length(datos)
  a <- (mean(datos)- qnorm(1 - alpha/2)* (sigma/sqrt(n)))
  b <- (mean(datos)+ qnorm(1- alpha/2)* (sigma/sqrt(n)))
  c(a,b)
}

# a)
set.seed(123)
dn <- rnorm(5,4,3)

# b)
ic <- intervalos(dn,3,0.95)
ic

# c)
k <- 1000
counter <- 0
i <- 1

for (i in 1:1000){
  d <- rnorm(5,4,3)
  c <- intervalos(d, 3, 0.95)
  if (4 <= c[2] & 4 >= c[1]){
   counter <- counter + 1
  }
}
p <- counter/1000
p

# d)
intervalo_s <- function(datos, nivel) {
  alpha <- 1 - nivel
  n <- length(datos)
  s <- sqrt(var(datos))
  a <- (mean(datos)- qnorm(1 - alpha/2)* (s/sqrt(n)))
  b <- (mean(datos)+ qnorm(1- alpha/2)* (s/sqrt(n)))
  c(a,b)
}
ic_s <- intervalo_s(dn, 0.95)
cat("length(IC) =", )