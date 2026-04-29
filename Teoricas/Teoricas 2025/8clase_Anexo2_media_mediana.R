#Media y mediana
#Simulación bajo normalidad
est1 <- est2 <- 0
n <- 100
set.seed(1001)
for(i in 1:10000){
  x <- rnorm(n, 0, 1)
  est1[i] <- mean(x)
  est2[i] <- median(x)
}

plot(est2, ylab="")
points(est1, col=2)


errMV <- mean((est1 - 0)^2)
errMed <- mean((est2 - 0)^2)
c(errMV, errMed)
## [1] 0.009924647 0.015230840
boxplot(est1, est2)


hist(est1, probability = TRUE, xlim=c(-0.5, 0.5))


hist(est2, probability = TRUE, xlim=c(-0.5, 0.5))


#Simulación con datos atípicos
est1 <- est2 <- 0
n <- 100
nout <- 10
set.seed(1001)
for(i in 1:10000){
  x <- rnorm(n, 0, 1)
  x[1:nout] <-  rnorm(nout, 3, 1) 
  est1[i] <- mean(x)
  est2[i] <- median(x)
}

plot(est2, ylab="")
points(est1, col=2)


errMV <- mean((est1 - 0)^2)
errMed <- mean((est2 - 0)^2)
c(errMV, errMed)
## [1] 0.09997972 0.03624680
boxplot(est1, est2)


#M-estimador de posición
#Simulación bajo normalidad
library(RobStatTM)
## Warning: package 'RobStatTM' was built under R version 4.3.3
## 
## Attaching package: 'RobStatTM'
## The following object is masked from 'package:datasets':
## 
##     stackloss
est1 <- est2 <- est3 <- 0
n <- 100
set.seed(1001)
for(i in 1:10000){
  x <- rnorm(n, 0, 1)
  est1[i] <- mean(x)
  est2[i] <- median(x)
  est3[i] <- locScaleM(x)$mu
}

errMV <- mean((est1 - 0)^2)
errMed <- mean((est2 - 0)^2)
errM <- mean((est3 - 0)^2)
c(errMV, errMed, errM)
## [1] 0.009924647 0.015230840 0.010572236
boxplot(est1, est2, est3)


#Simulación con datos atípicos
est1 <- est2 <- est3 <- 0
n <- 100
nout <- 10
set.seed(1001)
for(i in 1:10000){
  x <- rnorm(n, 0, 1)
  x[1:nout] <-  rnorm(nout, 3, 1) 
  est1[i] <- mean(x)
  est2[i] <- median(x)
  est3[i] <- locScaleM(x)$mu
}

errMV <- mean((est1 - 0)^2)
errMed <- mean((est2 - 0)^2)
errM <- mean((est3 - 0)^2)

c(errMV, errMed, errM)
## [1] 0.09997972 0.03624680 0.03541407
boxplot(est1, est2, est3)
