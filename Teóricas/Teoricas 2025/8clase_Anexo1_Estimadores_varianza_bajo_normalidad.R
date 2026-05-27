#n=10
estV1 <- estV2 <- estV3 <- 0
n <- 10
set.seed(1001)
for(i in 1:100000){
  x <- rnorm(n, 1, 2)
  s2 <- var(x) 
  estV1[i] <- s2
  estV2[i] <- s2 * (n-1)/n
  estV3[i] <- s2 * (n-1)/(n+1)
}
ecms2 <- mean((estV1 - 4)^2)
ecmsigmahat2 <- mean((estV2 - 4)^2)
ecmsigmahatopt2 <- mean((estV3 - 4)^2)
c(ecms2, ecmsigmahat2, ecmsigmahatopt2)
## [1] 3.577772 3.054843 2.918752


#n=100
estV1 <- estV2 <- estV3 <- 0
n <- 100
set.seed(1001)
for(i in 1:100000){
  x <- rnorm(n, 1, 2)
  s2 <- var(x) 
  estV1[i] <- s2
  estV2[i] <- s2 * (n-1)/n
  estV3[i] <- s2 * (n-1)/(n+1)
}
ecms2 <- mean((estV1 - 4)^2)
ecmsigmahat2 <- mean((estV2 - 4)^2)
ecmsigmahatopt2 <- mean((estV3 - 4)^2)
c(ecms2, ecmsigmahat2, ecmsigmahatopt2)
## [1] 0.3220671 0.3172863 0.3157678