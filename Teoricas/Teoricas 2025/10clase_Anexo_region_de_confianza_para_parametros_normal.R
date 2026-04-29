#Regiones de confianza para los param de la normal
#Calculo los estadîsticos y cuantiles necesarios
n<-10
alfa <- 0.05
beta <- 1-sqrt(1-0.05)
beta
## [1] 0.02532057
qchisq(1 - beta/2, n-1)
## [1] 20.99778
qchisq(beta/2, n-1)
## [1] 2.227415
qchisq(1 - beta/2, n-1)
## [1] 20.99778
qchisq(beta/2, n-1)
## [1] 2.227415
set.seed(1000)
x <- rnorm(2,3)

xraya <- mean(x)
s <- sd(x)
#Calculo una región de nivel mayor o igual a 1−α
#por el método de Bonferroni
l1bonf_mu <- xraya-qt(1-alfa/4, n-1)*s/sqrt(n)
l2bonf_mu <- xraya+qt(1-alfa/4, n-1)*s/sqrt(n)
l1bonf_sigmacuad <- (n-1)*var(x)/qchisq(1-alfa/4, n-1)
l2bonf_sigmacuad <- (n-1)*var(x)/qchisq(alfa/4, n-1)


mus <- seq(0,1)
sigma2s <- seq(0,1)
plot(mus, sigma2s, type ="n", xlim=c(1.6, 2.7), ylim=c(0,2))
abline(v=l1bonf_mu, col="blue")  
abline(v=l2bonf_mu, col="blue")  
abline(h=l1bonf_sigmacuad, col="blue")  
abline(h=l2bonf_sigmacuad, col="blue")  
rect(l1bonf_mu, l1bonf_sigmacuad, 
     l2bonf_mu, l2bonf_sigmacuad, 
     col = adjustcolor("lightblue", alpha.f = 0.3), border = NA)


#Calculo una región de nivel igual a 1−α
#usando el superteorema
l1var <- var(x)*(n-1)/qchisq(1-beta/2, n-1)
l2var <- var(x)*(n-1)/qchisq(beta/2, n-1)

mus <- seq(l1bonf_mu-0.2,l2bonf_mu+0.2, 0.01)
sigma2s <- (mus-xraya)^2 * n/qnorm(1-beta/2)
plot(mus, sigma2s, type ="l", xlim=c(1.6, 2.7))

abline(h=l1var)
abline(h=l2var)

# Shade the band between l1var and l2var under the curve
polygon(
  c(mus, rev(mus)),
  c(pmin(pmax(sigma2s, l1var), l2var), rep(l2var, length(mus))),
  col = adjustcolor("lightgreen", alpha.f = 0.3),
  border = NA
)


#Grafico las dos superpuestas
plot(mus, sigma2s, type ="l", xlim=c(1.6, 2.7))

abline(h=l1var)
abline(h=l2var)

# Shade the band between l1var and l2var under the curve
polygon(
  c(mus, rev(mus)),
  c(pmin(pmax(sigma2s, l1var), l2var), rep(l2var, length(mus))),
  col = adjustcolor("lightgreen", alpha.f = 0.3),
  border = NA
)
abline(v=l1bonf_mu, col="blue")  
abline(v=l2bonf_mu, col="blue")  
abline(h=l1bonf_sigmacuad, col="blue")  
abline(h=l2bonf_sigmacuad, col="blue")  
rect(l1bonf_mu, l1bonf_sigmacuad, 
     l2bonf_mu, l2bonf_sigmacuad, 
     col = adjustcolor("lightblue", alpha.f = 0.3), border = NA)