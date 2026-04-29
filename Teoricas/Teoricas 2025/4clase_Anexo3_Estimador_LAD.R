# Mínimas desviaciones absolutas
# argminθ∑i=1,n, ∣xi−θ∣.
x <- rnorm(100, 1, 1)
S<-function(tita){
  sum(abs(x-tita))
}
titas <- seq(0, 3, 0.01)
ims <- sapply(titas, S)
plot(titas, ims)
abline(v=median(x))


x <- rexp(100, 1)
S<-function(tita){
  sum(abs(x-tita))
}
titas <- seq(0, 3, 0.01)
ims <- sapply(titas, S)
plot(titas, ims)
abline(v=median(x))
