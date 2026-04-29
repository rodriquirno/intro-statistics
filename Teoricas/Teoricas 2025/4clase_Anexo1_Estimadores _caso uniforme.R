tita <- 2
x <- runif(100, 0, tita)
titahatmom <- 2 * mean(x)
titahatmv <- max(x)
titahatmom
## [1] 1.855503
titahatmv
## [1] 1.995327
titahatmom <- titahatmv <- 0
for(i in 1:1000){
  x <- runif(100, 0, tita)
  titahatmom[i] <- 2 * mean(x)
  titahatmv[i] <- max(x)
}
plot(titahatmom)
points(titahatmv, col = 2)
boxplot(titahatmom, titahatmv)
hist(titahatmom, probability = TRUE)
hist(titahatmv, probability = TRUE)