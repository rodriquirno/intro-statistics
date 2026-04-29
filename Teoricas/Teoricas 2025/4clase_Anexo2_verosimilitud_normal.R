x <- c(
  2.5,4.1,1.8,1.8,2.2,1.6,2.5,0.1,3.0,4.4,3.5,2.3,1.1,1.4,2.8,0.4,2.7,
  4.0,1.1,1.6,2.1,2.7,2.5,2.2,2.5,1.1,0.6,1.3,0.6,1.7
)

L <- function(mu, sigma2) {
  x <- rnorm(100, 2, 1)
  n <- length(mu)
  sapply(1:n, function(i) prod(dnorm(x, mean=mu[i], sd=sqrt(sigma2[i]))))
}


# Grid over x and y
mus <- seq(1, 3, length = 50)
sigma2s <- seq(0.5, 2, length = 50)

# Compute function values
z <- outer(mus, sigma2s, L)

# Contour plot
contour(mus, sigma2s, z, main = "Contour of L")


# Heatmap style image
image(mus, sigma2s, z, main = "Heatmap of f(x,y)")


persp(mus, sigma2s, z,
      theta = 30, phi = 20,  # rotation angles
      expand = 0.7,
      col = "lightblue",
      shade = 0.5,
      ticktype = "detailed",
      main = "Surface plot of f(x,y)")


persp(mus, sigma2s, z,
      theta = 90, phi = 20,  # rotation angles
      expand = 0.7,
      col = "lightblue",
      shade = 0.5,
      ticktype = "detailed",
      main = "Surface plot of f(x,y)")


persp(mus, sigma2s, z,
      theta = 60, phi = 20,  # rotation angles
      expand = 0.7,
      col = "lightblue",
      shade = 0.5,
      ticktype = "detailed",
      main = "Surface plot of f(x,y)")
