peso <- reasda.table("bajoPeso.txt", header = TRUE)

# a)
plot(
    x = peso$apgar5,
    y = peso$presSist,
    xlab = "estado general bb",
    ylab = "presion sistolica",
    pch = 19,
    col = "grey"
)

# b) ajustar modelo lineal
modelo  <- lm(presSist ~ edadG + peso, data = peso)
resumen <- summary(modelo)

# c) presion estimada para eddadG = 31, apgar5 = 7
nueva_obs  <- data.frame(edadG = 31, apgar5 = 7)
prediccion <- predict(modelo, nueva_obs)

# d) intervalo de confianza para tita1
# primera forma
nivel <- 0.95
n     <- nrow(datos)
p     <- 3
t     <- qt((1 + nivel) / 2, df = n - p)

ic <- c(
    modelo$coefficients["edadG"] + t * resumen() # completar con formula
)

# segunda forma
confit(modelo, param = "edadG", level = nivel)

