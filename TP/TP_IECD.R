glakes <- read.csv("glakes.csv")

calcular_W <- function(formula, predecir_obs, data) {
  n <- nrow(data)
  Y_hat <- rep(0, n) # vector de valores predichos
  r     <- rep(0, n) # vector de residuos
  
  for (i in seq_len(n)) { # LOOCV
    model    <- lm(formula, data = data[-i, ]) # modelo sin i-ésima obs 
    Y_hat[i] <- predecir_obs(coef(model), data, i) # valor predicho
    r[i]     <- data$Time[i] - Y_hat[i] # residuo
  }
  
  list(W = sum(r^2), residuos = r) # devuelve W y vector de resiudos
}

# Modelo (a): Time = beta * Tonnage 
formula_a <- Time ~ Tonnage - 1
predecir_obs_a <- function(coeff, data, i){
  coeff[1] * data$Tonnage[i]
}
res_a <- calcular_W(formula_a, predecir_obs_a, glakes)
cat("Estadístico W para el Modelo (a):", res_a$W, "\n")

# Modelo (b): Time = alpha + beta * Tonnage
formula_b <- Time ~ Tonnage
predecir_obs_b <- function(coeff, data, i) {
  coeff[1] + coeff[2] * data$Tonnage[i]
}

res_b <- calcular_W(formula_b, predecir_obs_b, glakes)
cat("Estadístico W para el Modelo (b):", res_b$W, "\n")


# Modelo (c): log(Time) = alpha + beta * Tonnage^0.25

formula_c <- log(Time) ~ I(Tonnage^0.25)

predecir_obs_c <- function(coeff, data, i) {
  exp(
    coeff[1] + 
      coeff[2] * (data$Tonnage[i]^0.25)
  )
}

res_c <- calcular_W(formula_c, predecir_obs_c, glakes)
cat("Estadístico W para el Modelo (c):", res_c$W, "\n")


# MODELO (d): log(Time) = alpha + beta * Tonnage^0.25 + gamma * Tonnage^0.5
formula_d  <- log(Time) ~ I(Tonnage^0.25) + I(Tonnage^0.5)

predecir_obs_d <- function(coeff, data, i) {
  exp(
    coeff[1] + 
      coeff[2] * (data$Tonnage[i]^0.25) + 
      coeff[3] * (data$Tonnage[i]^0.5)   
  )
}

res_d <- calcular_W(formula_d, predecir_obs_d, glakes)
cat("Estadístico W para el Modelo (d):", res_d$W, "\n")


# GRÁFICO 1 
# 1. Ajustar los modelos con el dataset completo
mod_a <- lm(Time ~ Tonnage - 1, data = glakes)
mod_b <- lm(Time ~ Tonnage, data = glakes)
mod_c <- lm(log(Time) ~ I(Tonnage^0.25), data = glakes)
mod_d <- lm(log(Time) ~ I(Tonnage^0.25) + I(Tonnage^0.5), data = glakes)

# ver cómo se comporta la predicción de cada modelo frente a los datos reales.
tonnage_seq <- seq(min(glakes$Tonnage), max(glakes$Tonnage), length.out = 200)
newdata <- data.frame(Tonnage = tonnage_seq)

pred_a <- predict(mod_a, newdata = newdata)
pred_b <- predict(mod_b, newdata = newdata)
pred_c <- exp(predict(mod_c, newdata = newdata))
pred_d <- exp(predict(mod_d, newdata = newdata))

# Configurar la ventana gráfica (1 fila, 1 columna)
par(mfrow = c(1, 1))

# Trazar el gráfico de dispersión original
plot(glakes$Tonnage, glakes$Time, 
     main = "Comparación de Modelos: Tiempo vs. Peso",
     xlab = "Tonnage", ylab = "Time", 
     pch = 16, col = "gray50")

# Superponer las curvas de cada modelo
lines(tonnage_seq, pred_a, col = "blue", lwd = 2, lty = 2)
lines(tonnage_seq, pred_b, col = "red", lwd = 2, lty = 2)
lines(tonnage_seq, pred_c, col = "darkgreen", lwd = 2, lty = 1)
lines(tonnage_seq, pred_d, col = "purple", lwd = 2, lty = 1)


legend("topleft", 
       legend = c("(a): Beta*T", "(b): Alpha + Beta*T", 
                  "(c): log(Time) ~ T^0.25", "(d): log(Time) ~ T^0.25 + T^0.5"),
       col = c("blue", "red", "darkgreen", "purple"), 
       lwd = 2, lty = c(2, 2, 1, 1), cex = 0.8)


# GRÁFICO 2
# Generar el Boxplot comparativo de los residuos
boxplot(
  abs(res_a$residuos), abs(res_b$residuos), 
  abs(res_c$residuos), abs(res_d$residuos), 
  names = c("(a)", "(b)", "(c)", "(d)"),
  main = "Comparación del Error Absoluto de Predicción",
  ylab = "Magnitud del Error Absoluto",
  col = c("lightblue", "pink", "lightgreen", "thistle"), 
  border = c("blue", "red", "darkgreen", "purple"),    
  pch = 19,      #(outliers)
  outcol = "gray50"
) 

