glakes <- read.csv("glakes.csv")

# calcular_W toma como parametros la formula del modelo, la funcion
# predictora asociada y el dataset, y devuelve W segun el procedimiento
# del enunciado.
calcular_W <- function(formula, predecir_obs, data) {
  
  n <- nrow(data)
    
  Y_hat <- rep(0, n) # vector de valores predichos
  r     <- rep(0, n) # vector de residuos
  
  for (i in seq_len(n)) {
    model <- lm(formula, data = data[-i, ]) # modelo ajustado sin i-esima obs
    
    Y_hat[i] <- predecir_obs(coef(model), data, i)
    
    r[i]    <- data$Time[i] - Yhat[i]
  }
  
  sum(r^2)
}

# Modelo d) :  alpha + beta * Tonnage^0.25 + gamma * Tonnage^0.5
formula_d  <- log(Time) ~ I(Tonnage^0.25) + I(Tonnage^0.5)

# calcula Y_hat_i para el modelo_d
predecir_obs_d <- function(coeff, data, i) {
  exp(
    coeff[1]                          
    + coeff[2] * (data$Tonnage[i]^0.25)  
    + coeff[3] * (data$Tonnage[i]^0.5)   
    )
}

calcular_W(formula_d, Yhat_i_d, glakes)

