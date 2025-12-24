install.packages("markovchain")
install.packages("localScore") 
install.packages("knitr") 
library(knitr)
library(markovchain)  # Se carga la librería para cadenas de Markov
library(localScore) # Se carga la librería auxiliar
set.seed(12345)       # Se fija la semilla para reproducibilidad

# Se define la matriz de transición P
P <- matrix(c(
  0,   1,   0,   0,
  1/3, 0, 2/3,  0,
  0, 2/3,  0, 1/3,
  0,   0,   1,   0
), nrow = 4, byrow = TRUE)

# Se define una función que simula cadenas y devuelve la cuenta de estados finales
simula_cadenas <- function(n_sims, n_steps, P, init) {
  estados_finales <- integer(n_sims)
  for (sim in seq_len(n_sims)) {
    x <- init
    for (t in seq_len(n_steps)) {
      x <- sample(0:3, size = 1, prob = P[x + 1, ])
    }
    estados_finales[sim] <- x
  }
  return(table(factor(estados_finales, levels = 0:3)))
}

# Se simulan 1000 cadenas con 10000 y 10001 pasos (iniciando en 0)
resultados_10000 <- simula_cadenas(1000, 10000, P, 0)
resultados_10001 <- simula_cadenas(1000, 10001, P, 0)

# Se muestra una tabla con frecuencias y error absoluto respecto a los límites esperados
knitr::kable(data.frame(Estado = 0:3,
                        "10000 transiciones " = as.integer(resultados_10000),
                        "10001 transiciones" = as.integer(resultados_10001),
                        "Error absoluto" = abs(c(as.integer(resultados_10000) + as.integer(resultados_10001))/1000 - c(0.25, 0.75, 0.75, 0.25)),
                        check.names = FALSE),
             caption = "Frecuencias de estados finales 1000 simulaciones")

# Se grafican las frecuencias observadas
par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
barplot(resultados_10000,
        main = "10000 pasos",    # Se grafica la cuenta para 10000 pasos
        xlab = "Estado Final",
        ylab = "Frecuencia de 1000",
        col = "lightyellow",
        ylim = c(0, 800))
barplot(resultados_10001,
        main = "10001 pasos",    # Se grafica la cuenta para 10001 pasos
        xlab = "Estado Final",
        ylab = "Frecuencia de 1000",
        col = "#FA937E",
        ylim = c(0, 800))

# Se calcula la distribución estacionaria teórica
stationary_distribution(P)  # Se calcula la distribución estacionaria de P

# Se calcula el periodo de la cadena definida con P
period(new("markovchain", states = c("A", "B", "C", "D"), transitionMatrix = P)) # Se calcula el periodo

# Se calcula el error medio absoluto respecto a los valores límite esperados
sum(abs(c(as.integer(resultados_10000) + as.integer(resultados_10001))/1000 - c(0.25, 0.75, 0.75, 0.25)))/4

