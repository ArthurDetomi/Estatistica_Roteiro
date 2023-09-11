amostragem_sistematica <- function(N, n) {
  populacao <- seq(1:N)
  intervalo <- trunc(N/n)
  seq_sort <- seq(1:intervalo)
  valor_aleatorio <- sample(seq_sort, size = 1, replace = FALSE)
  seq_aleatoria <- seq(from = valor_aleatorio, by = intervalo, length.out = n)
  return(list(seq_aleatoria))
}

result <- amostragem_sistematica(8500, 50)
result
