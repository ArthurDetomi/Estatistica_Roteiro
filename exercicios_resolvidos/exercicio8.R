# populacao
N <- 4250
# estratos
E1 <- 1500
E2 <- 650
E3 <- 800
E4 <- 550
E5 <- 750
#numerar elementos
adm <- seq(1:E1)
arqui <- seq(1:E2)
ccomp <- seq(1:E3)
jorn <- seq(1:E4)
econo <- seq(1:E5)
# calculando proporcoes
proporcao <- function(estrato) {
  return(round(estrato / N, 2));
}
P1 <- proporcao(E1)
P2 <- proporcao(E2)
P3 <- proporcao(E3)
P4 <- proporcao(E4)
P5 <- proporcao(E5)
P <- sum(P1, P2, P3, P4, P5)
# quantidade de cada estrato iremos pegar
n <-  50 # tamanho da amostra
Q1 <- round(n * P1, 0)
Q2 <- round(n * P2, 0)
Q3 <- round(n * P3, 0)
Q4 <- round(n * P4, 0)
Q5 <- round(n * P5, 0)

# montando tabela
tab <- matrix(0, 6, 3)

colnames(tab) <- c("Estrato","Proporcao",
                   "Quantidade")
rownames(tab) <- c("Administração","Arquitetura",
                   "Ciência da Computação","Jornalismo",
                   "Economia","Total")
tab[1:6,1] <- c(E1, E2, E3, E4, E5, N)
tab[1:6,2] <- c(P1, P2, P3, P4, P5, P)
tab[1:6,3] <- c(Q1, Q2, Q3, Q4, Q5, n)
tab
# Amostragem estratificada
ae1 <- sample(adm, Q1);ae1
ae2 <- sample(arqui, Q2);ae2
ae3 <- sample(ccomp, Q3);ae3
ae4 <- sample(jorn, Q4);ae4
ae5 <- sample(econo, Q5);ae5

ae <- c(ae1, ae2, ae3, ae4, ae5);ae
sort(ae);ae

