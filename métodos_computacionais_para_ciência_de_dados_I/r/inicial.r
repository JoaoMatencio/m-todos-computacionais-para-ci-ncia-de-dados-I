raio <- 10
altura <- 70
espessura <- 1
volume <- pi * raio^2 * altura
volume <- volume - pi * (raio - espessura)^2 * altura
print(volume)

numeric.vet <- c(1,3,1,9)
class(numeric.vet)
caractere.vet <- c('a','vv')
class(caractere.vet)

x <- 1:5
y <- c(2:4, 1, 2)
x == seq(1, 5)
x < y

altura <- c(150, 152, 145, 157, 167, 172, 175, 170, 165, 177, 162, 180, 160, 155, 147)
menor_160 <- altura[altura <= 160]
menor_170_maior_160 <- altura[altura <= 170 and altura > 160]
maior_170 <- altura[altura > 170]
remover_180 <- altura[altura != 180]