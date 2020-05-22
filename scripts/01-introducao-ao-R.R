# INTRODUÇÃO AO R

# Rodando códigos (o R como calculadora) ----------------------------------

# ATALHO para rodar o código: CTRL + ENTER

# adição
1 + 1

# subtração
4 - 2

# multiplicação
2 * 3

# divisão
5 / 3

# potência
4 ^ 2

days <- 43 * 30 * 12

1 + 

# Criando objetos/variáveis -----------------------------------------------

# Salvando o valor 1 no objeto "obj"
obj <- 1
obj2 = 1
obj

# Também dizemos 'guardando as saídas'
soma <- 2 + 2
soma

# ATALHO para a <- : ALT (option) - (alt menos)

# O R difencia minúscula de maiúscula!

a <- 5
A <- 42

a
A

# Os nomes devem começar com uma letra. 
# Podem conter letras, números, _ e .

# Permitido

x <- 1
x1 <- 2
objeto <- 3
meu_objeto <- 4
meu.objeto <- 5

# Não permitido

1x <- 1
_objeto <- 2
meu-objeto <- 3

# Estilo de nomes

eu_uso_snake_case
outrasPessoasUsamCamelCase
algumas.pessoas.usam.pontos
E_algumasPoucas.Pessoas_RENUNCIAMconvenções

# Exercícios --------------------------------------------------------------

# 1. Linhas que começam com # são comentários. 
# Elas são ignoradas na hora
# de avaliar o código. Coloque o cursor em 
# cima de um comentário, pressione
# CTRL + ENTER e tente entender qual operação 
# o RStudio realizou no Console.

# 2. Multiplique a sua idade por 12 
# e salve em um objeto chamado "meses".

meses <- 20.5 * 12
meses

rm(meses)

# Data frames -------------------------------------------------------------

mtcars

# Classes -----------------------------------------------------------------

# Podemos somar dois números
1 + 2

# Não podemos somar duas letras (texto)
"a" + "b"

##############################
# Use aspas para criar texto #
##############################

a <- 10
rm(a)
# O objeto a, sem aspas
a

# A letra (texto) a, com aspas
"a"
'a'
# Numéricos (numeric)

a <- 10
class(a)

# Caracteres (character, strings)

obj <- "a"
obj2 <- "masculino"

# string é um nome também dado aos characteres

class(obj)
class(obj2)

str(obj)

# lógicos (logical, booleanos)

verdadeiro <- TRUE
falso <- FALSE

# Isso dá erro
# TRUE <- 1

class(verdadeiro)
class(falso)

# Data frames

mtcars
class(mtcars)

str(mtcars)

# Vetores -----------------------------------------------------------------

# Vetores são conjuntos indexado de valores

vetor1 <- c(1, 4, 3, 10)
vetor2 <- c("a", "b", "z")

vetor1
vetor2

# Uma maneira fácil de criar um vetor com
# uma sequência de números 
# é utilizar o operador `:`

letters

# Vetor de 1 a 10
1:10

# Vetor de 10 a 1
10:1

# Vetor de -3 a 3
-3:3

# Quando dizemos que vetores são conjuntos indexados, 
# isso quer dizer que cada valor 
# dentro de um vetor tem uma posição

vetor <- c("a", "b", "c", "d")

vetor2 <- vetor[1:3]
vetor2

vetor[1]
vetor[2]
vetor[3]
vetor[4]

vetor[1:3]
vetor[c(2, 3)]
vetor[c(1, 2, 4)]

vetor[5]

vetor[c(-1,-3)]

# Um vetor só pode guardar um tipo de objeto e 
# ele terá sempre 
# a mesma classe dos objetos que guarda

vetor1 <- c(1, 5, 3, -10)
vetor2 <- c("a", "b", "c")

class(vetor1)
class(vetor2)

# Se tentarmos misturar duas classes, o R vai apresentar o 
# comportamento conhecido como coerção

# 20, 21, 23, a, 20, 15

vetor <- c(1, 2, "a")
vetor

class(vetor)

# Naturalmente, podemos fazer operações matemáticas com vetores

vetor <- c(0, 5, 20, -3)

vetor + c(1, 1, 1, 1)
vetor - 1
vetor / 2
vetor * 10

c(0, 5, 20, -3) + c(1, 1, 1, 1)

# Você também pode fazer operações que envolvem mais de um vetor:

vetor1 <- c(1, 2, 3)
vetor2 <- c(10, 20, 30)

vetor1  + vetor2

# As coisas ficam um pouco confusas quando os dois 
# vetores não tem o mesmo tamanho

vetor1 <- c(1, 2)
vetor2 <- c(10, 20, 30, 40)

vetor1 + vetor2

# Esse comportamento é chamado de reciclagem.

# As coisas ficam um pouco mais confusas quando os comprimentos
# dos vetores não são múltiplos

vetor1 <- c(1, 2, 3)
vetor2 <- c(10, 20, 30, 40, 50)

vetor1 + vetor2

# Exercícios --------------------------------------------------------------

vetor1 <- c(1, 2, 3)

vetor1[1]
vetor1[-1]

novo_vetor <- vetor1[1]
vetor1 <- vetor1[1]

# logical < numeric < character

# a. Guarde em um objeto uma sequência de números que comece 
# em 0 e termine em 5.

meu_objeto <- 0:5
meu_objeto <- c(0, 1, 2, 3, 4, 5)

# b. Use subsetting para fazer o R devolver o 
# primeiro número dessa sequência.
# Em seguida, faça o R devolver o último número da sequência.

meu_objeto[1]
meu_objeto[6]

# c. Multiplique todos os valores do vetor por -1. 
# Guarde o resultado em
# um novo objeto.

novo_objeto <- meu_objeto * (-1)
novo_objeto

# Comparações lógicas ------------------------------------------------------

# Valores lógicos

class(TRUE)
class(FALSE)

# TRUE e FALSE são nomes reservados

TRUE <- 100

# Testes com resultado verdadeiro
1 == 1
"a" == "a"

# Testes com resultado falso
1 == 2
"a" == "b"

# Diferente
1 != 1
1 != 2

# Maior
3 > 3
3 > 2

# Maior ou igual
3 >= 4
3 >= 3

# Menor 
3 < 3
3 < 4

# Menor ou igual
3 <= 2
3 <= 3

3 %in% c(1, 2, 3)
"a" %in% c("b", "c")

# Comprações lógicas serão a base dos filtros!

minha_coluna <- c(1, 3, 0, 10, -1, 5, 20)

minha_coluna > 3
minha_coluna > c(3, 3, 3, 3, 3, 3, 3)

minha_coluna[minha_coluna > 3]

minha_coluna[c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)]

# Exercícios --------------------------------------------------------------

# 1. O que acontece se tentarmos misturar valores lógicos e texto em um
# vetor? E se misturarmos valores lógicos e números?

c(TRUE, FALSE, "a")
c(TRUE, FALSE, 33)

minha_coluna <- c(1, 3, 0, 10, -1, 5, 20)

sum(minha_coluna > 3)

# 2. Escreva um código que devolva apenas os valores maiores
# ou iguais a 10 do vetor abaixo:

vetor <- c(4, 8, 15, 16, 20, 42)

resultado_do_teste <- vetor >= 10

vetor[vetor >= 10]
vetor[resultado_do_teste]

# [DESAFIO] Escreva um código que devolta apenas os valores pares
# do vetor acima.

vetor[vetor %% 2 == 0]

# Dica: o operador %% devolve o resto da divisão entre dois números

3 %% 2

vetor[c(1, 2, 4, 6)]

# Valores especiais -------------------------------------------------------


vetor[10]

# Existem valores reservados para representar dados faltantes, 
# infinitos, e indefinições matemáticas.

NA   # (Not Available) significa dado faltante/indisponível. 

NaN  # (Not a Number) representa indefinições matemáticas, 
# como 0/0 e log(-1). 
# Um NaN é um NA, mas a recíproca não é verdadeira.

log(c(1, 10, -1, 2))
c(log(1), log(10), log(-1), log(2))

Inf  # (Infinito) é um número muito grande ou o limite matemático, 
# por exemplo, 
# 1/0 e 10^310. Aceita sinal negativo -Inf.

NULL # representa a ausência de objeto.

# Comparações lógicas

c("a", "b", NA)

"a" == NA

idade_ana <- 30
idade_beto <- NA
idade_carla <- NA

idade_ana == idade_beto

idade_beto == idade_carla


# Use as funções is.na(), is.nan(), is.infinite() e is.null() 
# para testar se um objeto é um desses valores.

x <- NA
is.na(x)

0/0 == NaN
is.nan(0/0)

minha_coluna <- c(1, 2, 3, NA, 5)
is.na(minha_coluna)

minha_coluna[is.na(minha_coluna)]

# ! é o operador negação

!TRUE
!FALSE

!is.na(minha_coluna)
minha_coluna[!is.na(minha_coluna)]

is.nan(NaN)
is.infinite(10^309)
is.null(NULL)

minha_coluna <- c(0, 10, 2, 55, 100)
log_coluna <- log(minha_coluna)
mean(log_coluna)

is.infinite(log_coluna)

# Data frames, o retorno --------------------------------------------------

mtcars

# Selecionando uma coluna do data frame

mtcars$mpg
mtcars$cyl

# A classe data frame tem uma característica 
# especial: dimensão

dim(mtcars)

vetor <- c(1, 2)
dim(vetor)

matriz <- matrix(1:4, 2, 2)
dim(matriz)

# Subsetting em objetos com 2 dimensões

# Sinxtaxe: data_frame[indice_linha, indice_coluna]

mtcars[1,1]
mtcars[,1]
mtcars[1,]

vetor[1]

# Selecionando colunas

mtcars[, c(1, 2)]
mtcars[, c("mpg", "am")]

# Filtrando linhas

mtcars$cyl
mtcars$cyl == 4

mtcars[mtcars$cyl == 4, ]
mtcars[mtcars$mpg > 25, ]

# Funções -----------------------------------------------------------------

# Funções são nomes que guardam um código de R. Esse código é
# avaliado quando rodamos uma função.

# a função `c()` foi utilizada para criar vetores;
# a função `class()` foi utilizada para descobrir a classe de um objeto;

# Argumentos

# Argumentos são sempre separados por vírgulas

c(1, 3, 5)

# A ordem é importante se você não nomear os argumentos

seq(from = 4, to = 10, by = 2)
seq(4, 10, 2)

seq(by = 2, to = 10, from = 4)
seq(2, 10, 4)

?seq
help(seq)

# Funções têm personalidade. Cada uma pode funcionar de um jeito
# diferente das demais.

# Exemplo 1

sum(1, 2)
sum(c(1, 2))

mean(1, 2)
mean(c(1, 2))

# Exemplo 2

mean(c(1, 2, NA))
mean(c(1, 2, NA), na.rm = TRUE)

cor(mtcars$mpg, mtcars$wt)

cor(c(1, 3, 2), c(3, NA, 10), na.rm = TRUE)

cor(c(1, 3, 2, 8), c(3, NA, 10, 9), use = "complete.obs")
cor(c(1, 2, 8), c(3, 10, 9))

log(5)

log(c(3, 5))

log(c(3, 5, NA))

# Colunas de data frames são vetores

mean(mtcars$mpg)

# Funções dentro de funções

sample(1:10, 100, replace = TRUE)

mean(log(sample(1:10, 100, replace = TRUE)))

# Criando funções

minha_soma <- function(x, y) {
  
  soma <- x + y
  
  return(soma)
  
}

minha_cola <- function(x, y) {
  
  cola <- paste(x, y)
  
  return(cola)
  
}

minha_soma(x = 1, y = 0)
minha_cola(x = "a", y = "b")

# Exercícios --------------------------------------------------------------

# 1. Use a funcao 'sum' para somar os valores de 1 a 100

sum(1:100)

# 2. A função colMeans serve para tirar a média das colunas 
# de um data frame. Use essa função para calcular a média de
# todas as colunas do mtcars.

colMeans(mtcars)
?colMeans

# Dica: veja a documentação da função colMeans se você não
# souber como utilizá-la.

# DESAFIO. Crie uma função que recebe 2 números
# e devolve a raiz quadrada da soma desses números.
# Dica: sqrt() é a função para raiz quadrada.

minha_raiz_quadrada <- function(a, b) {
  
  soma <- a + b
  raiz <- sqrt(soma)
  
  return(raiz)
  
}

minha_raiz_quadrada("aagag", 3)

# Pacotes -----------------------------------------------------------------

# Para instalar pacotes

install.packages("tidyverse")
install.packages(c("tidyverse", "rmarkdown", "devtools"))

install.packages("remotes")

remotes::install_github("curso-r/CursoR")

# Para carregar pacotes

library(tidyverse)
library(dplyr)

# Também é possível acessar as funções usando ::

dplyr::select()

CursoR::atualizar_material("r4ds1")





