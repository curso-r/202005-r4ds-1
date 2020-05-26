# solucao 1
maximo <- function(x, y) {
  max(x, y)
}

maximo(55, -55)

# solucao 2
maximo <- function(x, y) {
  if(x > y) {
    x
  } else if (x < y) {
    y
  } else if(x == y) {
    x
  }
}

maximo(4, 3)

# solucao 3
maximo <- function(x, y) {
  resp <- c(x, y)
  onde_esta_o_maximo <- c(x > y, y > x)
  resp[onde_esta_o_maximo]
}

maximo(4, 3)
