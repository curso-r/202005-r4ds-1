# Exercícios de gráficos com ggplot2 --------------------------------------

library(tidyverse)

# Para fazer os exercícios abaixo, utilize a base IMDB.

imdb <- read_rds("dados/imdb.rds")

# Utilize títulos, labels e cores para que os gráficos
# fiquem bem formatados, como se eles fossem ser entregues
# em um relatório de trabalho ou publicados para o 
# público.

# 1. Faça um gráfico de dispersão (gráfico de pontos) da nota imdb
# contra o orçamento do filme.

grafico_disp <- imdb %>%
  mutate(
    orcamento = orcamento/1e6
  ) %>%
  ggplot(aes(x = orcamento, y = nota_imdb)) +
  geom_point() +
  labs(x = "Orçamento (em milhões de dólares)", y = "Nota IMDB", title = "Relação entre nota IMDB e Orçamento.")

# Desafio: trace uma reta vertical indicando o orçamento médio.
media_orcamento <- imdb %>%
  summarise(media = mean(orcamento/1e6, na.rm = TRUE))

grafico_disp +
  geom_vline(
    aes(xintercept = media), 
    data = media_orcamento, 
    colour = "red",
    size = 1,
    linetype = "dashed"
  )

# -------------------------------------------------------------------------

# 2. Rode o código abaixo. 

diretores <- c(
  "Steven Spielberg", 
  "Quentin Tarantino", 
  "Christopher Nolan",
  "Martin Scorsese"
)

imdb %>% 
  filter(diretor %in% diretores) %>% 
  group_by(ano, diretor) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = nota_media)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(diretor))

# a) Analisando o gráfico gerado, 
# descreva o que a função facet_wrap() faz.

# Dica: olhe a documentação da função facet_wrap().
help(facet_wrap)


# b) Utilize os argumentos nrow e ncol da função
# facet_wrap() para colocar os quatro gráficos
# em uma única coluna.

imdb %>% 
  filter(diretor %in% diretores) %>% 
  group_by(ano, diretor) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = nota_media)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(diretor), nrow = 1)

# -------------------------------------------------------------------------

# 3. Vamos fazer um gráfico de barras do número de
# filmes com nota maior que 8 ao longo dos anos.

# a. Crie uma nova coluna na base IMDB indicando
# se a nota de um filme é maior que 8 ou não.
imdb <- imdb %>%
  mutate(
    nota_maior_que_8 = ifelse(nota_imdb > 8, "maior que 8", "menor que 8")
  )

# Dica: use a função ifelse.

# b. Utilizando a coluna criada em (a), 
# crie uma tabela com o número anual de filmes
# com nota maior 8.
filmes_com_nota_maior_que_8 <- imdb %>%
  group_by(ano) %>%
  summarise(
    filmes_com_nota_maior_que_8 = sum(nota_maior_que_8 == "maior que 8")
  )

# c. Utilize a tabela criada em (b) para 
# fazer um gráfico de barras do número de
# filmes com nota maior que 8 ao longo dos anos.
filmes_com_nota_maior_que_8 %>%
  ggplot(aes(x = ano, y = filmes_com_nota_maior_que_8)) +
  geom_col() +
  labs(x = "Ano", y = "Contagem", title = "Filmes com notas IMDB maior do que 8 ao longo do tempo.") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))

# -------------------------------------------------------------------------

# 4. Juntando gráficos diferentes.

# a. Instale o pacote patchwork.
install.packages("patchwork")

# b. Faça um gráfico das nota médias dos
# filmes do Keanu Reeves ao longo dos anos.
# Salve o gráfico no objeto grafico_notas.

# Estrutura do código:
filmes_do_keanu_reeves <- imdb %>% 
  # codígo para manipular da base
  filter(
    ator_1 == "Keanu Reeves" | 
      ator_2 == "Keanu Reeves" | 
      ator_3 == "Keanu Reeves"
  )


grafico_notas <- filmes_do_keanu_reeves %>%
  group_by(ano) %>%
  summarise(
    nota_media = mean(nota_imdb, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = ano, y = nota_media)) +
  geom_line() +
  geom_point()
# código para gerar o gráfico

# c. Faça um histograma do lucro dos filmes
# filmes do Keanu Reeves.
# Salve o gráfico no objeto grafico_lucro.

grafico_lucro <- filmes_do_keanu_reeves %>%
  mutate(
    lucro = (receita - orcamento)/1e6
  ) %>%
  ggplot() +
  geom_histogram(aes(x = lucro), bins = 10)

# d. Rode o código abaixo e observe a figura gerada.
library(patchwork)

(grafico_notas + grafico_lucro)

# e. Por que não poderíamos usar a função
# facet_wrap() para gerar a mesma figura do item (d)?



