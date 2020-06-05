
# Carregar pacotes --------------------------------------------------------

library(tidyverse)

# Ler base IMDB -----------------------------------------------------------

imdb <- read_rds("dados/imdb.rds")

imdb <- imdb %>% mutate(lucro = receita - orcamento)

# Filosofia ---------------------------------------------------------------

# Um gráfico estatístico é uma representação visual dos dados 
# por meio de atributos estéticos (posição, cor, forma, 
# tamanho, ...) de formas geométricas (pontos, linhas,
# barras, ...). Leland Wilkinson, The Grammar of Graphics

# Layered grammar of graphics: cada elemento do 
# gráfico pode ser representado por uma camada e 
# um gráfico seria a sobreposição dessas camadas.
# Hadley Wickham, A layered grammar of graphics 

# Gráfico de pontos (dispersão) -------------------------------------------

# ggplot2::ggplot()

# Apenas o canvas
imdb %>% 
  ggplot()

# Salvando em um objeto
p <- imdb %>% 
  ggplot()

# Gráfico de dispersão da receita contra o orçamento
imdb %>% 
  ggplot() +
  geom_point(aes(x = orcamento, y = receita))

# Inserindo a reta x = y
imdb %>%
  ggplot() +
  geom_point(aes(x = orcamento, y = receita)) +
  geom_abline(intercept = 0, slope = 1, color = "red")

# y = a + b * x
# y = 0 + 1 * x
# y = x

# Observe como cada elemento é uma camada do gráfico.
# Agora colocamos a camada da linha antes da camada
# dos pontos.
imdb %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_point(aes(x = orcamento, y = receita))

# Atribuindo a variável lucro aos pontos
imdb %>%
  ggplot() +
  geom_point(aes(x = orcamento, y = receita, color = lucro))

# Categorizando o lucro antes
imdb %>%
  mutate(
    lucrou = ifelse(lucro <= 0, "Não", "Sim")
  ) %>%
  filter(!is.na(lucrou)) %>%
  ggplot() +
  geom_point(aes(x = orcamento, y = receita, color = lucrou))

# Salvando um gráfico em um arquivo
meu_grafico <- imdb %>%
  mutate(
    lucrou = ifelse(lucro <= 0, "Não", "Sim")
  ) %>%
  ggplot() +
  geom_point(aes(x = orcamento, y = receita, color = lucrou))

ggsave(filename = "meu_grafico.png", plot = meu_grafico)

# imdb %>%
#   mutate(
#     lucrou = ifelse(lucro <= 0, "Não", "Sim")
#   ) %>% 
#   select(lucro, lucrou) %>% View

# Exercícios --------------------------------------------------------------

# a. Crie um gráfico de dispersão da nota do imdb pelo orçamento.

imdb %>% 
  ggplot() +
  geom_point(aes(y = nota_imdb, x = orcamento))

# b. Pinte todos os pontos do gráfico de azul.

imdb %>% mutate(nova_coluna = "blue") %>% View

imdb %>% 
  ggplot() +
  geom_point(aes(y = nota_imdb, x = orcamento), color = "blue")

# Gráfico de linhas -------------------------------------------------------

# Nota média dos filmes ao longo dos anos

imdb %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>%  
  ggplot() +
  geom_line(aes(x = ano, y = nota_media)) +
  geom_point(aes(x = ano, y = nota_media))

# Número de filmes coloridos e preto e branco por ano 

imdb %>% 
  filter(!is.na(cor)) %>% 
  group_by(ano, cor) %>% 
  summarise(num_filmes = n()) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = num_filmes, color = cor))

# Nota média do Robert De Niro por ano
imdb %>% 
  filter(ator_1 == "Robert De Niro") %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = nota_media))

# Colocando pontos no gráfico
imdb %>% 
  filter(ator_1 == "Robert De Niro") %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = nota_media)) +
  geom_point(aes(x = ano, y = nota_media))

# Reescrevendo de uma forma mais agradável
imdb %>% 
  filter(ator_1 == "Robert De Niro") %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = nota_media)) +
  geom_line() +
  geom_point()

# Colocando as notas no gráfico
imdb %>% 
  filter(ator_1 == "Robert De Niro") %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  mutate(nota_media = round(nota_media, 1)) %>% 
  ggplot(aes(x = ano, y = nota_media)) +
  geom_line() +
  geom_label(aes(label = nota_media))


# Exercício ---------------------------------------------------------------

# Faça um gráfico do orçamento médio dos filmes ao longo dos anos.

imdb %>% 
  group_by(ano) %>% 
  summarise(orc_medio = mean(orcamento, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = orc_medio))

# Gráfico de barras -------------------------------------------------------

# imdb %>% 
#   top_n(10, receita)

# Número de filmes dos diretores da base
imdb %>% 
  count(diretor, name = "qtd_filmes") %>% 
  top_n(10, qtd_filmes) %>%
  ggplot() +
  geom_col(aes(x = diretor, y = n))

# Tirando NA e pintando as barras
imdb %>% 
  count(diretor) %>%
  filter(!is.na(diretor)) %>% 
  top_n(10, n) %>%
  ggplot() +
  geom_col(
    aes(x = diretor, y = n, fill = diretor),
    color = "black",
    show.legend = FALSE
  )

# Invertendo as coordenadas
imdb %>% 
  count(diretor) %>%
  filter(!is.na(diretor)) %>% 
  top_n(10, n) %>%
  ggplot() +
  geom_col(
    aes(x = diretor, y = n, fill = diretor),
    show.legend = FALSE
  ) +
  coord_flip()
  
as.numeric(as.factor(imdb$diretor[1:5]))

as.numeric(imdb$diretor[1:5])

library(forcats)

# Ordenando as barras
imdb %>% 
  count(diretor) %>%
  filter(!is.na(diretor)) %>% 
  top_n(10, n) %>%
  mutate(
    diretor = forcats::fct_reorder(diretor, n)
  ) %>% 
  ggplot() +
  geom_col(
    aes(x = diretor, y = n, fill = diretor),
    show.legend = FALSE
  ) +
  coord_flip()

# Colocando label nas barras
imdb %>% 
  count(diretor) %>%
  filter(!is.na(diretor)) %>% 
  top_n(10, n) %>%
  mutate(
    diretor = forcats::fct_reorder(diretor, n)
  ) %>% 
  ggplot() +
  geom_col(
    aes(x = diretor, y = n, fill = diretor),
    show.legend = FALSE
  ) +
  geom_label(aes(x = diretor, y = n/2, label = n)) +
  coord_flip()

# Exercícios --------------------------------------------------------------

# a. Transforme o gráfico do exercício anterior em um gráfico de barras.

imdb %>% 
  group_by(ano) %>% 
  summarise(orc_medio = mean(orcamento, na.rm = TRUE)) %>% 
  ggplot() +
  geom_col(aes(x = ano, y = orc_medio))

# Exemplo geom_bar
imdb %>% 
  ggplot() +
  geom_bar(aes(x = classificacao))

imdb %>% 
  count(classificacao) %>% 
  ggplot() +
  geom_col(aes(x = classificacao, y = n))

# b. Refaça o gráfico apenas para filmes de 1989 para cá.

imdb %>% 
  filter(ano >= 1989) %>% 
  group_by(ano) %>% 
  summarise(orc_medio = mean(orcamento, na.rm = TRUE)) %>% 
  ggplot() +
  geom_col(aes(x = ano, y = orc_medio))

# Histogramas e boxplots --------------------------------------------------

# Histograma do lucro dos filmes do Steven Spielberg 
imdb %>% 
  filter(diretor == "Steven Spielberg") %>%
  ggplot() +
  geom_histogram(aes(x = lucro))

# Arrumando o tamanho das bases
imdb %>% 
  filter(diretor == "Steven Spielberg") %>%
  ggplot() +
  geom_histogram(
    aes(x = lucro), 
    binwidth = 100000000,
    color = "black",
    fill = "red"
  )

# Boxplot do lucro dos filmes dos diretores
# fizeram mais de 15 filmes
imdb %>% 
  filter(!is.na(diretor)) %>%
  group_by(diretor) %>% 
  filter(n() >= 15) %>% 
  ggplot() +
  geom_boxplot(aes(x = diretor, y = lucro))

# Ordenando pela mediana

imdb %>% 
  filter(!is.na(diretor)) %>%
  group_by(diretor) %>% 
  filter(n() >= 15) %>% 
  ungroup() %>%
  mutate(
    diretor = forcats::fct_reorder(
      diretor, 
      lucro,
      .fun = median,
      na.rm = TRUE)
  ) %>% 
  ggplot() +
  geom_boxplot(aes(x = diretor, y = lucro))


# Exercícios --------------------------------------------------------------

#a. Descubra quais são os 5 atores que mais aparecem na coluna ator_1.

atores_que_mais_aparecem <- imdb %>% 
  count(ator_1) %>% 
  top_n(5, n) %>% 
  pull(ator_1)

tabela_atores_que_mais_aparecem <- imdb %>% 
  count(ator_1) %>% 
  top_n(5, n)


#b. Faça um boxplot do lucro dos filmes desses atores.

tabela_atores_que_mais_aparecem %>% 
  left_join(imdb, by = "ator_1") %>% 
  mutate(
    ator_1 = forcats::fct_reorder(ator_1, lucro, na.rm = TRUE)
  ) %>% 
  ggplot() +
  geom_boxplot(aes(x = ator_1, y = lucro))

imdb %>% 
  count(ator_1) %>% 
  top_n(5, n) %>% 
  left_join(imdb, by = "ator_1") %>% 
  mutate(
    ator_1 = forcats::fct_reorder(ator_1, lucro, na.rm = TRUE)
  ) %>% 
  ggplot() +
  geom_boxplot(aes(x = ator_1, y = lucro))


imdb %>% 
  filter(ator_1 %in% atores_que_mais_aparecem) %>%
  mutate(
    ator_1 = forcats::fct_reorder(ator_1, lucro, na.rm = TRUE)
  ) %>% 
  ggplot() +
  geom_boxplot(aes(x = ator_1, y = lucro))

# Título e labels ---------------------------------------------------------

# Labels
imdb %>%
  ggplot() +
  geom_point(
    aes(x = orcamento, y = receita, size = lucro)
  ) +
  labs(
    x = "Orçamento ($)",
    y = "Receita ($)",
    color = "Lucro ($)",
    title = "Gráfico de dispersão",
    subtitle = "Receita vs Orçamento"
  )

# Escalas
imdb %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = nota_media)) +
  scale_x_continuous(breaks = seq(1916, 2016, 10)) +
  # scale_x_continuous(breaks = 1916:2020) +
  scale_y_continuous(breaks = seq(0, 10, 2))

# Visão do gráfico
imdb %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = nota_media)) +
  scale_x_continuous(breaks = seq(1916, 2016, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  coord_cartesian(ylim = c(0, 10), xlim = c(1900, 2020))

# Cores -------------------------------------------------------------------

# Escolhendo cores pelo nome
imdb %>% 
  count(diretor) %>%
  filter(!is.na(diretor)) %>% 
  top_n(5, n) %>%
  ggplot() +
  geom_col(
    aes(x = diretor, y = n, fill = diretor), 
    stat = "identity",
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("#7ea6f8", "blue", "green", "pink", "purple")
  )

# Escolhendo pelo hexadecimal
imdb %>% 
  count(diretor) %>%
  filter(!is.na(diretor)) %>% 
  top_n(5, n) %>%
  ggplot() +
  geom_bar(
    aes(x = diretor, y = n, fill = diretor), 
    stat = "identity",
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("#ff4500", "#268b07", "#ff7400", "#0befff", "#a4bdba")
  )

# Mudando textos da legenda
imdb %>% 
  filter(!is.na(cor)) %>% 
  group_by(ano, cor) %>% 
  summarise(num_filmes = n()) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = num_filmes, color = cor)) +
  scale_color_discrete(labels = c("Preto e branco", "Colorido"))

scale_color_gradient(low = "white", high = "black")

# Definiando cores das formas geométricas
imdb %>% 
  ggplot() +
  geom_point(
    mapping = aes(x = orcamento, y = receita), 
    color = "#ff7400"
  )

# Tema --------------------------------------------------------------------

# Temas prontos
imdb %>% 
  ggplot() +
  geom_point(mapping = aes(x = orcamento, y = receita)) +
  theme_minimal()
  
  # theme_bw() 
  # theme_classic() 
  # theme_dark()
  # theme_minimal()

# A função theme()
imdb %>% 
  ggplot() +
  geom_point(mapping = aes(x = orcamento, y = receita)) +
  labs(
    title = "Gráfico de dispersão",
    subtitle = "Receita vs Orçamento"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      color = "purple", 
      face = "bold" 
    ),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "yellow")
  )
