library(tidyverse)
library(httr)
library(readxl)

# Importar o arquivo para uma pasta de projeto do RStudio.
infos <- GET(
  "https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeral",
  add_headers("X-Parse-Application-Id" = "unAFkcaNDeXajurGB7LChj8SgQYS2ptm")
) %>% content()

infos_results <- infos_results[[1]]

# Download do xlsx
download.file(infos_results$arquivo$url, "dados/covid.xlsx")

# Leitura do xlsx
covid <- read_excel("dados/covid.xlsx", guess_max = 10000)

# arrumando as colunas
covid <- covid %>%
  mutate(
    data = as.Date(data)
  ) %>%
  mutate_at(vars(populacaoTCU2019:emAcompanhamentoNovos), as.numeric)

# Filtrar só as capitais do Brasil (eliminar todas as outras cidades, já tenho a lista das capitais)
# Transoformar zero (0) mortes ou casos em 0.5 (para não ter problema na transformação logarítmica), isso depois da transformação no log base 2 vai virar -1;
# Criar uma coluna (vetor) com a área em Km2 de cada capital (já tenho uma lista com a área em Km2 de cada capital);
# Criar uma coluna com a população de cada capital (já tenho uma lista com a população)
# Criar uma coluna com a densidade populacional de cada capital população / área em Km2;
# Criar uma coluna com log na base 2 da coluna de óbitosacumulados (obitosAcumulado;  já tem no arquivo original)
# Fazer um gráfico onde contém a data a partir da primeira morte no Brasil, no eixo x e no eixo Y o log2 do número de mortes acumulada; 


