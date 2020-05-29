bdzinho <- tribble(
  ~fruta, ~cor,
  "Mação", "Vermelha",
  "Mação", "Vermelha",
  "Mação", "Verde",
)

imdb %>% distinct(starts_with("ator"))

bdzinho %>% 
  sample_frac(1) %>%
  distinct()




%>% distinct(fruta)
bdzinho %>% distinct(fruta, .keep_all = TRUE)


bdzinho %>% distinct(cor)
bdzinho %>% distinct(cor, .keep_all = TRUE)

