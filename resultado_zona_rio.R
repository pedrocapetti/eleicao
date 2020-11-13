#Importa as bibliotecas
library(tidyverse)
library(jsonlite)
library(data.table)
library(dplyr)
library(httr)
library(magrittr)

site <- "https://infograficos-eleicoes-2020-dev.s3.amazonaws.com/1-turno/zonas/oglobo-prefeitos-rj-rio-de-janeiro.json"
get_budget_json <- fromJSON(site, flatten = TRUE)
zonas <- names(get_budget_json[["z"]])

#Resultado
resultado <- tribble(
  ~zona_eleitoral, ~sigla, ~qnt_voto)

for (zona in zonas) {
  ze <- zona
  partido <- get_budget_json[["z"]][[zona]][["c"]][["id"]]
  voto <- get_budget_json[["z"]][[zona]][["c"]][["v"]]
  resultado %<>% add_row(zona_eleitoral = as.numeric(ze),
                          sigla = as.character(partido),
                          qnt_voto = as.numeric(voto))
}

r <- resultado %>% group_by(zona_eleitoral) %>% 
  mutate(total = sum(qnt_voto),
         ranking = dense_rank(desc(qnt_voto))) %>%
  ungroup() %>%
  mutate(vv = round((qnt_voto/total*100),2)) %>%
  group_by(sigla) %>%
  mutate(media = round(mean(vv),2)) %>%
  ungroup()
