#Importa as bibliotecas
library(tidyverse)
library(jsonlite)
library(data.table)
library(dplyr)
library(httr)
library(magrittr)

#Importa a API e também os municípios
site <- "http://infograficos-eleicoes-2020-dev.s3.amazonaws.com/PREFEITO_BRASIL.JSON"
get_budget_json <- fromJSON(site, flatten = TRUE)
municipios <- fread("https://d37iydjzbdkvr9.cloudfront.net/arquivos/2020/10/25/municipios_ibge.csv", sep = ";", encoding = "UTF-8")
ibge <- names(get_budget_json[["municipios"]])

#Lista de Abstenções
dados <- data.frame()

for (cod in ibge) {
  cod_ibge <- cod
  e <- get_budget_json[["municipios"]][[cod]][["t"]]
  a <- get_budget_json[["municipios"]][[cod]][["a"]]
  tabela <- as.data.frame(cod_ibge) %>%
    mutate(votos = e,
           abst = a)
  dados <- merge.data.frame(dados, tabela, all = TRUE)
}
  
dados <- dados %>% mutate(votos = as.integer(gsub("\\.", "", votos)),
                          abst = as.integer(gsub("\\.", "", abst)),
                          qnt_eleitores = (votos + abst),
                          perc_abst = round((abst/qnt_eleitores)*100, 2))

#Lista dos Vencedores
vencedores <- tribble(
  ~nome, ~partido, ~votos, ~sit, ~seg_t, ~cod_ibge)

for (cod in ibge) {
  n <- get_budget_json[["municipios"]][[cod]][["c"]][["nc"]]
  p <- get_budget_json[["municipios"]][[cod]][["c"]][["sp"]]
  v <- get_budget_json[["municipios"]][[cod]][["c"]][["tp"]]
  s <- get_budget_json[["municipios"]][[cod]][["c"]][["e"]]
  st <- get_budget_json[["municipios"]][[cod]][["c"]][["st"]]
  ci <- cod
    vencedores %<>% add_row(nome = n,
                            partido = p,
                            votos = v,
                            sit = s,
                            seg_t = st,
                            cod_ibge = ci
    )
}

vencedores <- vencedores %>% mutate(sigla = substr(partido, 2, 3))
