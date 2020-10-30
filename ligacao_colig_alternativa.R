library(data.table)
library(dplyr)
library(tidyverse)
library(stringr)

#Importa a planilha de candidatos
tse_candidatura <- fread("consulta_cand_2020_BRASIL.csv", encoding = "Latin-1")

#Seleciona os dados únicos para Prefeitura
tse_candidatura <- tse_candidatura %>% select("SG_UF", "NM_UE", "NR_TURNO","SG_UE", "CD_CARGO", "SG_PARTIDO", "SQ_COLIGACAO", "DS_SITUACAO_CANDIDATO_PLEITO", "DS_COMPOSICAO_COLIGACAO", "TP_AGREMIACAO") %>%
  filter(CD_CARGO == 11) %>% filter(NR_TURNO == 1) %>% distinct()

tse_teste <- tse_candidatura %>% tidyr::separate_rows(DS_COMPOSICAO_COLIGACAO, sep = "/")
tse_teste$DS_COMPOSICAO_COLIGACAO <- stringr::str_trim(tse_teste$DS_COMPOSICAO_COLIGACAO)

#Número de coligações
teste <- tse_teste %>% 
  group_by(SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, TP_AGREMIACAO) %>% 
  summarise(CONT = n())


pr <- tse_candidatura %>% group_by(SG_PARTIDO, TP_AGREMIACAO) %>% summarise(CONT = n())


teste_fiel <- tse_teste %>% filter(SG_PARTIDO == "PT") %>% filter(DS_COMPOSICAO_COLIGACAO == "PSL")

pr <- spread(pr, TP_AGREMIACAO, CONT)
pr$COLIGAÇÃO <- replace_na(pr$COLIGAÇÃO, 0)
pr <- pr %>% mutate(TOTAL = COLIGAÇÃO + `PARTIDO ISOLADO`,
                    TX_ISO = `PARTIDO ISOLADO`/TOTAL)

write.csv(teste, "partido_coligacao_todos.csv", row.names = FALSE)

#Tamanho de coligações
tamanho_colig <- tse_teste %>% group_by(SQ_COLIGACAO) %>% summarise(CONT = n()) %>% arrange(desc(CONT))

#2016
tse_2016 <- fread("consulta_cand_2016_BRASIL.csv", encoding = "Latin-1", colClasses = "chr")

#Seleciona os dados únicos para Prefeitura
tse_2016 <- tse_2016 %>% select("SG_UF", "NM_UE", "NR_TURNO","SG_UE", "NM_TIPO_ELEICAO", "CD_CARGO", "SG_PARTIDO", "SQ_COLIGACAO", "DS_SITUACAO_CANDIDATO_PLEITO", "DS_COMPOSICAO_COLIGACAO", "TP_AGREMIACAO", "NR_CPF_CANDIDATO", "DS_SIT_TOT_TURNO") %>%
  filter(CD_CARGO == 11) %>% filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  filter(NM_TIPO_ELEICAO == "ELEIÇÃO ORDINÁRIA") %>% distinct(NR_CPF_CANDIDATO, .keep_all = T)

t <- tse_2016 %>% filter(TP_AGREMIACAO == "PARTIDO ISOLADO") %>% distinct(SG_UE, .keep_all = T)

eleitorado <- fread("perfil_eleitorado_2020.csv", encoding = "Latin-1", colClasses = "chr")
eleitorado <- eleitorado %>% group_by(NM_MUNICIPIO, CD_MUNICIPIO) %>% summarise(TOTAL = sum(as.numeric(QT_ELEITORES_PERFIL)))

t <- left_join(t, eleitorado, by = c("SG_UE" = "CD_MUNICIPIO")) %>% arrange(TOTAL)


pr_2016 <- tse_2016 %>% 
  group_by(SG_PARTIDO, TP_AGREMIACAO) %>% 
  summarise(CONT = n())

pr_2016 <- spread(pr_2016, TP_AGREMIACAO, CONT)
pr_2016$COLIGAÇÃO <- replace_na(pr_2016$COLIGAÇÃO, 0)
pr_2016 <- pr_2016 %>% mutate(TOTAL = COLIGAÇÃO + `PARTIDO ISOLADO`,
                    TX_ISO = `PARTIDO ISOLADO`/TOTAL)

write.csv(pr_2016, "partido_isolado_2016.csv", row.names = FALSE)
