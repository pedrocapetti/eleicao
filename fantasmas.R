#Importa bibliotecas
library(data.table)
library(dplyr)
library(tidyverse)

#Importa arquivo de votação de 2016
tse_candidatura <- fread("votacao_candidato_munzona_2016_BRASIL.csv", encoding = "Latin-1", colClasses = "chr")

#Faz os filtros
tse <- tse_candidatura %>% 
  filter(NM_TIPO_ELEICAO == "Eleição Ordinária") %>%
  filter(CD_CARGO == "13") %>%
  filter(DS_DETALHE_SITUACAO_CAND == "DEFERIDO" | 
           DS_DETALHE_SITUACAO_CAND == "DEFERIDO COM RECURSO")

#Votação por candidato em 2016
votos_2016 <- tse %>% 
  group_by(SQ_CANDIDATO, SG_UE, SG_PARTIDO) %>% 
  summarise(QT_VOTO = sum(as.numeric(QT_VOTOS_NOMINAIS))) %>% 
  arrange(QT_VOTO)


#Importa o arquivo com dados necessários
cand_2016 <- fread("consulta_cand_2016_BRASIL.csv", encoding = "Latin-1", colClasses = "chr") %>% 
  select(SG_UE, SQ_CANDIDATO, NM_CANDIDATO, NR_CPF_CANDIDATO, NR_TITULO_ELEITORAL_CANDIDATO,  DS_GRAU_INSTRUCAO, DS_OCUPACAO, DS_GENERO, DS_GRAU_INSTRUCAO) %>% distinct()

#Merge votação x candidatura
tse_fant <- inner_join(votos_2016, cand_2016, 
                       by = c("SQ_CANDIDATO" = "SQ_CANDIDATO",
                              "SG_UE" = "SG_UE"))

#Filtra os candidatos com menos de 1 voto
tse_zero <- tse_fant %>% filter(QT_VOTO == 0)

#Importa a tabela de Candidatura de 2020
cand_20 <- fread("consulta_cand_2020_BRASIL.csv", encoding = "Latin-1", colClasses = "chr")

#Seleciona os dados únicos e colunas importantes
cand_20 <- cand_20 %>% select("SG_UF", "SG_UE", "CD_CARGO", "SG_PARTIDO", "NM_CANDIDATO", "NR_TITULO_ELEITORAL_CANDIDATO", "NR_CPF_CANDIDATO", "NM_URNA_CANDIDATO", "NM_UE") %>% distinct()

#Merge do Fantasma 2016 x 2020
fantasma_repetido <- inner_join(cand_20, tse_zero, by = "NR_TITULO_ELEITORAL_CANDIDATO")

fantasma_repetido %>% group_by(DS_GENERO) %>% summarise(CON = n())

#Incluir 2018 na parada
votos_18 <- fread("votacao_candidato_munzona_2018_BRASIL.csv", encoding = "Latin-1", colClasses = "chr")

votos_18 <- votos_18 %>% filter(NM_TIPO_ELEICAO == "Eleição Ordinária") %>%
  filter(DS_DETALHE_SITUACAO_CAND == "DEFERIDO" | 
           DS_DETALHE_SITUACAO_CAND == "DEFERIDO COM RECURSO")

vot_18 <- votos_18 %>% 
  group_by(SQ_CANDIDATO, SG_UE, SG_PARTIDO, DS_CARGO) %>% 
  summarise(QT_VOTO = sum(as.numeric(QT_VOTOS_NOMINAIS))) %>% 
  arrange(QT_VOTO)

#Importar a tabela de despesas
despesas_18 <- fread("receitas_candidatos_2018_BRASIL.csv", encoding = "Latin-1", colClasses = "chr")

despesas_18 <- despesas_18 %>% select(DS_CARGO, SQ_CANDIDATO, NM_CANDIDATO, NR_CPF_CANDIDATO, DS_ORIGEM_RECEITA, DS_NATUREZA_RECEITA, DS_ESPECIE_RECEITA, DS_RECEITA, VR_RECEITA)

despesas_18$VR_RECEITA <- as.numeric(gsub(",", ".", despesas_18$VR_RECEITA))

gasto_partido <- despesas_18 %>% 
  filter(DS_ORIGEM_RECEITA == "Recursos de partido político") %>%
  filter(DS_CARGO == "Deputado Federal" | 
           DS_CARGO == "Deputado Estadual" |
           DS_CARGO == "Deputado Distrital") %>%
  group_by(SQ_CANDIDATO, NR_CPF_CANDIDATO, NM_CANDIDATO) %>%
  summarise(GASTO = sum(VR_RECEITA))

#Filtrar a tabela da relação voto x gasto
cand_18 <- inner_join(vot_18, gasto_partido, by = "SQ_CANDIDATO") %>% distinct()
names(cand_18)
cand_18$REL <- cand_18$GASTO/cand_18$QT_VOTO

cand_18_filtro <- cand_18 %>% filter(GASTO > 30000 & QT_VOTO < 5000)

fant_20 <- inner_join(cand_20, cand_18_filtro, by = "NR_CPF_CANDIDATO")
