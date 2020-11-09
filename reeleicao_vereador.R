#Importa as bibliotecas
library(cepespR)
library(data.table)
library(dplyr)
library(tidyverse)

#Vamos fazer um filtro para os campos que queremos extrair do Cepesp Data
filtro = c("NUM_TURNO", "ANO_ELEICAO", "CPF_CANDIDATO", "DESC_SIT_TOT_TURNO", "SIGLA_PARTIDO", "SG_UE")
pleitos_mun = ("2016")

#Eleição Municipal para prefeito
mun_cam <- get_candidates(year = pleitos_mun, position = "Vereador", columns_list = filtro)

#Filtra apenas os eleitos em cada ano
mun_eleitos <- mun_cam %>% 
  filter(NUM_TURNO == 1) %>%
  filter(DESC_SIT_TOT_TURNO == "ELEITO"|
           DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA"|
           DESC_SIT_TOT_TURNO == "ELEITO POR QP") %>% 
  distinct(CPF_CANDIDATO, .keep_all = TRUE)

#Vamos obter quantas pessoas eleitas tentam se eleger novamente em 2016
mun_2018 <- mun_eleitos %>% 
  filter(ANO_ELEICAO == "2016")

#Importa a tabela deste ano
eleicao_2020 <- fread("consulta_cand_2020_BRASIL.csv", encoding = "Latin-1", sep = ";", colClasses = "chr") %>% 
  select("ANO_ELEICAO", "SG_UE", "NM_UE", "NR_CPF_CANDIDATO", "SG_PARTIDO", "CD_CARGO", "DS_CARGO") %>% 
  filter(CD_CARGO == "13") %>% distinct(NR_CPF_CANDIDATO, .keep_all = T)

cand_city <- eleicao_2020 %>% group_by(SG_UE, NM_UE) %>% summarise(CONCO = n())

#Junta para ver aqueles que estão tentando reeleição
reeleicao <- inner_join(eleicao_2020, mun_eleitos, by = c("NR_CPF_CANDIDATO" = "CPF_CANDIDATO"))
