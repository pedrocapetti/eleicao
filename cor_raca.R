#Importa as bibliotecas
library(cepespR)
library(data.tabe)
library(dplyr)
library(tidyverse)

#Vamos fazer um filtro para os campos que queremos extrair do Cepesp Data
#É necessário separar eleição municipal e estadual/federal para solicitação na API
filtro = c("NUM_TURNO", "ANO_ELEICAO", "DESCRICAO_CARGO", "CPF_CANDIDATO", "DESC_SIT_TOT_TURNO", "SIGLA_PARTIDO", "CODIGO_COR_RACA", "DESCRICAO_COR_RACA")
pleitos_mun = ("2000, 2004, 2008, 2012, 2016")
pleitos_fed = ("1998, 2002, 2006, 2010, 2014, 2018")

#Eleição Municipal para os cargos e junta em uma única planilha
mun_pref <- get_candidates(year = pleitos_mun, position = "Prefeito", columns_list = filtro)
mun_cam <- get_candidates(year = pleitos_mun, position = "Vereador", columns_list = filtro)
mun <- rbind(mun_pref, mun_cam)

#Eleição Federal para os cargos e junta em uma única planilha
fed_gov <- get_candidates(year = pleitos_fed, position = "Governador", columns_list = filtro)
fed_depe <- get_candidates(year = pleitos_fed, position = "Deputado Estadual", columns_list = filtro)
fed_depf <- get_candidates(year = pleitos_fed, position = "Deputado Federal", columns_list = filtro)
fed_sen <- get_candidates(year = pleitos_fed, position = "Senador", columns_list = filtro)
fed_pres <- get_candidates(year = pleitos_fed, position = "Presidente", columns_list = filtro)
fed <- rbind(fed_gov, fed_depe, fed_depf, fed_sen, fed_pres)

#Juntando as tabelas de políticos (mun + fed), lembrando com o filtro do primeiro turno e pegando apenas aqueles que estiveram cadastrados na urna nos últimos anos
pol <- rbind(mun, fed) %>% filter(NUM_TURNO == 1 & DESC_SIT_TOT_TURNO != "#NULO#")

#Importa lista de candidatos de 2020
cand_20 <- fread("consulta_cand_2020_BRASIL.csv", encoding = "Latin-1", colClasses = "chr") %>% select(SG_PARTIDO, NR_CPF_CANDIDATO, NM_CANDIDATO, CD_COR_RACA, DS_COR_RACA)

#Importa lista de candidatos
pol_candidato <- inner_join(cand_20, pol, by = c("NR_CPF_CANDIDATO" = "CPF_CANDIDATO")) %>% distinct()

#Filtra aqueles que nunca declararam Raça
pol_candidato <- pol_candidato %>% filter(CODIGO_COR_RACA != "-1")

#Pega aqueles que estão diferentes
pol_cand_t <- pol_candidato %>% filter(CODIGO_COR_RACA != CD_COR_RACA)

#Pega só aqueles que mudaram em 2020
pol_mud <- pol_cand_t %>% select(NR_CPF_CANDIDATO, ANO_ELEICAO, NM_CANDIDATO, SG_PARTIDO, DS_COR_RACA, DESCRICAO_COR_RACA) %>% distinct()

#Descobrimos que tem uma mulher que se declarou com duas cores distintas, vamos colocar pardo por ser o que ela declarou por duas vezes, na 68502
pol_mud <- pol_mud[-68544, ]

t <- spread(pol_mud, ANO_ELEICAO, DESCRICAO_COR_RACA)

t <- t %>% filter(DS_COR_RACA != "SEM INFORMAÇÃO")

triplice <- t %>% filter(`2016` == "BRANCA" & `2018` == "BRANCA")
