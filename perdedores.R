#Importa as bibliotecas
library(cepespR)
library(data.tabe)
library(dplyr)
library(tidyverse)

#Vamos fazer um filtro para os campos que queremos extrair do Cepesp Data
filtro = c("NUM_TURNO", "DESCRICAO_ELEICAO", "DESCRICAO_CARGO", "SIGLA_UE", "ANO_ELEICAO", "CODIGO_SEXO", "DESCRICAO_SEXO", "CPF_CANDIDATO", "NOME_CANDIDATO", "SIGLA_PARTIDO", "DESC_SIT_TOT_TURNO")
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
pol <- rbind(mun, fed) %>% distinct(ANO_ELEICAO, CPF_CANDIDATO, .keep_all = T)


#Os eternos renegados
pol_rene <- pol %>% 
  filter(DESC_SIT_TOT_TURNO == "SUPLENTE" |
           DESC_SIT_TOT_TURNO == "NÃO ELEITO")

pol_cpf <- pol_rene %>% 
  group_by(CPF_CANDIDATO) %>%
  summarise(CANDIDATURA = unique(length(ANO_ELEICAO))) %>%
  filter(CANDIDATURA == 11)

pol_cpf$CPF_CANDIDATO <- as.character(pol_cpf$CPF_CANDIDATO)

#Candidatura 20
cand_20 <- fread("consulta_cand_2020_BRASIL.csv", encoding = "Latin-1", colClasses = "chr")

#Seleciona os dados únicos e colunas importantes
cand_20 <- cand_20 %>% select("SG_UF", "SG_UE", "CD_CARGO", "SG_PARTIDO", "NM_CANDIDATO", "NR_TITULO_ELEITORAL_CANDIDATO", "NR_CPF_CANDIDATO") %>% distinct()

reneg <- inner_join(cand_20, pol_cpf, by = c("NR_CPF_CANDIDATO" = "CPF_CANDIDATO"))  
