#Importa bibliotecas
library(data.table)
library(dplyr)
library(tidyverse)

#Importa os filiados
patri <- fread("bem_candidato_2020_BRASIL.csv", encoding = "Latin-1", sep = ";")

patri$VR_BEM_CANDIDATO <- gsub(",", ".", patri$VR_BEM_CANDIDATO)
patri$VR_BEM_CANDIDATO <- as.numeric(patri$VR_BEM_CANDIDATO)
patri$DT_ULTIMA_ATUALIZACAO <-as.Date(patri$DT_ULTIMA_ATUALIZACAO, "%d/%m/%Y")

especie <- patri %>% filter(DS_TIPO_BEM_CANDIDATO == "Dinheiro em espécie - moeda nacional")

especie_data <- especie %>% group_by(SQ_CANDIDATO, SG_UE, NM_UE, DT_ULTIMA_ATUALIZACAO, DS_BEM_CANDIDATO) %>% summarise(SOMA = sum(VR_BEM_CANDIDATO)) %>% slice((which.max(DT_ULTIMA_ATUALIZACAO)))

cand_20 <- fread("consulta_cand_2020_BRASIL.csv", encoding = "Latin-1")

#Seleciona os dados únicos para Prefeitura
cand_20 <- cand_20 %>% select("SG_UF", "SG_UE", "CD_CARGO", "SG_PARTIDO", "NM_CANDIDATO", "NR_TITULO_ELEITORAL_CANDIDATO", "NR_CPF_CANDIDATO", "SQ_CANDIDATO") %>% distinct()

especie_data <- inner_join(especie_data, cand_20, by = "SQ_CANDIDATO")
