#Importa bibliotecas
library(cepespR)
library(data.table)
library(dplyr)
library(tidyverse)

#Importa
cand_20 <- fread("consulta_cand_2020_BRASIL.csv", encoding = "Latin-1", colClasses = "chr")

#Seleciona os dados únicos e colunas importantes
cand_20 <- cand_20 %>% select("SG_UF", "SG_UE", "CD_CARGO", "SG_PARTIDO", "NM_CANDIDATO", "NR_TITULO_ELEITORAL_CANDIDATO", "NR_CPF_CANDIDATO") %>% distinct()

filtro = c("NUM_TURNO", "ANO_ELEICAO", "CPF_CANDIDATO", "DESC_SIT_TOT_TURNO", "SIGLA_PARTIDO", "NM_PARTIDO")
pleitos_mun = ("2016")

#Eleição Municipal para os cargos e junta em uma única planilha
mun_pref <- get_candidates(year = pleitos_mun, position = "Prefeito", columns_list = filtro)
mun_cam <- get_candidates(year = pleitos_mun, position = "Vereador", columns_list = filtro)
mun <- rbind(mun_pref, mun_cam)

#Juntando as tabelas de políticos (mun + fed), lembrando com o filtro do primeiro turno e pegando apenas aqueles que estiveram cadastrados na urna nos últimos anos
pol <- mun %>% filter(NUM_TURNO == 1) %>% distinct(CPF_CANDIDATO, .keep_all = TRUE)
colnames(pol)[2] <- "ANO_ELEICAO_PASSADA"

#MUDA
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PMDB"] <- "MDB"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PEN"] <- "PATRIOTA"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PSDC"] <- "DC"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PRP"] <- "PATRIOTA"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PRB"] <- "REPUBLICANOS"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PPS"] <- "CIDADANIA"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PHS"] <- "PODE"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PT do B"] <- "AVANTE"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PTN"] <- "PODE"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PPL"] <- "PC do B"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "SD"] <- "SOLIDARIEDADE"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PR"] <- "PL"

#Troca nome da coluna
colnames(pol)[5] <- "SIGLA_ANTIGA"

#Junta partidaria
muda_partido <- inner_join(cand_20, pol, by = c("NR_CPF_CANDIDATO" = "CPF_CANDIDATO"))

#Filtra aqueles partidos diferentes
sigla <- muda_partido %>% filter(SIGLA_ANTIGA != SG_PARTIDO)

#Fluxo do partido
fluxo <- sigla %>% group_by(SIGLA_ANTIGA, SG_PARTIDO) %>% summarise(SOMA = n())

#Saída e Entrada
saldo_saida <- sigla %>% group_by(SIGLA_ANTIGA) %>% summarise(SOMA = n()) %>% arrange(desc(SOMA))
saldo_entrada <- sigla %>% group_by(SG_PARTIDO) %>% summarise(SOMA = n()) %>% arrange(desc(SOMA))
