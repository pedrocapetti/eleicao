#Importa as bibliotecas
library(cepespR)
library(data.table)
library(dplyr)
library(tidyverse)

#Vamos fazer um filtro para os campos que queremos extrair do Cepesp Data
filtro = c("NUM_TURNO", "ANO_ELEICAO", "DESCRICAO_ELEICAO", "CPF_CANDIDATO", "DESC_SIT_TOT_TURNO", "SIGLA_PARTIDO", "SIGLA_UE")
pleitos_mun = ("2016, 2012, 2008, 2004, 2000")

#Eleição Municipal para prefeito
pol <- get_candidates(year = pleitos_mun, position = "Prefeito", columns_list = filtro, only_elected = T)

unique(pol$DESCRICAO_ELEICAO)

pol <- pol %>% filter(DESCRICAO_ELEICAO == "ELEICOES 2000" |
                        DESCRICAO_ELEICAO == "ELEICOES 2004" |
                        DESCRICAO_ELEICAO == "Eleições 2008" | 
                        DESCRICAO_ELEICAO == "ELEIÇÃO MUNICIPAL 2012" |
                        DESCRICAO_ELEICAO == "Eleições Municipais 2016") %>%
  filter(NUM_TURNO == "1")

pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PMDB"] <- "MDB"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PFL"] <- "DEM"
pol$SIGLA_PARTIDO[pol$SIGLA_PARTIDO == "PRONA"] <- "PL"
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

pol$SIGLA_UE <- as.numeric(pol$SIGLA_UE)

cidade <- fread("https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/master/municipios_brasileiros_tse.csv", encoding = "UTF-8") %>% select(codigo_tse, codigo_ibge, nome_municipio, uf)

pop <- fread("https://docs.google.com/spreadsheets/d/e/2PACX-1vS_Qp00npFLAUflGiiFhrV1I5bAIUQec4ozOiD0ET7pA9lJ9E8RVebN7gHjJvvgOnYST6K33eW_qUlI/pub?gid=703577243&single=true&output=csv", encoding = "UTF-8")

colnames(pop)[4] <- "2000"

a <- gather(pop, ano, mun, "2000", "2004", "2008", "2012", "2016", "2020")
a$ano <- as.numeric(a$ano)

resumo <- inner_join(pol, cidade, by = c("SIGLA_UE" = "codigo_tse"))

resumo_city <- inner_join(resumo, a, by = c("ANO_ELEICAO" = "ano",
                                              "codigo_ibge" = "cod"))

pop_comando <- resumo_city %>% group_by(SIGLA_PARTIDO, ANO_ELEICAO) %>% summarise(SOMA = sum(mun, na.rm = T)) %>% spread(ANO_ELEICAO, SOMA)


pop_comando <- resumo_city %>% group_by(SIGLA_PARTIDO, ANO_ELEICAO) %>% summarise(SOMA = n()) %>% spread(ANO_ELEICAO, SOMA)

write.csv(pop_comando, "pref_comandada.csv", row.names = FALSE)