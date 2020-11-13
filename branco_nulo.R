pleitos_mun = ("2016, 2012, 2008, 2004, 2000")
filtro <- c("ANO_ELEICAO", "NUM_TURNO", "COD_MUN_IBGE", "QTD_COMPARECIMENTO", "QT_VOTOS_NULOS", "QT_VOTOS_BRANCOS")

votos <- get_elections(year = pleitos_mun, position = "Prefeito", regional_aggregation="Municipality", political_aggregation = "Consolidado", columns_list = filtro)

prima <- votos %>% filter(NUM_TURNO == 1) %>%
  mutate(QT_BRNUL = QT_VOTOS_NULOS + QT_VOTOS_BRANCOS,
         PERCENT = round((QT_BRNUL/QTD_COMPARECIMENTO*100),2))

branco_nulo <- prima %>% group_by(ANO_ELEICAO) %>% 
  summarise(T_ELEITOR = sum(QTD_COMPARECIMENTO),
            T_BRNUL = sum(QT_BRNUL)) %>%
  mutate(ANO = round((T_BRNUL/T_ELEITOR*100),2))

write.csv(branco_nulo, "branco_nulo_1t.csv", row.names = FALSE)


branco <- votos %>% select(ANO_ELEICAO, COD_MUN_IBGE, PERCENT)

municipios <- fread("POP_2020.csv", encoding = "UTF-8")

branco <- inner_join(branco, municipios, by = c("COD_MUN_IBGE" = "COD_IBGE"))

branco <- branco %>% mutate(tamanho_mun = case_when(POP < 20000 ~ "ate 20k", 
                                 POP > 20000 & POP <= 50000 ~ "20-50k",
                                 POP > 50000 & POP <= 100000 ~ "50-100k",
                                 POP > 100000 & POP <= 200000 ~ "100-200k",
                                 POP > 200000 & POP <= 500000 ~ "200-500k",
                                 POP > 500000 ~ "+500k"))


media_ano <- branco %>% group_by(ANO_ELEICAO, tamanho_mun) %>% summarise(media = mean(PERCENT)) %>% filter(tamanho_mun != "")

write.csv(media_ano, "media_branconulo_porcidade.csv", row.names = FALSE)

ggplot(media_ano, aes(ANO_ELEICAO, media)) + geom_point() +
  geom_line() + theme_gray() + ylim(0, 16) +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016)) +
  facet_wrap(vars(tamanho_mun), ncol = 2)
  

