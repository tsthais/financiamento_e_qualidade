### FINANCIAMENTO E QUALIDADE DA EDUCAÇÃO NO BRASIL - IDEB ###
# Bibliotecas utilizadas

require(dplyr);require(ggplot2);require(moments)
require(readxl);require(tidyr);require(RColorBrewer)
require(gridExtra);require(kableExtra);require(stringr)
require(scales);require(nortest);require(lmtest);
require(geobr)

# 1. Limpeza do dados do Ideb

# Diretório de trabalho.
setwd("D:/2020_2/TCC1/Bases/Ideb")

# Leitura de dados.
anos_iniciais <- read_xlsx("divulgacao_anos_iniciais_municipios_2019.xlsx", skip = 9)

# Selecionando variáveis que foram usadas no estudo:
# Uf, município (código e nome), tipo de rede de ensino e os valores do 
# Ideb para os anos de 2005 a 2019.
# Além disso com a função foram retirados os valores faltantes.
anos_iniciais <- anos_iniciais %>% 
  select(SG_UF, CO_MUNICIPIO, NO_MUNICIPIO, REDE, VL_OBSERVADO_2005,VL_OBSERVADO_2007,      
         VL_OBSERVADO_2009, VL_OBSERVADO_2011, VL_OBSERVADO_2013, VL_OBSERVADO_2015, 
         VL_OBSERVADO_2017,VL_OBSERVADO_2019) %>% 
  filter(!is.na(NO_MUNICIPIO))

# Trocando os nomes das colunas que correspondem aos valores do Ideb.
names(anos_iniciais)[c(5:12)] <- c("2005", "2007", "2009", "2011", 
                                   "2013", "2015", "2017", "2019")

# Transformando o banco de dados.
# Os dados foram resumidos para que tivesse apenas uma coluna com os valores 
# do Ideb e outra com o Ano correspondente do valor do indicador.
# Novas variáveis: ano e valor_observado.

anos_iniciais <- anos_iniciais %>% 
  gather(ano, valor_observado, -SG_UF, -CO_MUNICIPIO, -NO_MUNICIPIO, -REDE) %>% 
  mutate(ano = as.numeric(ano), 
         valor_observado = ifelse(valor_observado == '-', NA, valor_observado), 
         valor_observado = as.numeric(valor_observado))

# Tranformação dos dados.
# Foi escolhido somente a rede municipal de escolas já que a unidade de 
# análise é o ensino fundamental;
# Foi criada uma nova coluna com as grandes regiões brasileiras;
# E criada uma nova coluna indicando a etapa de ensino.

anos_iniciais_mun <- anos_iniciais %>% filter(REDE == "Municipal")%>% 
  rename(Uf = SG_UF) %>% 
  mutate(regiao = ifelse(Uf == "RR"| Uf == 'AM'| Uf == "AP"| Uf == "PA"| Uf == "AC"| Uf == 'RO'| Uf == 'TO', "Norte",
                         ifelse(Uf == 'MT'| Uf =='DF'| Uf =='GO'| Uf =='MS', 'Centro- Oeste',
                                ifelse(Uf == 'MA'| Uf =='CE'| Uf =='RN'| Uf =='PB'| Uf == 'PI'| Uf =='PE'| Uf =='AL'| Uf =='SE'| Uf =='BA', 'Nordeste',
                                       ifelse(Uf == 'MG'| Uf =='ES'| Uf =='RJ'| Uf =='SP', 'Sudeste',
                                              ifelse(Uf == 'PR'| Uf =='SC'| Uf =='RS', 'Sul', 'X'))))), 
         etapa = "Anos Iniciais")

# Os dados em que se começou o trabalho foram removidos por não ter
# mais utilidade.
rm(anos_iniciais)

# 2. Análise de dados do Ideb

# Medidas descritivas do indicador no País
medidas <- anos_iniciais_mun %>% group_by(ano) %>% 
  summarise( min = min(valor_observado, na.rm = T), 
             prim_quar = quantile(valor_observado, probs = 0.25, na.rm = T), 
             mediana = median(valor_observado, na.rm = T), 
             media = mean(valor_observado, na.rm = T), 
             ter_qaur = quantile(valor_observado, probs = 0.75, na.rm = T), 
             max = max(valor_observado, na.rm = T), 
             var = var(valor_observado, na.rm = T),
             ampl = diff(range(valor_observado, na.rm = T)), 
             sd = sd(valor_observado, na.rm = T),
             curt = kurtosis(valor_observado, na.rm = T)) %>% 
  mutate(cv = (sd/media)*100)

# Gráfico de Boxplot do indicador no País
ggplot(anos_iniciais_mun, aes(x = as.factor(ano), y = valor_observado))+
  geom_boxplot(fill = "#4DAF4A",color="black", alpha = .6)+
  stat_summary(fun.y="mean", shape = 15, size = .3)+
  labs(x = "", y = "Valor do Ideb")+
  theme(legend.position = "bottom", 
        plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  theme_light()

# Medidas descritivas do indicador nas grandes regiões brasileiras
medidas.reg <- anos_iniciais_mun %>% group_by(regiao, ano) %>% 
  summarise( min = min(valor_observado, na.rm = T), 
             prim_quar = quantile(valor_observado, probs = 0.25, na.rm = T), 
             mediana = median(valor_observado, na.rm = T), 
             media = mean(valor_observado, na.rm = T), 
             ter_qaur = quantile(valor_observado, probs = 0.75, na.rm = T), 
             max = max(valor_observado, na.rm = T), 
             var = var(valor_observado, na.rm = T),
             ampl = diff(range(valor_observado, na.rm = T)), 
             sd = sd(valor_observado, na.rm = T),
             curt = kurtosis(valor_observado, na.rm = T)) %>% 
  mutate(cv = (sd/media)*100)

# Gráfico de médias do indicador nas grandes regiões brasileiras 
medidas_br <- medidas %>% select(ano, media) %>% mutate(regiao = "Brasil")
medidas_regioes <- medidas.reg %>% select(regiao,ano, media) %>% full_join(medidas_br)
ggplot(medidas_regioes, aes(x = ano, y = media, color = regiao))+
  geom_point(size = 2) + 
  geom_line(size = 1, alpha = .8, show.legend = F) +
  geom_label(aes(label = format(round(media, 2),decimal.mark = ","), fill = regiao), color = "white",
             label.padding = unit(0.2, "lines"), # Rectangle size around label
             label.size = 0.1,fontface = "bold", alpha = .8, show.legend = F)+
  scale_x_continuous(breaks = seq(2005,2019,by = 2))+
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                                "#FF7F00", "#F781BF"))+
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                               "#FF7F00", "#F781BF"))+
  labs(x = "", y = "Média do Ideb", color = "Região: ")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))

# Medidas descritivas do indicador nos estados brasileiros
medidas.uf <- anos_iniciais_mun %>% group_by(Uf, regiao,ano) %>% 
  summarise( min = min(valor_observado, na.rm = T), 
             prim_quar = quantile(valor_observado, probs = 0.25, na.rm = T), 
             mediana = median(valor_observado, na.rm = T), 
             media = mean(valor_observado, na.rm = T), 
             ter_qaur = quantile(valor_observado, probs = 0.75, na.rm = T), 
             max = max(valor_observado, na.rm = T), 
             var = var(valor_observado, na.rm = T),
             ampl = diff(range(valor_observado, na.rm = T)), 
             sd = sd(valor_observado, na.rm = T),
             curt = kurtosis(valor_observado, na.rm = T)) %>% 
  mutate(cv = (sd/media)*100)

centro_oeste <- medidas.uf %>% filter(regiao == "Centro- Oeste")

# Gráfico de boxplot do indicador para os estados de cada região brasileira
brasil <- anos_iniciais_mun %>% mutate(regiao = "Brasil", Uf = "BRASIL") %>% 
  filter(ano == 2005 | ano == 2013| ano == 2019) 

anos_iniciais_mun %>% 
  filter(ano == 2005 | ano == 2013| ano == 2019) %>% 
  filter(regiao == "Centro- Oeste") %>% 
  full_join(brasil) %>% 
  ggplot(aes(x = Uf, y = valor_observado, fill = Uf))+
  geom_boxplot(color="black", alpha = .6, position=position_dodge(.9))+
  stat_summary(fun.y="mean", shape = 15, size = .3, aes(group = Uf), position=position_dodge(.9))+
  facet_wrap(~ano)+
  scale_fill_brewer(palette="Set1")+
  labs(x = "", y = "Valor do Ideb", fill = "Estados: ")+
  theme_light()+
  theme(legend.position = "bottom",  
        plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))


# Região Nordeste

nordeste <- medidas.uf %>% filter(regiao == "Nordeste")

# Gráfico de Boxplot dos estados da região Nordeste em 2005 e 2019

minhas_cores <- c(brewer.pal(8,"Set1"), 
                  brewer.pal(8,"Set2")) # Cores padronizadas para os gráficos

anos_iniciais_mun %>% 
  filter(ano == 2005 | ano == 2013| ano == 2019) %>% 
  filter(regiao == "Nordeste") %>% 
  full_join(brasil) %>%
  mutate(nome = factor(Uf, levels = c("BRASIL", "AL", "BA", "CE", "MA", "PB", "PE", "PI", 
                                      "RN", "SE"))) %>% 
  ggplot(aes(x = nome, y = valor_observado, fill = nome))+
  geom_boxplot(color="black", alpha = .6, position=position_dodge(.9))+
  stat_summary(fun.y="mean", shape = 15, size = .3, aes(group = Uf), position=position_dodge(.9))+
  facet_wrap(~ano)+
  scale_fill_manual(values = minhas_cores)+
  labs(x = "", y = "Valor do Ideb", fill = "Estados: ")+
  theme_light()+
  theme(legend.position = "bottom",  
        plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))



# Região Norte

norte <- medidas.uf %>% filter(regiao == "Norte")

# Gráfico de Boxplot dos estados da região Norte em 2005 e 2019

anos_iniciais_mun %>% 
  filter(ano == 2005 | ano == 2013| ano == 2019) %>% 
  filter(regiao == "Norte") %>% 
  full_join(brasil) %>%
  mutate(nome = factor(Uf, levels = c("BRASIL", "AC", "AM", "AP", "PA", "RO", "RR", "TO"))) %>% 
  ggplot(aes(x = nome, y = valor_observado, fill = nome))+
  geom_boxplot(color="black", alpha = .6, position=position_dodge(.9))+
  stat_summary(fun.y="mean", shape = 15, size = .3, aes(group = Uf), position=position_dodge(.9))+
  facet_wrap(~ano)+
  scale_fill_manual(values = minhas_cores)+
  labs(x = "", y = "Valor do Ideb", fill = "Estados: ")+
  theme_light()+
  theme(legend.position = "bottom",  
        plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))


# Região Sudeste

sudeste <- medidas.uf %>% filter(regiao == "Sudeste")

# Gráfico de Boxplot dos estados da região Sudeste em 2005 e 2019

anos_iniciais_mun %>% 
  filter(ano == 2005 | ano == 2013| ano == 2019) %>% 
  filter(regiao == "Sudeste") %>% 
  full_join(brasil) %>%
  ggplot(aes(x = Uf, y = valor_observado, fill = Uf))+
  geom_boxplot(color="black", alpha = .6, position=position_dodge(.9))+
  stat_summary(fun.y="mean", shape = 15, size = .3, aes(group = Uf), position=position_dodge(.9))+
  facet_wrap(~ano)+
  scale_fill_manual(values = minhas_cores)+
  labs(x = "", y = "Valor do Ideb", fill = "Estados: ")+
  theme_light()+
  theme(legend.position = "bottom",  
        plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))



# Região Sul

sul <- medidas.uf %>% filter(regiao == "Sul")

# Gráfico de Boxplot dos estados da região Sul em 2005 e 2019

anos_iniciais_mun %>% 
  filter(ano == 2005 | ano == 2013| ano == 2019) %>% 
  filter(regiao == "Sul") %>% 
  full_join(brasil) %>%
  ggplot(aes(x = Uf, y = valor_observado, fill = Uf))+
  geom_boxplot(color="black", alpha = .6, position=position_dodge(.9))+
  stat_summary(fun.y="mean", shape = 15, size = .3, aes(group = Uf), position=position_dodge(.9))+
  facet_wrap(~ano)+
  scale_fill_manual(values = minhas_cores)+
  labs(x = "", y = "Valor do Ideb", fill = "Estados: ")+
  theme_light()+
  theme(legend.position = "bottom",  
        plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))

# 3. Análise de ranking do indicador

# Ranking 2005
ranking2005 <- anos_iniciais_mun %>% filter(ano == 2005) %>% 
  filter(!is.na(valor_observado)) %>% 
  arrange(-valor_observado) %>% 
  mutate(n = 1:n())

# Separação das primeiras e últimas posições de 2005
primeiras2005 <- ranking2005[c(1:10),]
primeiras2005 <- primeiras2005 %>% mutate(n1 = 1:n()) %>% 
  select(n1, n, regiao, Uf, NO_MUNICIPIO, valor_observado, ano)
ultimas2005 <- ranking2005[c((length((ai_2005$valor_observado))-9):length(ai_2005$valor_observado)),]
ultimas2005 <- ultimas2005 %>% arrange(valor_observado) %>% mutate(n1 = 1:n()) %>% 
  select(n1, n, regiao, Uf, NO_MUNICIPIO, valor_observado, ano)%>% arrange(-n)

# Ranking 2013
ranking2013 <- anos_iniciais_mun %>% filter(ano == 2013) %>% 
  filter(!is.na(valor_observado)) %>% 
  arrange(-valor_observado) %>% 
  mutate(n = 1:n())

# Separação das primeiras e últimas posições de 2013
primeiras2013 <- ranking2013[c(1:10),]
primeiras2013 <- primeiras2013 %>% mutate(n1 = 1:n()) %>% 
  select(n1, n, regiao, Uf, NO_MUNICIPIO, valor_observado, ano)
ultimas2013 <- ranking2013[c((length((ai_2013$valor_observado))-9):length(ai_2013$valor_observado)),]
ultimas2013 <- ultimas2013 %>% arrange(valor_observado) %>% mutate(n1 = 1:n()) %>% 
  select(n1, n, regiao, Uf, NO_MUNICIPIO, valor_observado, ano) %>% arrange(-n)

# Ranking 2019
ranking2019 <- anos_iniciais_mun %>% filter(ano == 2019) %>% 
  filter(!is.na(valor_observado)) %>% 
  arrange(-valor_observado) %>% 
  mutate(n = 1:n())

# Separação das primeiras e últimas posições de 2019
primeiras2019 <- ranking2019[c(1:10),]
primeiras2019 <- primeiras2019 %>% mutate(n1 = 1:n()) %>% 
  select(n1, n, regiao, Uf, NO_MUNICIPIO, valor_observado, ano)
ultimas2019 <- ranking2019[c((length((ai_2019$valor_observado))-9):length(ai_2019$valor_observado)),]
ultimas2019 <- ultimas2019 %>% arrange(valor_observado) %>% mutate(n1 = 1:n()) %>% 
  select(n1, n, regiao, Uf, NO_MUNICIPIO, valor_observado, ano)%>% arrange(-n)


# Merge pra saber em que posições os municípios dos ranking estão nos
# outros anos

# Merge de 2005 com 2013 e 2019
# Primeiras posições
primeiras2005 <- primeiras2005 %>%
  rename(n_melhores = n1, rank_2005 = n, ideb_2005 = valor_observado) %>% 
  inner_join(ranking2013, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2005) %>% 
  select(n_melhores, rank_2005, regiao, Uf, NO_MUNICIPIO, ideb_2005, n, valor_observado, ano.x) %>% 
  rename(rank_2013 = n, ideb_2013 = valor_observado) %>% 
  inner_join(ranking2019, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2005) %>% 
  select(n_melhores, rank_2005, regiao, Uf, NO_MUNICIPIO, ideb_2005, n, valor_observado, ideb_2013, rank_2013) %>% 
  rename(rank_2019 = n, ideb_2019 = valor_observado) %>% 
  select(n_melhores,rank_2005, regiao, Uf, NO_MUNICIPIO, 
         ideb_2005,rank_2013,ideb_2013, rank_2019, 
         ideb_2019) 

# Renomeando os dados de primeiras posições em 2005
names(primeiras2005) <- c(" ", "Ranking em 2005", "Região", "Estado", "Município", 
                          "Ideb em 2005", "Ranking em 2013", "Ideb em 2013", "Ranking em 2019", 
                          "Ideb em 2019")

# Últimas posições
ultimas2005 <- ultimas2005 %>% 
  rename(n_melhores = n1, rank_2005 = n, ideb_2005 = valor_observado) %>% 
  inner_join(ranking2013, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2005) %>% 
  select(n_melhores, rank_2005, regiao, Uf, NO_MUNICIPIO, ideb_2005, n, valor_observado, ano.x) %>% 
  rename(rank_2013 = n, ideb_2013 = valor_observado) %>% 
  left_join(ranking2019, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2005) %>% 
  select(n_melhores, rank_2005, regiao, Uf, NO_MUNICIPIO, ideb_2005, n, valor_observado, ideb_2013, rank_2013) %>% 
  rename(rank_2019 = n, ideb_2019 = valor_observado) %>% 
  select(n_melhores,rank_2005, regiao, Uf, NO_MUNICIPIO, 
         ideb_2005,rank_2013,ideb_2013, rank_2019, 
         ideb_2019) 

# Renomeando banco de dados das últimas posições em 2005
names(ultimas2005) <- c(" ", "Ranking em 2005", "Região", "Estado", "Município", 
                        "Ideb em 2005", "Ranking em 2013", "Ideb em 2013", "Ranking em 2019", 
                        "Ideb em 2019")

# Merge de 2013 com 2005 e 2019
# Primeiras posições 
primeiras2013 <- primeiras2013 %>%
  rename(n_melhores = n1, rank_2013 = n, ideb_2013 = valor_observado) %>% 
  full_join(ranking2005, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2013) %>% 
  select(n_melhores, rank_2013, regiao, Uf, NO_MUNICIPIO, ideb_2013, n, valor_observado, ano.x) %>% 
  rename(rank_2005 = n, ideb_2005 = valor_observado) %>% 
  full_join(ranking2019, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2013) %>% 
  select(n_melhores, rank_2013, regiao, Uf, NO_MUNICIPIO, ideb_2013, n, valor_observado, ideb_2005, rank_2005) %>% 
  rename(rank_2019 = n, ideb_2019 = valor_observado) %>% 
  select(n_melhores,rank_2013, regiao, Uf, NO_MUNICIPIO, 
         ideb_2013,rank_2005,ideb_2005, rank_2019, 
         ideb_2019)

# Renomeando colunas das primeiras posições de 2013
names(primeiras2013) <- c(" ", "Ranking em 2013", "Região", "Estado", "Município", 
                          "Ideb em 2013", "Ranking em 2005", "Ideb em 2005", "Ranking em 2019", 
                          "Ideb em 2019")

# Últimas posições
ultimas2013 <- ultimas2013 %>% 
  rename(n_melhores = n1, rank_2013 = n, ideb_2013 = valor_observado) %>% 
  full_join(ranking2005, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2013) %>% 
  select(n_melhores, rank_2013, regiao, Uf, NO_MUNICIPIO, ideb_2013, n, valor_observado, ano.x) %>% 
  rename(rank_2005 = n, ideb_2005 = valor_observado) %>% 
  full_join(ranking2019, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2013) %>% 
  select(n_melhores, rank_2013, regiao, Uf, NO_MUNICIPIO, ideb_2013, n, valor_observado, ideb_2005, rank_2005) %>% 
  rename(rank_2019 = n, ideb_2019 = valor_observado) %>% 
  select(n_melhores,rank_2013, regiao, Uf, NO_MUNICIPIO, 
         ideb_2013,rank_2005,ideb_2005, rank_2019, 
         ideb_2019) 

# Renomeando colunas dos dados das últimas posições do ranking de 2013
names(ultimas2013) <- c(" ", "Ranking em 2013", "Região", "Estado", "Município", 
                        "Ideb em 2013", "Ranking em 2005", "Ideb em 2005", "Ranking em 2019", 
                        "Ideb em 2019")

# Merge de 2019 com 2005 e 2013
# Primeiras posições
primeiras2019 <- primeiras2019 %>%
  rename(n_melhores = n1, rank_2019 = n, ideb_2019 = valor_observado) %>% 
  full_join(ranking2005, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2019) %>% 
  select(n_melhores, rank_2019, regiao, Uf, NO_MUNICIPIO, ideb_2019, n, valor_observado, ano.x) %>% 
  rename(rank_2005 = n, ideb_2005 = valor_observado) %>% 
  full_join(ranking2013, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2019) %>% 
  select(n_melhores, rank_2019, regiao, Uf, NO_MUNICIPIO, ideb_2019, n, valor_observado, ideb_2005, rank_2005) %>% 
  rename(rank_2013 = n, ideb_2013 = valor_observado) %>% 
  select(n_melhores,rank_2019, regiao, Uf, NO_MUNICIPIO, 
         ideb_2019,rank_2005,ideb_2005, rank_2013, 
         ideb_2013)

# Renomeando colunas dos dados das primeiras posições do ranking de 2019
names(primeiras2019) <- c(" ", "Ranking em 2019", "Região", "Estado", "Município", 
                          "Ideb em 2019", "Ranking em 2005", "Ideb em 2005",
                          "Ranking em 2013", 
                          "Ideb em 2013")

# Últimas posições de 2019
ultimas2019 <- ultimas2019 %>% 
  rename(n_melhores = n1, rank_2019 = n, ideb_2019 = valor_observado) %>% 
  full_join(ranking2005, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2019) %>% 
  select(n_melhores, rank_2019, regiao, Uf, NO_MUNICIPIO, ideb_2019, n, valor_observado, ano.x) %>% 
  rename(rank_2005 = n, ideb_2005 = valor_observado) %>% 
  full_join(ranking2013, by = c("regiao", "Uf", "NO_MUNICIPIO")) %>%
  filter(ano.x == 2019) %>% 
  select(n_melhores, rank_2019, regiao, Uf, NO_MUNICIPIO, ideb_2019, n, valor_observado, ideb_2005, rank_2005) %>% 
  rename(rank_2013 = n, ideb_2013 = valor_observado) %>% 
  select(n_melhores,rank_2019, regiao, Uf, NO_MUNICIPIO, 
         ideb_2019,rank_2005,ideb_2005, rank_2013, 
         ideb_2013)  

# Renomeando dados das últimas posições de 2019
names(ultimas2019) <- c(" ", "Ranking em 2019", "Região", "Estado", "Município", 
                        "Ideb em 2019", "Ranking em 2005", "Ideb em 2005",
                        "Ranking em 2013", 
                        "Ideb em 2013")

# Juntando os dados das primeiras posições em 2005, 2013 e 2019
primeiras <- primeiras2005 %>% full_join(primeiras2013) %>%
  full_join(primeiras2019)
primeiras <- primeiras[,-1]
primeiras <- primeiras[,c(2,3,4,1,5:9)]

# Renomeando dados das primeiras posições
names(primeiras) <- c("Região", "Estado", "Município", "Posição", "Ideb", "Posição", "Ideb", "Posição", "Ideb")

# Tabela das primeiras posições do ranking em 2005, 2013 e 2019
kable(primeiras, caption = "Lista dos municípios segundo as dez primeiras posições do Ideb. Brasil. 2005, 2013 e 2019", 
      "latex", booktabs = T) %>%   
  kable_styling(full_width = F, font_size = 10, position = 'center', latex_options="scale_down") %>% 
  add_header_above(c(" ","","", "2005" = 2, "2013" = 2, "2019" = 2)) %>%
  pack_rows("2005", 1, 10) %>%
  pack_rows("2013", 11, 20)%>%
  pack_rows("2019", 21, 30) %>% 
  collapse_rows(1, latex_hline = "none")

# Juntando os dados das últimas posições em 2005, 2013 e 2019
ultimas <- ultimas2005 %>% full_join(ultimas2013) %>%
  full_join(ultimas2019)
ultimas <- ultimas[,-1]
ultimas <- ultimas[,c(2,3,4,1,5:9)]

# Renomeando dados das ultimas posições
names(ultimas) <- c("Região", "Estado", "Município", "Posição", "Ideb", "Posição", "Ideb", "Posição", "Ideb")

# Tabela das ultimas posições do ranking em 2005, 2013 e 2019
kable(ultimas, caption = "Lista dos municípios segundo as dez últimas posições do Ideb. Brasil. 2005, 2013 e 2019", 
      "latex", booktabs = T) %>%   
  kable_styling(full_width = F, font_size = 10, position = 'center', latex_options="scale_down") %>% 
  add_header_above(c(" ","","", "2005" = 2, "2013" = 2, "2019" = 2)) %>%
  pack_rows("2005", 1, 10) %>%
  pack_rows("2013", 11, 20)%>%
  pack_rows("2019", 21, 30) %>% 
  collapse_rows(1, latex_hline = "none")

# 4. Mapa do Ideb segundo município

# Classificação da variável
ai2005 <- anos_iniciais_mun %>% filter(ano == 2005) %>% 
  mutate(clas_ideb = ifelse(valor_observado >= 0.5 & valor_observado <= 1.5, 
                            "(0,5-1,5]", 
                            ifelse(valor_observado>1.5 & valor_observado<=2.5, 
                                   "(1,5-2,5]", 
                                   ifelse(valor_observado>2.5&valor_observado<=3.5,
                                          "(2,5-3,5]", 
                                          ifelse(valor_observado>3.5&valor_observado<=4.5,
                                                 "(3,5-4,5]", 
                                                 ifelse(valor_observado>4.5&valor_observado<=5.5, 
                                                        "(4,5-5,5]", 
                                                        ifelse(valor_observado>5.5&valor_observado<=6.5, 
                                                               "(5,5-6,5]", 
                                                               ifelse(valor_observado>6.5&valor_observado<=7.5, 
                                                                      "(6,5-7,5]", 
                                                                      ifelse(valor_observado>7.5&valor_observado<=8.5, 
                                                                             "(7,5-8,5]", 
                                                                             ifelse(valor_observado>8.5&valor_observado<=9.5, 
                                                                                    "(8,5-9,5]", ifelse(is.na(valor_observado),"Sem informações", NA)))))))))))

ai2013 <- anos_iniciais_mun %>% filter(ano == 2013)%>% 
  mutate(clas_ideb = ifelse(valor_observado >= 0.5 & valor_observado <= 1.5, 
                            "(0,5-1,5]", 
                            ifelse(valor_observado>1.5 & valor_observado<=2.5, 
                                   "(1,5-2,5]", 
                                   ifelse(valor_observado>2.5&valor_observado<=3.5,
                                          "(2,5-3,5]", 
                                          ifelse(valor_observado>3.5&valor_observado<=4.5,
                                                 "(3,5-4,5]", 
                                                 ifelse(valor_observado>4.5&valor_observado<=5.5, 
                                                        "(4,5-5,5]", 
                                                        ifelse(valor_observado>5.5&valor_observado<=6.5, 
                                                               "(5,5-6,5]", 
                                                               ifelse(valor_observado>6.5&valor_observado<=7.5, 
                                                                      "(6,5-7,5]", 
                                                                      ifelse(valor_observado>7.5&valor_observado<=8.5, 
                                                                             "(7,5-8,5]", 
                                                                             ifelse(valor_observado>8.5&valor_observado<=9.5, 
                                                                                    "(8,5-9,5]", ifelse(is.na(valor_observado),"Sem informações", NA)))))))))))


ai2019 <- anos_iniciais_mun %>% filter(ano == 2019)%>% 
  mutate(clas_ideb = ifelse(valor_observado >= 0.5 & valor_observado <= 1.5, 
                            "(0,5-1,5]", 
                            ifelse(valor_observado>1.5 & valor_observado<=2.5, 
                                   "(1,5-2,5]", 
                                   ifelse(valor_observado>2.5&valor_observado<=3.5,
                                          "(2,5-3,5]", 
                                          ifelse(valor_observado>3.5&valor_observado<=4.5,
                                                 "(3,5-4,5]", 
                                                 ifelse(valor_observado>4.5&valor_observado<=5.5, 
                                                        "(4,5-5,5]", 
                                                        ifelse(valor_observado>5.5&valor_observado<=6.5, 
                                                               "(5,5-6,5]", 
                                                               ifelse(valor_observado>6.5&valor_observado<=7.5, 
                                                                      "(6,5-7,5]", 
                                                                      ifelse(valor_observado>7.5&valor_observado<=8.5, 
                                                                             "(7,5-8,5]", 
                                                                             ifelse(valor_observado>8.5&valor_observado<=9.5, 
                                                                                    "(8,5-9,5]", ifelse(is.na(valor_observado),"Sem informações", NA)))))))))))


# Para a elaboração do mapa é necessário as coordenadas dos municípios
# brasileiros
brasil <- read_municipality() %>% 
  rename(CO_MUNICIPIO = code_muni)

# Cores do mapa
cores_mapa <- c("#000099", "#6666FF", "#66CCCC", "#33CC66", 
                "#00FF00", "#FFFF66", "#FFCC33", 
                "#FF6600", "#FF0000", 
                "#FF99FF")

# Mapa de 2005
mapa_2005 <- ai2005 %>% inner_join(brasil, by = "CO_MUNICIPIO") %>% 
  mutate(clas_ideb = ifelse(is.na(clas_ideb), "Sem informação", clas_ideb), 
         clas_ideb = factor(clas_ideb, levels = c("(0,5-1,5]", "(1,5-2,5]", "(2,5-3,5]", "(3,5-4,5]", 
                                                  "(4,5-5,5]", "(5,5-6,5]", "(6,5-7,5]", "(7,5-8,5]", 
                                                  "(8,5-9,5]", "Sem informação")))

mapa05 <- ggplot() +
  geom_sf(data = mapa_2005$geom,color = NA, aes(fill = mapa_2005$clas_ideb), size=.15, alpha = 0.9)+
  scale_fill_manual(values = cores_mapa, drop = F) +
  labs( )+
  theme_light()+
  theme(legend.position = "none",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Mapa de 2013
mapa_2013 <- ai2013 %>% inner_join(brasil, by = "CO_MUNICIPIO") %>% 
  mutate(clas_ideb = ifelse(is.na(clas_ideb), "Sem informação", clas_ideb), 
         clas_ideb = factor(clas_ideb, levels = c("(0,5-1,5]", "(1,5-2,5]", "(2,5-3,5]", "(3,5-4,5]", 
                                                  "(4,5-5,5]", "(5,5-6,5]", "(6,5-7,5]", "(7,5-8,5]", 
                                                  "(8,5-9,5]", "Sem informação")))

mapa13 <- ggplot() +
  geom_sf(data = mapa_2013$geom,color = NA, aes(fill = mapa_2013$clas_ideb), size=.15, alpha = 0.9)+
  scale_fill_manual(values = cores_mapa, drop = F) +
  labs(fill = "Escala do Ideb: ")+
  theme_light()+
  theme(legend.position = "none",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Mapa de 2019
mapa_2019 <- ai2019 %>% inner_join(brasil, by = "CO_MUNICIPIO") %>% 
  mutate(clas_ideb = ifelse(is.na(clas_ideb), "Sem informação", clas_ideb), 
         clas_ideb = factor(clas_ideb, levels = c("(0,5-1,5]", "(1,5-2,5]", "(2,5-3,5]", "(3,5-4,5]", 
                                                  "(4,5-5,5]", "(5,5-6,5]", "(6,5-7,5]", "(7,5-8,5]", 
                                                  "(8,5-9,5]", "Sem informação")))

mapa19 <- ggplot() +
  geom_sf(data = mapa_2019$geom,color = NA, aes(fill = mapa_2019$clas_ideb), size=.15)+
  scale_fill_manual(values = cores_mapa, drop = F) +
  labs(fill = "Escala do Ideb: ")+
  theme_light()+
  theme(  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  theme(legend.position = 'bottom',
        legend.title = element_text(size=14),
        legend.text = element_text(size=13),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )


# Apresentando o três mapas em só uma figura
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(mapa19)

p3 <- grid.arrange(arrangeGrob(mapa05 + theme(legend.position="none"),
                               mapa13+ theme(legend.position="none"),
                               mapa19+theme(legend.position = "none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(12, 1))


