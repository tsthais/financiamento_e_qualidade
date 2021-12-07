### FINANCIAMENTO E QUALIDADE DA EDUCAÇÃO NO BRASIL - INDICADOR DE INVESTIMENTO ##

# Bibliotecas utilizadas

require(dplyr);require(ggplot2);require(moments)
require(readxl);require(tidyr);require(RColorBrewer)
require(gridExtra);require(kableExtra);require(stringr)
require(scales);require(nortest);require(lmtest);
require(geobr);require(Hmisc)

# 1. Limpeza dos dados do Siope

# Diretorio de trabalho
setwd("D:/2020_2/TCC1/Bases/Indicadores SIOPE")

# Importando os dados
indicadores <- readRDS("indicadores_siope_final.rds")

# Tratamento dos dados:
# Criando uma variavel com as siglas dos estados;
# Alterando o nome dos indicadores; 
# Inserindo uma coluna de região
indicadores <- indicadores %>% 
  rename(Uf = sigla_uf, CO_INDICADOR = nome_indicador) %>% 
  mutate(regiao = ifelse(Uf == "RR"| Uf == 'AM'| Uf == "AP"| Uf == "PA"| Uf == "AC"| Uf == 'RO'| Uf == 'TO', "Norte",
                         ifelse(Uf == 'MT'| Uf =='DF'| Uf =='GO'| Uf =='MS', 'Centro- Oeste',
                                ifelse(Uf == 'MA'| Uf =='CE'| Uf =='RN'| Uf =='PB'| Uf == 'PI'| Uf =='PE'| Uf =='AL'| Uf =='SE'| Uf =='BA', 'Nordeste',
                                       ifelse(Uf == 'MG'| Uf =='ES'| Uf =='RJ'| Uf =='SP', 'Sudeste',
                                              ifelse(Uf == 'PR'| Uf =='SC'| Uf =='RS', 'Sul', 'X'))))), 
         indicador = ifelse(CO_INDICADOR == "1.2", "IND102", 
                            ifelse(CO_INDICADOR == "2.1", "IND201", 
                                   ifelse(CO_INDICADOR == "2.2", "IND202", 
                                          ifelse(CO_INDICADOR == "2.3", "IND203", 
                                                 ifelse(CO_INDICADOR == "4.2", "IND402", NA)))))) 

# Escolhendo o indicador que será analisado, indicador 4.2
ind402 <- indicadores %>% 
  filter(indicador == "IND402") %>% 
  filter(Ano > 2006 & Ano < 2020) %>% filter(valor != 0)

# 2. Análise descritiva

# Medidas descritivas do indicador no País
tapply(ind402$valor, ind402$indicador, summary)
tapply(ind402$valor, ind402$indicador, var)

# Medidas descritivas do indicador no País por ano
medidas_ano <- ind402 %>% group_by(Ano, indicador) %>% 
  summarise( min = min(valor, na.rm = T), 
             prim_quar = quantile(valor, probs = 0.25, na.rm = T), 
             mediana = median(valor, na.rm = T), 
             media = mean(valor, na.rm = T), 
             ter_qaur = quantile(valor, probs = 0.75, na.rm = T), 
             max = max(valor, na.rm = T), 
             var = var(valor, na.rm = T),
             ampl = diff(range(valor, na.rm = T)), 
             sd = sd(valor, na.rm = T),
             curt = kurtosis(valor, na.rm = T)) %>% 
  mutate(cv = (sd/media)*100)


# Tabela de medidas descritivas para apresentação
tabela1 <- medidas_ano %>% select(Ano, min, prim_quar, mediana, media, ter_qaur, max, cv)
names(tabela1) <- c("Ano", "Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Coeficiente de Variação (%)")
tabela1 <- tabela1 %>% 
  mutate(`Mínimo` = format(round(`Mínimo`,2), big.mark = '.', decimal.mark = ','), 
         `1º Quartil` = format(round(`1º Quartil`,2), big.mark = '.', decimal.mark = ','), 
         `Mediana` = format(round(`Mediana`,2), big.mark = '.', decimal.mark = ','), 
         `Média` = format(round(`Média`,2), big.mark = '.', decimal.mark = ','), 
         `3º Quartil` = format(round(`3º Quartil`,2), big.mark = '.', decimal.mark = ','), 
         `Máximo` = format(round(`Máximo`,2), big.mark = '.', decimal.mark = ','), 
         `Coeficiente de Variação (%)` = format(round(`Coeficiente de Variação (%)`,2), big.mark = '.', decimal.mark = ','))

kable(tabela1, caption = "Medidas descritivas do Indicador 4.2 de 2008 a 2019", "latex") %>%   
  kable_styling(full_width = F, font_size = 10, position = 'center', latex_options="scale_down")


# Gráfico de Boxplot do indicador de investimento no Brasil
ind402 %>% filter(indicador == "IND402") %>% 
  ggplot(aes(x = as.factor(Ano), y = valor))+
  geom_boxplot(fill = "#4DAF4A",color="black", alpha = .6)+
  stat_summary(fun.y="mean", shape = 15, size = .3)+
  labs(x = "", y = "Indicador 4.2")+
  theme(legend.position = "bottom", 
        plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  theme_light()+
  scale_y_continuous(labels = unit_format(unit = "MIL", scale = 1e-3))

# Medidas descritivas por região
medidas_reg <- ind402 %>% group_by(Ano, indicador, regiao) %>% 
  summarise( min = min(valor, na.rm = T), 
             prim_quar = quantile(valor, probs = 0.25, na.rm = T), 
             mediana = median(valor, na.rm = T), 
             media = mean(valor, na.rm = T), 
             ter_qaur = quantile(valor, probs = 0.75, na.rm = T), 
             max = max(valor, na.rm = T), 
             var = var(valor, na.rm = T),
             ampl = diff(range(valor, na.rm = T)), 
             sd = sd(valor, na.rm = T),
             curt = kurtosis(valor, na.rm = T)) %>% 
  mutate(cv = (sd/media)*100)

# Gráfico com as médias do indicador por região
medidas_br <- medidas_ano %>% select(Ano, media, indicador) %>% filter(indicador == "IND402") %>%  mutate(regiao = "Brasil")
medidas_regioes <- medidas_reg %>% select(regiao,Ano, media, indicador) %>% filter(indicador == "IND402")%>% full_join(medidas_br)

ggplot(medidas_regioes, aes(x = Ano, y = media, color = regiao))+
  geom_point(size = 2) + 
  geom_line(size = 1, alpha = .8, show.legend = F) +
  scale_x_continuous(breaks = seq(2005,2020,by = 2))+
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                                "#FF7F00", "#F781BF"))+
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                               "#FF7F00", "#F781BF"))+
  labs(x = "", y = "Média do Indicador 4.2", color = "Região: ")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  scale_y_continuous(labels = unit_format(unit = "MIL", scale = 1e-3))

# Tabela de médias do indicador por ano para apresentação
tabela2 <- medidas_br[,c(1:2)] 
tabela2 <- medidas_br %>% mutate(media = format(round(media, digits = 2), big.mark = '.', decimal.mark = ',')) %>% 
  spread(Ano, media)

kable(tabela2, caption = "Média do Indicador 4.2 no Brasil", "latex") %>%   
  kable_styling(full_width = F, font_size = 10, position = 'center', latex_options="scale_down")


# Medidas descritivas do indicador por estado
medidas.uf <- ind402 %>% group_by(Uf, regiao,Ano, indicador) %>% 
  summarise( min = min(valor, na.rm = T), 
             prim_quar = quantile(valor, probs = 0.25, na.rm = T), 
             mediana = median(valor, na.rm = T), 
             media = mean(valor, na.rm = T), 
             ter_qaur = quantile(valor, probs = 0.75, na.rm = T), 
             max = max(valor, na.rm = T), 
             var = var(valor, na.rm = T),
             ampl = diff(range(valor, na.rm = T)), 
             sd = sd(valor, na.rm = T),
             curt = kurtosis(valor, na.rm = T)) %>% 
  mutate(cv = (sd/media)*100)

# Gráfico de médias dos estados da região Centro- Oeste
ind402 %>% filter(regiao == "Centro- Oeste") %>% 
  filter(Ano == 2009|Ano == 2013| Ano == 2019) %>%
  ggplot(aes(Uf, valor,group = indicador, color = indicador)) +
  facet_wrap(~ Ano) +
  geom_point(position = position_jitterdodge(jitter.width = .2, 
                                             dodge.width = .7), 
             alpha = .1) +
  stat_summary(fun.y = mean, na.rm = TRUE, 
               geom = "point", shape = "diamond",
               size = 4, color = "black", 
               position = position_dodge(width = .7)) +
  stat_summary(fun.data = mean_cl_normal, na.rm = TRUE, 
               geom = "errorbar", width = .2, color = "black",
               position = position_dodge(width = .7)) +
  scale_color_brewer(palette = "Set1")+
  labs(x = "Estados", y = "Valor dos indicadores", color = "Indicador: ")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  scale_y_continuous(labels = unit_format(unit = "MIL", scale = 1e-3))

# Gráfico de médias dos estados da região Nordeste
ind402 %>% filter(regiao == "Nordeste") %>% 
  filter(Ano == 2009|Ano == 2013| Ano == 2019) %>%
  ggplot(aes(Uf, valor,group = indicador, color = indicador)) +
  facet_wrap(~ Ano) +
  geom_point(position = position_jitterdodge(jitter.width = .2, 
                                             dodge.width = .7), 
             alpha = .1) +
  stat_summary(fun.y = mean, na.rm = TRUE, 
               geom = "point", shape = "diamond",
               size = 4, color = "black", 
               position = position_dodge(width = .7)) +
  stat_summary(fun.data = mean_cl_normal, na.rm = TRUE, 
               geom = "errorbar", width = .2, color = "black",
               position = position_dodge(width = .7)) +
  scale_color_brewer(palette = "Set1")+
  labs(x = "Estados", y = "Valor dos indicadores", color = "Indicador: ")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  scale_y_continuous(labels = unit_format(unit = "MIL", scale = 1e-3))

# Gráfico de médias dos estados da região Norte
ind402 %>% filter(regiao == "Norte") %>% 
  filter(Ano == 2009|Ano == 2013| Ano == 2019) %>%
  ggplot(aes(Uf, valor,group = indicador, color = indicador)) +
  facet_wrap(~ Ano) +
  geom_point(position = position_jitterdodge(jitter.width = .2, 
                                             dodge.width = .7), 
             alpha = .1) +
  stat_summary(fun.y = mean, na.rm = TRUE, 
               geom = "point", shape = "diamond",
               size = 4, color = "black", 
               position = position_dodge(width = .7)) +
  stat_summary(fun.data = mean_cl_normal, na.rm = TRUE, 
               geom = "errorbar", width = .2, color = "black",
               position = position_dodge(width = .7)) +
  scale_color_brewer(palette = "Set1")+
  labs(x = "Estados", y = "Valor dos indicadores", color = "Indicador: ")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  scale_y_continuous(labels = unit_format(unit = "MIL", scale = 1e-3))

# Gráfico de médias dos estados da região Sudeste
ind402 %>% filter(regiao == "Sudeste") %>% 
  filter(Ano == 2009|Ano == 2013| Ano == 2019) %>%
  ggplot(aes(Uf, valor,group = indicador, color = indicador)) +
  facet_wrap(~ Ano) +
  geom_point(position = position_jitterdodge(jitter.width = .2, 
                                             dodge.width = .7), 
             alpha = .1) +
  stat_summary(fun.y = mean, na.rm = TRUE, 
               geom = "point", shape = "diamond",
               size = 4, color = "black", 
               position = position_dodge(width = .7)) +
  stat_summary(fun.data = mean_cl_normal, na.rm = TRUE, 
               geom = "errorbar", width = .2, color = "black",
               position = position_dodge(width = .7)) +
  scale_color_brewer(palette = "Set1")+
  labs(x = "Estados", y = "Valor dos indicadores", color = "Indicador: ")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  scale_y_continuous(labels = unit_format(unit = "MIL", scale = 1e-3))

# Gráfico de médias dos estados da região Sul
ind402 %>% filter(regiao == "Sul") %>% 
  filter(Ano == 2009|Ano == 2013| Ano == 2019) %>%
  ggplot(aes(Uf, valor,group = indicador, color = indicador)) +
  facet_wrap(~ Ano) +
  geom_point(position = position_jitterdodge(jitter.width = .2, 
                                             dodge.width = .7), 
             alpha = .1) +
  stat_summary(fun.y = mean, na.rm = TRUE, 
               geom = "point", shape = "diamond",
               size = 4, color = "black", 
               position = position_dodge(width = .7)) +
  stat_summary(fun.data = mean_cl_normal, na.rm = TRUE, 
               geom = "errorbar", width = .2, color = "black",
               position = position_dodge(width = .7)) +
  scale_color_brewer(palette = "Set1")+
  labs(x = "Estados", y = "Valor dos indicadores", color = "Indicador: ")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))+
  scale_y_continuous(labels = unit_format(unit = "MIL", scale = 1e-3))


# Gráfico de barras do indicador segundo município
clas_mun <- ind402 %>% 
  mutate(class402 = ifelse(valor <=1000, "<1 mil ", 
                           ifelse(valor>1000&valor<=3000, "de 1 a \n 3 mil", 
                                  ifelse(valor>3000&valor<=5000, "de 3 a \n 5 mil", 
                                         ifelse(valor>5000&valor<=7000, "de 5 a \n 7 mil", 
                                                ifelse(valor>7000&valor<=9000, "de 7 a \n 9 mil", 
                                                       ifelse(valor>9000&valor<=11000, "de 9 a \n 11 mil", 
                                                              ifelse(valor>11000&valor<=13000, "de 11 a \n 13 mil", 
                                                                     ifelse(valor>13000&valor<=15000, "de 13 a \n 15 mil", 
                                                                            ifelse(valor>15000, " >15 mil", NA))))))))), 
         class402 = factor(class402, levels = c("<1 mil ", "de 1 a \n 3 mil", "de 3 a \n 5 mil", 
                                                "de 5 a \n 7 mil","de 7 a \n 9 mil",  "de 9 a \n 11 mil", 
                                                "de 11 a \n 13 mil","de 13 a \n 15 mil", " >15 mil" )))

clas_mun %>% filter(Ano == 2009|Ano == 2013|Ano == 2019) %>% 
  filter(indicador == "IND402") %>% 
  ggplot( aes(x= class402)) +
  geom_bar(fill = "#984EA3",color="black", alpha = .6)+
  facet_wrap(~Ano)+
  labs(x = "\n Classificação indicador (em reais)", y = "Contagem municípios")+
  theme_light()+
  theme(legend.position = "bottom",strip.text = element_text(size = 20, face = "bold"),  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))

# Tabela de proporção do indicador
prop <- clas_mun %>% group_by(Ano) %>% mutate(n =1, soma = sum(n, na.rm = T)) %>% 
  group_by(Ano, class402) %>% 
  mutate(n=1, soma1 = sum(n, na.rm = T)) %>% 
  summarise(prop = round((soma1/soma)*100, 2)) %>% 
  mutate(n=1:n()) %>% 
  filter(n == 1) %>% 
  filter(Ano == 2009|Ano == 2013|Ano == 2019) %>% 
  select(-n) %>% 
  spread(class402, prop)


