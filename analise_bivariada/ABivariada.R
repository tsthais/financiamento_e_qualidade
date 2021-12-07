# Análise Bivariada


# A base de anos iniciais foi carregada antes no código de análise 
# descritiva do Ideb
anos_in <- anos_iniciais_mun %>% 
  rename(codigo_municipio = CO_MUNICIPIO, Ano = ano) %>% 
  mutate(codigo_municipio = as.numeric(str_sub(codigo_municipio, 1, 6)))

# Também, antes deste código foi carregado os dados no código de análise de 
# dados do Siope
# Os dados do indicador 4.2 e do Ideb foram reunidos em um só banco de dados
# Além disso foram separados 90% dos dados de investimento por aluno, esses
# dados que foram separa que foram usados na análise
base <- ind402 %>% full_join(anos_in)%>% 
  filter(valor>0 & valor < 664127.05) %>%
  filter(!is.na(valor)) %>% 
  filter(!is.na(valor_observado))

base <- base %>% filter(valor>quantile(valor,0.05)&valor<quantile(valor, 0.95))

# Gráfico de dispersão para os anos de 2009, 2013 e 2019
base %>%  filter(Ano == 2019|Ano == 2013| Ano == 2009) %>% 
  ggplot(aes(y = valor_observado, x = valor))+
  geom_point(alpha = 0.6)+
  geom_smooth(method = "lm", se = F, color = "#FF7F00")+
  facet_wrap(~Ano)+
  labs(y = "Ideb", x = "Indicador 4.2")+
  theme_light()+
  theme(legend.position = "bottom",strip.text = element_text(size = 20, face = "bold"),  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))

# Correlação por ano a partir de 2009
base %>% group_by(Ano) %>% summarise(cor = cor(valor, valor_observado))
base %>% group_by(Ano) %>% summarise(cort = cor.test(valor, valor_observado)$p.value)
