### FINANCIAMENTO E QUALIDADE DA EDUCAÇÃO NO BRASIL - MODELO ##

# Bibliotecas utilizadas

require(dplyr);require(ggplot2);require(moments)
require(readxl);require(tidyr);require(RColorBrewer)
require(gridExtra);require(kableExtra);require(stringr)
require(scales);require(nortest);require(lmtest);
require(geobr);require(Hmisc);require(mixlm)


# Importando dados do tamanho populacional brasileiro
pop <- function(nome, ano, numero1, numero2){
  tamanhopop <- read_xls(paste0("C:\\Users\\thais\\Downloads\\base_de_dados_pib\\UF_Municipio",nome,".xls"), skip=numero1, sheet = numero2)
  tamanhopop <- tamanhopop %>% mutate(codigo_municipio = paste0(`COD. UF`, `COD. MUNIC`),
                                      codigo_municipio = as.numeric(str_sub(codigo_municipio, 1, 6)), 
                                      Ano = ano) %>% 
    rename(Uf = UF, municipio2 = `NOME DO MUNICÍPIO`, pop = `POPULAÇÃO ESTIMADA`) %>% 
    mutate(pop = as.numeric(as.character(pop))) %>% 
    filter(!is.na(municipio2)) %>% select(-`COD. UF`, -`COD. MUNIC`) 
  return(tamanhopop)
}

tamanhopop11 <- pop(11, 2011, 2, 1)
tamanhopop13 <- pop(13, 2013, 2, 1)
tamanhopop15 <- pop(15, 2015, 2, 2)
tamanhopop17 <- pop(17, 2017, 1, 2)
tamanhopop19 <- pop(19, 2019, 1, 2)

tamanhopop <- tamanhopop11 %>% full_join(tamanhopop13) %>% full_join(tamanhopop15) %>% 
  full_join(tamanhopop17) %>% full_join(tamanhopop19)

# Importando bases do Pib e tratando esta base
pib1 <- read_xls("C:\\Users\\thais\\Downloads\\base_de_dados_pib\\PIB dos Municípios - 2002-2009.xls")
pib2 <- read_xls("C:\\Users\\thais\\Downloads\\base_de_dados_pib\\PIB dos Municípios - 2010-2018.xls")
names(pib1)
names(pib2)

pib1 <- pib1 %>% select(Ano, `Sigla da Unidade da Federação`, 
                        `Código do Município`, `Nome do Município`, 
                        `Produto Interno Bruto per capita, \na preços correntes\n(R$ 1,00)`)
pib2 <- pib2 %>% select(Ano, `Sigla da Unidade da Federação`, 
                        `Código do Município`, `Nome do Município`, 
                        `Produto Interno Bruto per capita, \na preços correntes\n(R$ 1,00)`)
pib <- pib1 %>% full_join(pib2) %>% mutate(Ano = ifelse(Ano == 2018, 2019, Ano))

names(pib) <- c("Ano", "Uf", "codigo_municipio", "municipio1", "pib_pc")

pib <- pib%>% 
  mutate(codigo_municipio = as.numeric(str_sub(codigo_municipio, 1, 6)))

# Juntando os dados das variáveis analisadas com as bases de pib e tamanho 
# populacional
base10 <- base %>% full_join(pib, by = c("codigo_municipio", "Ano", "Uf")) %>% 
  full_join(tamanhopop, by = c("codigo_municipio", "Uf", "Ano")) %>% 
  select(-municipio1) %>% mutate(inv_pib = valor/pib_pc)

# O modelo só será ajustado para o estado do paraná
# Além disso, nas linhas de código abaixo são classificadas as variáveis pib e 
# tamanho populacional
parana <- base10 %>% filter(Uf == "PR") %>% group_by(Ano) %>% 
  mutate(class_pib = ifelse(pib_pc <= quantile(pib_pc, probs = 1/4, na.rm = T), "Pequeno",
                            ifelse(pib_pc > quantile(pib_pc, probs = 1/4, na.rm = T) & pib_pc <= quantile(pib_pc, .5, na.rm = T), "Médio", 
                                   ifelse(pib_pc>quantile(pib_pc, .5, na.rm = T) & pib_pc<= quantile(pib_pc, 3/4, na.rm = T), "Grande", 
                                          ifelse(pib_pc> quantile(pib_pc, 3/4, na.rm = T)&pib_pc<= quantile(pib_pc,1, na.rm = T), "Muito grande", NA)))), 
         class_pop = ifelse(pop <= quantile(pop, probs = 1/4, na.rm = T), "Pequeno",
                            ifelse(pop > quantile(pop, probs = 1/4, na.rm = T) & pop <= quantile(pop, .5, na.rm = T), "Médio", 
                                   ifelse(pop>quantile(pop, .5, na.rm = T) & pop<= quantile(pop, 3/4, na.rm = T), "Grande", 
                                          ifelse(pop> quantile(pop, 3/4, na.rm = T)&pop<= quantile(pop,1, na.rm = T), "Muito grande", NA))))) %>% 
  filter(!is.na(municipio)) %>% filter(Ano >2009)

# Foram elaborados gráficos de dispersão entre as variáveis investimento e ideb
# para os anos de 2011, 2013, 2015, 2017 e 2019

# Para o ano de 2011:
parana %>% filter(Ano == 2011) %>% 
  ggplot(aes(y = valor_observado, x = valor, color = factor(class_pib, levels = c("Pequeno", "Médio", "Grande", "Muito grande"))))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = F, color = "#FF7F00", method = "lm")+
  scale_color_manual(values = cores, drop = F)+
  labs(y = "Ideb", x = "Indicador 4.2", color = "Classificação PIB per capita")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))

# Para o ano de 2013:
parana %>%   filter(Ano == 2013) %>% 
  ggplot(aes(y = valor_observado, x = valor, color = factor(class_pib, levels = c("Pequeno", "Médio", "Grande", "Muito grande"))))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = F, color = "#FF7F00", method = "lm")+
  scale_color_manual(values = cores, drop = F)+
  labs(y = "Ideb", x = "Indicador 4.2", color = "Classificação PIB per capita")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))

# Para o ano de 2015:
parana %>%   filter(Ano == 2015) %>% 
  ggplot(aes(y = valor_observado, x = valor, color = factor(class_pib, levels = c("Pequeno", "Médio", "Grande", "Muito grande"))))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = F, color = "#FF7F00", method = "lm")+
  scale_color_manual(values = cores, drop = F)+
  labs(y = "Ideb", x = "Indicador 4.2", color = "Classificação PIB per capita")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))

# Para o ano de 2017:
parana %>%   filter(Ano == 2017) %>% 
  ggplot(aes(y = valor_observado, x = (valor), color = factor(class_pib, levels = c("Pequeno", "Médio", "Grande", "Muito grande"))))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = F, color = "#FF7F00", method = "lm")+
  scale_color_manual(values = cores, drop = F)+
  labs(y = "Ideb", x = "Indicador 4.2", color = "Classificação PIB per capita")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))

# Para o ano de 2019
parana %>%   filter(Ano == 2019) %>% 
  ggplot(aes(y = valor_observado, x = valor, color = factor(class_pib, levels = c("Pequeno", "Médio", "Grande", "Muito grande"))))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = F, color = "#FF7F00", method = "lm")+
  scale_color_manual(values = cores, drop = F)+
  labs(y = "Ideb", x = "Indicador 4.2", color = "Classificação PIB per capita")+
  theme_light()+
  theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))

# Foi calculada a correlação entre as variáveis para os mesmos anos
parana %>% group_by(Ano) %>% summarise(correlacao = cor(log(valor), log(valor_observado)))


# 1. Função para o ajuste dos modelos simples
model <- function(dados, ano){
  # cores usadas no grafico
  cores <- c("#6666FF","#33CC66",  "#FFCC33",  "#FF0000")
  # nova base
  base <- dados %>% filter(Ano == ano)
  #grafico de dispersao
  r1 <- base %>%   
    ggplot(aes(y = log(valor_observado), x = log(inv_pib)))+
    geom_point(alpha = 0.6)+
    geom_smooth(se = F, color = "#FF7F00", method = "lm")+
    scale_color_manual(values = cores, drop = F)+
    labs(y = "log(Ideb)", x = "log(Indicador 4.2/PIB per capita)",
         #color = "Classificação PIB per capita", 
         title = paste("Correlação: ", round(cor(log(base$valor_observado), log(base$inv_pib)),4)))+
    theme_light()+
    theme(legend.position = "bottom",  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'), 
          plot.title = element_text(hjust = 0.5))
  # modelo
  modelo <- lm(log(valor_observado)~log(inv_pib), base)
  # significancia dos parametros
  a <- summary(modelo)
  r2 <- paste("O p-valor do teste de significância dos parâmetros foi igual a", round(pf(a$fstatistic[1], a$fstatistic[2], a$fstatistic[3], lower.tail = F), 5))
  #analise de residuos
  norm <- ad.test(modelo$residuals)
  homo <- bptest(modelo)
  r3 <- paste("O pvalor do teste de normalidade foi igual a", round(norm$p.value, 5), "e o de homocedasticidade,", round(homo$p.value,5))
  # teste t
  r4 <- paste("O resultado do teste de significância para cada parâmetro:")
  r5 <- a$coefficients
  # modelo 
  r6 <- modelo
  r7 <- a$adj.r.squared
  out <- list(r1, r2, r3, r4, r5, r6, r7)
  return(out)
}

# Modelo em 2011
modelo11 <- model(parana, 2011)
resultados11 <- data.frame(modelo11[[5]])
resultados11$var <- c("Intercepto", "log(Indicador 4.2/PIB per capita)")
resultados11 <- resultados11[,c(5,1,2,3,4)]
names(resultados11) <- c("Variáveis explicativas","$\\\beta$", 
                         "Erro Padronizado", "Estatística t", 
                         "P- valor")
kable(resultados11, "latex",caption = "Significância do coeficiente de regressão do modelo. 2011", escape = F)%>%   
  kable_styling(full_width = F, font_size = 10, position = 'center')

# Modelo em 2013
modelo13 <- model(parana, 2013)
resultados13 <- data.frame(modelo13[[5]])
resultados13$var <- c("Intercepto", "log(Indicador 4.2/PIB per capita)")
resultados13 <- resultados13[,c(5,1,2,3,4)]
names(resultados13) <- c("Variáveis explicativas","$\\\beta$", 
                         "Erro Padronizado", "Estatística t", 
                         "P- valor")
kable(resultados13, "latex",caption = "Significância do coeficiente de regressão do modelo. 2011", escape = F)%>%   
  kable_styling(full_width = F, font_size = 10, position = 'center')

# Modelo em 2015
modelo15 <- model(parana, 2015)
resultados15 <- data.frame(modelo15[[5]])
resultados15$var <- c("Intercepto", "log(Indicador 4.2/PIB per capita)")
resultados15 <- resultados15[,c(5,1,2,3,4)]
names(resultados15) <- c("Variáveis explicativas","$\\\beta$", 
                         "Erro Padronizado", "Estatística t", 
                         "P- valor")
kable(resultados15, "latex",caption = "Significância do coeficiente de regressão do modelo. 2011", escape = F)%>%   
  kable_styling(full_width = F, font_size = 10, position = 'center')

# Modelo em 2017
modelo17 <- model(parana, 2017)

# Modelo em 2019
modelo19 <- model(parana, 2019)

# 2. Novas variaveis para o modelo

# Percentual de professores com curso superior
# 2011
perc2011 <- read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\perc_docentes\\DSU - MUNICIPIOS 2011.xlsx", skip = 9)
perc2011 <- perc2011 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(PK_COD_MUNICIPIO = as.numeric(str_sub(PK_COD_MUNICIPIO, 1, 6))) %>% 
  select(ano, SIGLA, PK_COD_MUNICIPIO, DSU_F14) %>% 
  rename(Ano = ano, Uf = SIGLA, codigo_municipio = PK_COD_MUNICIPIO, pdcs = DSU_F14) %>% 
  mutate(pdcs = as.numeric(pdcs))
# 2013
perc2013 <-  read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\perc_docentes\\DSU - MUNICIPIOS 2013.xlsx", skip = 9)
perc2013 <- perc2013 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(PK_COD_MUNICIPIO = as.numeric(str_sub(PK_COD_MUNICIPIO, 1, 6))) %>% 
  select(ano, SIGLA, PK_COD_MUNICIPIO, DSU_F14) %>% 
  rename(Ano = ano, Uf = SIGLA, codigo_municipio = PK_COD_MUNICIPIO, pdcs = DSU_F14) %>% 
  mutate(pdcs = as.numeric(pdcs))
# 2015
perc2015 <-  read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\perc_docentes\\DSU_MUNICIPIOS_2015.xlsx", skip = 9)
perc2015 <- perc2015 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(CO_MUNICIPIO = as.numeric(str_sub(CO_MUNICIPIO, 1, 6))) %>% 
  select(NU_ANO_CENSO, SG_UF, CO_MUNICIPIO, DSU_F14) %>% 
  rename(Ano = NU_ANO_CENSO, Uf = SG_UF, codigo_municipio = CO_MUNICIPIO, pdcs = DSU_F14) %>% 
  mutate(pdcs = as.numeric(pdcs))
# 2017
perc2017 <-  read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\perc_docentes\\DSU_MUNICIPIOS_2017.xlsx", skip = 9)
perc2017 <- perc2017 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(NO_MUNICIPIO = as.numeric(str_sub(NO_MUNICIPIO, 1, 6))) %>% 
  select(NU_ANO_CENSO, SG_UF, NO_MUNICIPIO, DSU_F14) %>% 
  rename(Ano = NU_ANO_CENSO, Uf = SG_UF, codigo_municipio = NO_MUNICIPIO, pdcs = DSU_F14) %>% 
  mutate(pdcs = as.numeric(pdcs))
# 2019
perc2019 <-  read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\perc_docentes\\DSU_MUNICIPIOS_2019.xlsx", skip = 9)
perc2019 <- perc2019 %>% filter(NO_DEPENDENCIA == "Municipal" & NO_CATEGORIA == "Total") %>% 
  mutate(CO_MUNICIPIO = as.numeric(str_sub(CO_MUNICIPIO, 1, 6))) %>% 
  select(NU_ANO_CENSO, SG_UF, CO_MUNICIPIO, FUN_AI_CAT_0) %>% 
  rename(Ano = NU_ANO_CENSO, Uf = SG_UF, codigo_municipio = CO_MUNICIPIO, pdcs = FUN_AI_CAT_0) %>% 
  mutate(pdcs = as.numeric(pdcs))
# Bases percentual de docentes com curso superior
perc <- perc2011 %>% full_join(perc2013) %>% full_join(perc2015) %>% 
  full_join(perc2017) %>% full_join(perc2019)
# Base para o estado escolhido
perc_estado <- perc %>% filter(Uf == "PR")

# Média de horas aula 
# 2011
hras_aula11 <- read_xls("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\hras_diarias\\horas_aula_municipios_2011.xls", skip = 6)
hras_aula11 <- hras_aula11[-1,c(1,3,5,6,7,12)]
names(hras_aula11) <- c("Ano", "Uf", "codigo_municipio", "nivel1", "nivel2", "hras_aula")
hras_aula11 <- hras_aula11 %>% filter(nivel2 == "Municipal" & nivel1 == "Total") %>% 
  mutate(codigo_municipio = as.numeric(str_sub(codigo_municipio, 1, 6)), 
         hras_aula = as.numeric(hras_aula)) %>% select(-nivel1, -nivel2)
# 2013
hras_aula13 <- read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\hras_diarias\\HAD_MUNICIPIOS_2013.xlsx", skip = 8)
hras_aula13 <- hras_aula13 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(PK_COD_MUNICIPIO = as.numeric(str_sub(PK_COD_MUNICIPIO, 1, 6))) %>% 
  select(ano, SIGLA, PK_COD_MUNICIPIO, HAD_F14) %>% 
  rename(Ano = ano, Uf = SIGLA, codigo_municipio = PK_COD_MUNICIPIO, hras_aula = HAD_F14) %>% 
  mutate(hras_aula = as.numeric(hras_aula))
# 2015
hras_aula15 <- read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\hras_diarias\\HAD_MUNICIPIOS_2015.xlsx", skip = 8)
hras_aula15 <- hras_aula15 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(CO_MUNICIPIO = as.numeric(str_sub(CO_MUNICIPIO, 1, 6))) %>% 
  select(NU_ANO_CENSO, SG_UF, CO_MUNICIPIO, HAD_F14) %>% 
  rename(Ano = NU_ANO_CENSO, Uf = SG_UF, codigo_municipio = CO_MUNICIPIO, hras_aula = HAD_F14) %>% 
  mutate(hras_aula = as.numeric(hras_aula))
# 2017
hras_aula17 <- read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\hras_diarias\\HAD_MUNICIPIOS_2017.xlsx", skip = 8)
hras_aula17 <- hras_aula17 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(CO_MUNICIPIO = as.numeric(str_sub(CO_MUNICIPIO, 1, 6))) %>% 
  select(NU_ANO_CENSO, SG_UF, CO_MUNICIPIO, HAD_F14) %>% 
  rename(Ano = NU_ANO_CENSO, Uf = SG_UF, codigo_municipio = CO_MUNICIPIO, hras_aula = HAD_F14) %>% 
  mutate(hras_aula = as.numeric(hras_aula))
# 2019
hras_aula19 <- read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\hras_diarias\\HAD_MUNICIPIOS_2019.xlsx", skip = 8)
hras_aula19 <- hras_aula19 %>% filter(NO_DEPENDENCIA == "Municipal" & NO_CATEGORIA == "Total") %>% 
  mutate(CO_MUNICIPIO = as.numeric(str_sub(CO_MUNICIPIO, 1, 6))) %>% 
  select(NU_ANO_CENSO, SG_UF, CO_MUNICIPIO, FUN_AI_CAT_0) %>% 
  rename(Ano = NU_ANO_CENSO, Uf = SG_UF, codigo_municipio = CO_MUNICIPIO, hras_aula = FUN_AI_CAT_0) %>% 
  mutate(hras_aula = as.numeric(hras_aula))
# Base de média de horas aula
hras_aula <- hras_aula11 %>% full_join(hras_aula13) %>% full_join(hras_aula15) %>% 
  full_join(hras_aula17) %>% full_join(hras_aula19)

# Média de alunos por turma 
# 2011
alunos_turma11 <- read_xls("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\alunos_turma\\alunos_turma_municipios_2011.xls", skip = 8)
alunos_turma11 <- alunos_turma11[-1,c(1,3,4,6,7,12)]
names(alunos_turma11) <- c("Ano", "Uf", "codigo_municipio", "nivel1", "nivel2", "alunos_turma")
alunos_turma11 <- alunos_turma11 %>% filter(nivel2 == "Municipal" & nivel1 == "Total") %>% 
  mutate(codigo_municipio = as.numeric(str_sub(codigo_municipio, 1, 6)), 
         alunos_turma = as.numeric(alunos_turma)) %>% select(-nivel1, -nivel2)
# 2013
alunos_turma13 <- read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\alunos_turma\\ATU_MUNICIPIOS_2013.xlsx", skip = 9)
alunos_turma13 <- alunos_turma13 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(PK_COD_MUNICIPIO = as.numeric(str_sub(PK_COD_MUNICIPIO, 1, 6))) %>% 
  select(ano, SIGLA, PK_COD_MUNICIPIO, ATU_F14) %>% 
  rename(Ano = ano, Uf = SIGLA, codigo_municipio = PK_COD_MUNICIPIO, alunos_turma = ATU_F14) %>% 
  mutate(alunos_turma = as.numeric(alunos_turma))
# 2015
alunos_turma15 <- read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\alunos_turma\\ATU_MUNICIPIOS_2015.xlsx", skip = 9)
alunos_turma15 <- alunos_turma15 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(CO_MUNICIPIO = as.numeric(str_sub(CO_MUNICIPIO, 1, 6))) %>% 
  select(NU_ANO_CENSO, SG_UF, CO_MUNICIPIO, ATU_F14) %>% 
  rename(Ano = NU_ANO_CENSO, Uf = SG_UF, codigo_municipio = CO_MUNICIPIO, alunos_turma = ATU_F14) %>% 
  mutate(alunos_turma = as.numeric(alunos_turma))
# 2017
alunos_turma17 <- read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\alunos_turma\\ATU_MUNICIPIOS_2017.xlsx", skip = 8)
alunos_turma17 <- alunos_turma17 %>% filter(Dependad == "Municipal" & TIPOLOCA == "Total") %>% 
  mutate(CO_MUNICIPIO = as.numeric(str_sub(CO_MUNICIPIO, 1, 6))) %>% 
  select(NU_ANO_CENSO, SG_UF, CO_MUNICIPIO, ATU_F14) %>% 
  rename(Ano = NU_ANO_CENSO, Uf = SG_UF, codigo_municipio = CO_MUNICIPIO, alunos_turma = ATU_F14) %>% 
  mutate(alunos_turma = as.numeric(alunos_turma))
# 2019
alunos_turma19 <- read_xlsx("C:\\Users\\thais\\Downloads\\base_de_dados_inep\\alunos_turma\\ATU_MUNICIPIOS_2019.xlsx", skip = 8)
alunos_turma19 <- alunos_turma19 %>% filter(NO_DEPENDENCIA == "Municipal" & NO_CATEGORIA == "Total") %>% 
  mutate(CO_MUNICIPIO = as.numeric(str_sub(CO_MUNICIPIO, 1, 6))) %>% 
  select(NU_ANO_CENSO, SG_UF, CO_MUNICIPIO, FUN_AI_CAT_0) %>% 
  rename(Ano = NU_ANO_CENSO, Uf = SG_UF, codigo_municipio = CO_MUNICIPIO, alunos_turma = FUN_AI_CAT_0) %>% 
  mutate(alunos_turma = as.numeric(alunos_turma))
# Base de média de alunos por turma
alunos_turma <- alunos_turma11 %>% full_join(alunos_turma13) %>% 
  full_join(alunos_turma15) %>% full_join(alunos_turma17) %>% full_join(alunos_turma19)

# Juntando com a base do estado escolhido 
estado <- parana %>% full_join(perc_estado, by = c("Ano", "Uf", "codigo_municipio")) %>% 
  full_join(alunos_turma, by = c("Ano", "Uf", "codigo_municipio")) %>% 
  full_join(hras_aula, by = c("Ano", "Uf", "codigo_municipio"))
estado <- estado %>% filter(!is.na(municipio))
class_pib <- estado11 %>% ungroup() %>% select(class_pib) %>% distinct() %>% 
  mutate(c_pib1 = ifelse(class_pib == "Pequeno", 1, 0), 
         c_pib2 = ifelse(class_pib == "Médio", 1, 0), 
         c_pib3 = ifelse(class_pib == "Grande", 1, 0), 
         c_pib4 = ifelse(class_pib == "Muito grande", 1, 0))
estado <- estado %>% full_join(class_pib)

# 3. Modelos de regressão múltipla

# Selecionando o ano de 2011
estado11 <- estado %>% filter(Ano == 2011)

# Modelo múltiplo em 2011
modelocompl <- lm(log(valor_observado)~log(inv_pib)+pdcs+factor(class_pop), estado11)
summary(modelocompl)
backward(modelocompl,.05)
modelo11 <- lm(log(valor_observado)~log(inv_pib)+pdcs, estado11)
summary(modelo11)
ad.test(modelo11$residuals)
bptest(modelo11)

# Selecionando 2013
estado13 <- estado %>% filter(Ano == 2013)

# Modelo múltiplo em 2013
modelocompl <- lm(log(valor_observado)~log(inv_pib)+pdcs+hras_aula+alunos_turma+factor(class_pop), estado13)
summary(modelocompl)
backward(modelocompl,.05)
modelo13 <- lm(log(valor_observado)~log(inv_pib)+pdcs+hras_aula, estado13)
summary(modelo13)
ad.test(modelo13$residuals)
bptest(modelo13)

# Selecionando 2015
estado15 <- estado %>% filter(Ano == 2015)

# Modelo múltiplo em 2015
modelocompl <- lm(log(valor_observado)~log(inv_pib)+pdcs+hras_aula+alunos_turma+factor(class_pop), estado15)
result <- summary(modelocompl)
backward(modelocompl,.05)
modelo15 <- lm(log(valor_observado)~log(inv_pib)+pdcs+hras_aula, estado15)
summary(modelo15)
ad.test(modelo15$residuals)
bptest(modelo15)

# Selecionando 2017
estado17 <- estado %>% filter(Ano == 2017)

# Modelo múltiplo em 2017
modelocompl <- lm(log(valor_observado)~log(inv_pib)+pdcs+hras_aula+alunos_turma+factor(class_pop), estado17)
summary(modelocompl)
backward(modelocompl,.05)
modelo17 <- lm(log(valor_observado)~log(inv_pib)+pdcs+hras_aula, estado17)
summary(modelo17)
ad.test(modelo17$residuals)
bptest(modelo17)

# Selecionando 2019
estado19 <- estado %>% filter(Ano == 2019)

# Modelo múltiplo em 2019
x <- log(estado19$inv_pib)-mean(log(estado19$inv_pib))
x2 <- x^2
modelocompl <- lm(log(valor_observado)~(inv_pib)+pdcs+hras_aula+alunos_turma+factor(class_pop), estado19)
result <- summary(modelocompl)
backward(modelocompl,.05)
modelo <- lm(log(valor_observado)~(inv_pib)+pdcs+hras_aula, estado19)
summary(modelo)
ad.test(modelo$residuals)
bptest(modelo)


# Exemplo de tabela para apresentação de resultados dos modelos
ve <- c("Intercepto", "Log(Indicador 4.2/PIB per capita)", 
        "Percentual de docentes com curso superior", 
        "Classificação tamanho populacional (Médio)", 
        "Classificação tamanho populacional (Grande)", 
        "Classificação tamanho populacional (Muito Grande)",
        "Média de horas- aula diária", "Média de alunos por turma")
pvalor <- c("$<$2e-16","1,32e-05", "0,087", "0,666 ", "0,054", "0,912",
            "0,013", "0,773 ")
tabela <- data.frame(ve, pvalor)
names(tabela) <- c("Variáveis explicativas", "P- valor")
kable(result$coefficients, "latex",caption = "Significância dos coeficientes de regressão do modelo para o ano de 2019")%>%   
  kable_styling(full_width = F, font_size = 10, position = 'center')
