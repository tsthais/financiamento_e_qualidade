<h1> 📚 Financiaamento e qualidade da educação no Brasil </h1>

#

<div>
<h3 align: "justify">➡ Sobre o projeto...</h3>

Este foi o meu Trabalho de Conclusão de Curso para obtenção do título de bacharel em Estatística pela Universidade de Brasíia. 
Além disso, foi um projeto que começou do meu interesse em investigar sobre diversos componenetes da educação pública no Brasil, dentre eles o financiamento.
Esse interesse começou no estágio que fiz no Fundo Nacional de Desenvolvimento da Educação, em que pude ter a dimensão de tantas ferramentas que mantêm o sistema educacional
brasileiro funcionando e, como Estatística, tive curiosidade em estudar os dados resultantes dessas ferramentas. 


Então, o meu trabalho consistiu na análise da relação entre investimento estudantil e qualidade dos anos iniciais do ensino fundamental dos municípios brasileiros.
O método que utilizei foi o de Análise de Regressão Múltipla para ajustar um modelo entre investimento e qualidade da educação.
</div>

#

<h3>➡ Um pouquinho sobre o problema </h3>

- A educação desempenha papel fundamental na formação social e humana do indivíduo, na Consistituição de 1988 foi declarada como direito do cidadão e dever do Estado;
sendo responsabilidade deste oferecer um ensino gratuito e de qualidade;

- Para oferecer um ensino qualificado para a população seria necessário traçar estratégias que depedem de um sistema de investimento;

- Como exemplo disso, o financiamento foi objeto de Emendas Constitucionais que apresentavam mecanismos que subvinculavam recursos para a manutenção da educação básica,
como o Fundo de Manutenção e Desenvolvimento do Ensino Fundamental e Valorização do Magistério (Fundef) e o Fundo de Manutenção e Desenvolvimento da Educação Básica e de 
Valorização dos Profissionais da Educação (Fundeb);

- Com os avanços no campo educacional surgiu a necessidade de se avaliar este sistema. Uma forma de se avaliar a qualidade da educação é por meio do o Índice de Desenvolvimento
da Educação Básica (Ideb). Esse indicador foi elaborado como forma de avaliar a qualidade do ensino no País, sintetizando informações de fluxo escolar e de avaliações nas
proficiências de português e matemática;

- A pretensão do meu estudo foi contribuir na discussão do financiamento e da qualidade educacional, buscando entender como os avanços nos investimentos para a educação brasileira tiveram 
impacto na qualidade de ensino dos anos iniciais do ensino fundamental.


#

<h3> ➡ Objetivos do trabalho </h3>

- Analisar a relação entre financiamento e a qualidade dos anos iniciais do ensino fundamental nos municípios brasileiros;
- Descrever analiticamente o investimento estudantil e o Ideb no País;
- Propor um modelo que descreva a relação entre investimento e qualidade da educação;
- Levantar variáveis de controle para o modelo proposto;
- Abordar a técnica de Análise de Regressão Múltipla e aplicá- la ao problema em estudo.


# 

<h3> ➡ Análise de Regressão Múltipla </h3>

- Essa técnica é empregada para analisar a relação de uma variável dependente (resposta) com um conjunto de variáveis independentes (explicativas);
- Geralmente esse procedimento é usado em problemas de pesquisa que pretende explicar uma variável, examinando a contribuição individual das variáveis preditoras à variável resposta;
-  A relação linear entre a variável resposta e as variáveis explicativas pode ser obtida através da equação:

<h4 align="center">
<a href="https://www.codecogs.com/eqnedit.php?latex=Y_i\&space;=\&space;\beta_0\&space;&plus;\&space;\beta_1&space;X_{i1}\&space;&plus;\&space;\beta_2&space;X_{i2}\&space;&plus;\&space;\dots\&space;&plus;\&space;\beta_{p-1}&space;X_{i,p-1}\&space;&plus;\&space;\varepsilon_i&space;\qquad&space;\quad&space;\forall&space;i&space;\in&space;[1,n]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Y_i\&space;=\&space;\beta_0\&space;&plus;\&space;\beta_1&space;X_{i1}\&space;&plus;\&space;\beta_2&space;X_{i2}\&space;&plus;\&space;\dots\&space;&plus;\&space;\beta_{p-1}&space;X_{i,p-1}\&space;&plus;\&space;\varepsilon_i&space;\qquad&space;\quad&space;\forall&space;i&space;\in&space;[1,n]" title="Y_i\ =\ \beta_0\ +\ \beta_1 X_{i1}\ +\ \beta_2 X_{i2}\ +\ \dots\ +\ \beta_{p-1} X_{i,p-1}\ +\ \varepsilon_i \qquad \quad \forall i \in [1,n]" /></a>
</h4>

- Onde <a href="https://www.codecogs.com/eqnedit.php?latex=Y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Y" title="Y" /></a> representa a variável resposta e os parâmetros
<a href="https://www.codecogs.com/eqnedit.php?latex=\beta_j" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\beta_j" title="\beta_j" /></a>,
<a href="https://www.codecogs.com/eqnedit.php?latex=j=\&space;1,\dots,p-1" target="_blank"><img src="https://latex.codecogs.com/gif.latex?j=\&space;1,\dots,p-1" title="j=\ 1,\dots,p-1" /></a>, representam os coeficientes de regressão.
As variáveis explicativas são denotadas por <a href="https://www.codecogs.com/eqnedit.php?latex=X_{ij}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?X_{ij}" title="X_{ij}" /></a>, 
<a href="https://www.codecogs.com/eqnedit.php?latex=i=\&space;1,\dots,n" target="_blank"><img src="https://latex.codecogs.com/gif.latex?i=\&space;1,\dots,n" title="i=\ 1,\dots,n" /></a> e
<a href="https://www.codecogs.com/eqnedit.php?latex=j=\&space;1,\dots,p-1" target="_blank"><img src="https://latex.codecogs.com/gif.latex?j=\&space;1,\dots,p-1" title="j=\ 1,\dots,p-1" /></a>.
Também pode ser encontrada nessa equação o erro aleatório, <a href="https://www.codecogs.com/eqnedit.php?latex=\varepsilon_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\varepsilon_i" title="\varepsilon_i" /></a>,
<a href="https://www.codecogs.com/eqnedit.php?latex=i=\&space;1,\dots,n" target="_blank"><img src="https://latex.codecogs.com/gif.latex?i=\&space;1,\dots,n" title="i=\ 1,\dots,n" /></a> e
; 

- O diagnóstico do modelo é feito através dos testes de normalidade e homocedastidade dos resíduos. Neste trabalho, foram utilizados os teste de normalidade de Anderson- Darling e Breush- Pagan;
- Para saber se o modelo era significativo foram utilizados os testes de ausência de regressão e de significância dos coeficientes de regressão;
- O coeficiente de determinação ajustado (<a href="https://www.codecogs.com/eqnedit.php?latex=R^2" target="_blank"><img src="https://latex.codecogs.com/gif.latex?R^2" title="R^2" /></a>)
foi utilizado para medir a redução da variação da resposta quando se associam as variáveis explicativas ao modelo;
- Para selecionar novas variáveis para o modelo foi utilizado o método de Forward.

# 

<h3>➡ Alguns resultados da Análise Descritiva </h3>

A partir da análise descritiva foi possível ter a dimensão de como são o investimento e a qualidade educacional no País. Aqui, brevemente, vou apresentar alguns resultados que foram obtidos com esta análise.

<h4>◻ Qualidade da educação </h4>

Ao analisar abaixo é possível verificar que os resultados do Ideb dos anos iniciais do ensino fundamental foram positivos no País
quando se percebe sua evolução de 2005 a 2019. A média do indicador cresceu ao longo desse período, ampliando assim o limite máximo. 
O intervalo da concentração dos resultados do Ideb nos municípios também evoluiu no período observado. A menos do ano de 2013, o índice apresentou vários
*outliers*, demonstrando que há vários municípios com diferenças elevadas tanto no valor máximo quanto no mínimo.

<p align="center">
  <img src="https://github.com/tsthais/financiamento_e_qualidade/blob/main/analise_descritiva_financiamento/im1.png" />
</p>

Durante o período analisado na próxima figura, as médias do Ideb das regiões Norte e Nordeste sempre foram menores do que a média brasileira. 
A região Nordeste apresentou médias menores do que a região Norte de 2005 a 2013 e a partir de 2015 ela conseguiu ultrapassar a média da região Norte. 
Em 2005, a média dos municípios brasileiros era 34,20% maior do que a média do Nordeste. Essa diferença diminuiu em 2013, 23,19% e, ficou ainda menor em 
2019, 13,37%.  A região Norte foi a única que não conseguiu atingir média maior ou igual a 5,00 no último ano da análise, 2019. Mesmo assim, é notável a evolução
do indicador de qualidade na região durante os anos analisados: em 2005, a média dos municípios brasileiros era 22,72% maior do que a média municípios dessa região
e em 2019, diminuiu para 20,59%.

<p align="center">
  <img src="https://github.com/tsthais/financiamento_e_qualidade/blob/main/analise_descritiva_financiamento/im2.png" />
</p>

O mapa abaixo ilustra a comparação do Ideb dos municípios brasileiros nos anos de 2005, 2013 e 2019. Por meio desta figura é possível observar que grande parte 
dos municípios, cerca de 27,26%, tiveram o Ideb entre 2,50 e 3,50. Essa escala foi predominante nas regiões Norte, Nordeste e Centro- Oeste. Ainda entre as 
regiões Sul e Sudeste se mostram alguns pontos de municípios que obtiveram o Ideb entre 4,50 e 5,50, o que representa apenas 11,01% dos municípios brasileiros.

Em 2013, a maior parte dos municípios obteve o Ideb entre 4,50 e 5,50, cerca de 26,81% . Em todo o País encontrou- se municípios com o indicador com valores entre 3,50 e 
4,50, cerca de 23,28%. Também, neste ano, muitos municípios tiveram o resultado entre 5,50 e 6,50 (25,74%), a maior parte dos municípios que apresentaram os valores entre 
esse intervalo foram das regiões Sul e Sudeste. 

No último ano apresentado no mapa (2019), cerca de 26,42% dos municípios do Brasil tiveram o indicador de qualidade entre 4,50 e 5,50. Mesmo assim, a maioria dos municípios 
(34,92%) tiveram o Ideb entre 5,50 e 6,50. Ainda é possível observar que alguns municípios tiveram o Ideb entre 6,50 e 7,50, 17,91%, que estão mais visíveis nas regiões Sul 
e Sudeste. Somente 1,42% dos municípios tiveram o Ideb maior do que 7,50 e alguns destes municípios estão na região Nordeste.

<p align="center">
  <img src="https://github.com/tsthais/financiamento_e_qualidade/blob/main/analise_descritiva_financiamento/im3.png" />
</p>

<h4>◻ Investimento estudantil </h4>

O investimento estudantil por aluno do ensino fundamental (Indicador 4.2) teve média, de 2008 a 2019, igual a R$ 6.093. 
A tabela abaixo apresenta as medidas descritivas do indicador de investimento por ano entre o período de 2008 e 2019.  
Mesmo que incomum, em 2009, 2010 e 2016 o valor mínimo de investimento por aluno nessa etapa de ensino foi menor do que R$ 1.000,00. 
Em 2016, o menor valor do indicador foi igual a R$ 4,66, este valor é considerado um *outlier*, pode ser alguma inconsistência na base 
de dados ou erro de digitação. Mesmo assim, medidas como média e mediana aumentaram ao longo destes anos.

<p align="center">
  <img src="https://github.com/tsthais/financiamento_e_qualidade/blob/main/analise_descritiva_financiamento/im4.png" />
</p>

A figura abaixo ilustra o comportamento deste indicador no Brasil ao longo dos anos estudados. *Outliers* foram encontrados em quase todos os anos, 
além disto em 2013 o maior valor observado de investimento por aluno ultrapassou R$ 600.000, que corresponde 100 vezes a média nacional (R$6.077,07) deste ano, 
de forma que foram consideradas prováveis inconsistências no valor encontrado. Vale ressaltar que essa discrepância levou o coeficiente de variação atingir 160,47%. 

<p align="center">
  <img src="https://github.com/tsthais/financiamento_e_qualidade/blob/main/analise_descritiva_financiamento/im5.png" />
</p>

A partir da Figura 10 é possível verificar que a média do indicador de investimento das regiões Norte e Nordeste foi menor do que a média brasileira para os anos estudados.
Ainda a respeito desta figura, as regiões Centro- Oeste, Sudeste e Sul apresentaram médias maiores do que a do Brasil para os anos estudados. 

<p align="center">
  <img src="https://github.com/tsthais/financiamento_e_qualidade/blob/main/analise_descritiva_financiamento/im6.png" />
</p>

A próxima figura compara a distribuição de investimento educacional por aluno do ensino fundamental dos municípios brasileiros entre os anos de 2009, 2013 e 2019. 
Em 2009, grande parte dos municípios (44,43%) tinham o valor de investimento por aluno entre 1 e 3 mil reais e 40,56% tinham entre 3 e 5 mil reais. Apenas 0,07% dos 
  municípios do País tinham o essa quantia menor do que 1 mil reais e cerca de 1% maior do que 15 mil.
  
  <p align="center">
  <img src="https://github.com/tsthais/financiamento_e_qualidade/blob/main/analise_descritiva_financiamento/im7.png" />
</p>


A Figura 17 ilustra a relação entre o Ideb e o indicador de investimento estudantil por aluno do ensino fundamental nos anos de 2009, 2013 e 2019. 
Para a elaboração desse gráfico foram considerados 90\% dos dados de investimento estudantil, já que essa variável apresentou inúmeros *outliers*, 
como pode ser visto na Figura 9. Ainda que existam muitas observações é possível distinguir que existe uma relação positiva entre o Ideb e o investimento 
estudantil por aluno do ensino fundamental nestes anos. Em 2009, embora exista uma relação positiva, essa relação é considerada fraca (Coeficiente de Pearson = 0,254).
Em 2013, a relação entre o Ideb e o valor investido por aluno do ensino fundamental é considerada moderada (Coeficiente de Pearson = 0,505). É possível notar uma 
linearidade entre as variáveis, de forma que conforme o valor de investimento por aluno aumenta o Ideb também aumenta. Assim como em 2009, em 2019 a relação entre as 
variáveis também foi considerada fraca (Coeficiente de Pearson = 0,370).

  <p align="center">
  <img src="https://github.com/tsthais/financiamento_e_qualidade/blob/main/analise_descritiva_financiamento/im8.png" />
</p>

#

<h3>➡ Modelagem </h3>

Para os modelos apresentados nesta seção, o Ideb foi considerada variável resposta (<a href="https://www.codecogs.com/eqnedit.php?latex=y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?y" title="y" /></a>) e 
as variáveis explicativas estudadas foram a razão entre o investimento estudantil por aluno do ensino fundamental e o PIB per capita municipal (<a href="https://www.codecogs.com/eqnedit.php?latex=x_1" target="_blank"><img src="https://latex.codecogs.com/gif.latex?x_1" title="x_1" /></a>), 
o percentual de docentes com curso supeerior (<a href="https://www.codecogs.com/eqnedit.php?latex=x_2" target="_blank"><img src="https://latex.codecogs.com/gif.latex?x_2" title="x_2" /></a>), 
a média de horas- aula diária (<a href="https://www.codecogs.com/eqnedit.php?latex=x_3" target="_blank"><img src="https://latex.codecogs.com/gif.latex?x_3" title="x_3" /></a>), 
a média de alunos por turma (<a href="https://www.codecogs.com/eqnedit.php?latex=x_4" target="_blank"><img src="https://latex.codecogs.com/gif.latex?x_4" title="x_4" /></a>) e a estimativa do tamanho populacional 
(<a href="https://www.codecogs.com/eqnedit.php?latex=x_5" target="_blank"><img src="https://latex.codecogs.com/gif.latex?x_5" title="x_5" /></a>).

Devido a heterogeneidade de informações, que pode ser percebida na Figura 17, a relação entre qualidade e investimento por aluno pode ficar comprometida. Dessa forma, este estudo se restringe a entender o impacto do investimento por aluno na qualidade do ensino fundamental apenas nos municípios do estado do Paraná.

A modelagem foi feita para cada ano, considerando que os dados, tanto de qualidade quanto de financiamento, são séries temporais e as observações apresentam uma estrutura de dependência. 

Inicialmente, no ano de 2011, foi considerado somente a razão entre investimento estudantil e PIB per capita como variável explicativa.  A correlação entre o Ideb e essa variável foi de - 0,191, que indica uma correlação negativa e fraca. No modelo ajustado para este ano somente as variáveis percentual de docentes com curso superior e a razão entre investimento e o PIB per capita foram significativas para o modelo, pelo método de seleção de Backward.
Em conjunto, as duas variáveis explicativas conseguem explicar 6,498% do Ideb. Mantendo- se constante o percentual de docentes com curso superior, há uma redução em 0,049% na média do Ideb se a razão entre o investimento estudantil por aluno do ensino fundamental e o PIB per capita aumentar em 1%.

<a href="https://www.codecogs.com/eqnedit.php?latex=E(Y)\&space;=\&space;Exp(1,508\&space;&plus;\&space;0,0001\&space;\cdot\&space;X_2)\&space;X_1^{-0,049}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?E(Y)\&space;=\&space;Exp(1,508\&space;&plus;\&space;0,0001\&space;\cdot\&space;X_2)\&space;X_1^{-0,049}" title="E(Y)\ =\ Exp(1,508\ +\ 0,0001\ \cdot\ X_2)\ X_1^{-0,049}" /></a>

Em 2013, a correlação entre o Ideb e a razão entre o indicador 4.2 e o PIB per capita foi igual a -0,265. Ajustando o modelo com outras variáveis explicativas, obteve- se o resultado de que pelo menos uma variável explicativa é significativa para o modelo (p- valor <0,001).
As variáveis média de horas- aula diária e o percentual de docentes com curso superior foram consideradas significantes para o modelo, além da razão entre investimento e o PIB.
Reunidas elas conseguem explicar 9,428% da variação do Ideb. Neste modelo, mantendo as variáveis média de horas- aula diária e o percentual de docentes com curso superior 
constantes, a variável que remete ao investimento reduz em cerca 0,068% da média do Ideb toda vez que ela aumenta 1%.

<a href="https://www.codecogs.com/eqnedit.php?latex=E(Y)\&space;=\&space;Exp(1,400\&space;&plus;\&space;0,001\&space;\cdot\&space;X_2\&space;&plus;\&space;0,024\&space;\cdot\&space;X_3)\&space;\cdot\&space;X_1^{-0,068}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?E(Y)\&space;=\&space;Exp(1,400\&space;&plus;\&space;0,001\&space;\cdot\&space;X_2\&space;&plus;\&space;0,024\&space;\cdot\&space;X_3)\&space;\cdot\&space;X_1^{-0,068}" title="E(Y)\ =\ Exp(1,400\ +\ 0,001\ \cdot\ X_2\ +\ 0,024\ \cdot\ X_3)\ \cdot\ X_1^{-0,068}" /></a>

No ano de 2015, a correlação entre o Ideb e a razão entre o investimento estudantil por aluno e o PIB per capita foi igual a -0,306. Usando o método de seleção de Backward, as variáveis selecionadas foram a de investimento, o percentual de docentes com curso superior e a média de horas- aula diária.
Com as variáveis selecionadas, todos os coeficientes do modelo foram significativos. As variáveis explicativas conseguiram explicar 14,66\% da variação do Ideb. Ainda assim, variando em 1% a razão entre investimento e o PIB, o Ideb reduz em cerca de 0,074%, mantendo as outras variáveis do modelo constantes.

<a href="https://www.codecogs.com/eqnedit.php?latex=E(Y)\&space;=\&space;Exp(1,385\&space;&plus;\&space;0,002\&space;\cdot\&space;X_2\&space;&plus;\&space;0,025\&space;\cdot\&space;X_3)\&space;\cdot\&space;X_1^{-0,074}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?E(Y)\&space;=\&space;Exp(1,385\&space;&plus;\&space;0,002\&space;\cdot\&space;X_2\&space;&plus;\&space;0,025\&space;\cdot\&space;X_3)\&space;\cdot\&space;X_1^{-0,074}" title="E(Y)\ =\ Exp(1,385\ +\ 0,002\ \cdot\ X_2\ +\ 0,025\ \cdot\ X_3)\ \cdot\ X_1^{-0,074}" /></a>

No ano de 2017, a correlação entre a razão entre o investimento estudantil por aluno e o PIB per capita municipal e o Ideb foi igual a - 0,222, isto é, a correlação entre essas variáveis é fraca.
Aplicando o método de seleção de Backward, têm- se que as variáveis que foram consideradas significativas para o modelo foram a razão entre o investimento e o PIB, a média de horas- aula diária e o percentual de docentes com curso superior.
Em conjunto essas variáveis conseguem explicar 11,08% da variação do Ideb. Além disso, com a média de horas- aula diária e o percentual de docentes com curso superior constantes, a cada aumento de 1% na razão entre investimento estudantil por aluno do ensino fundamental e o PIB há a redução de 0,053% na média do Ideb.

<a href="https://www.codecogs.com/eqnedit.php?latex=E(Y)\&space;=\&space;Exp(1,409\&space;&plus;\&space;0,002\&space;\cdot\&space;X_2\&space;&plus;\&space;0,035\&space;\cdot\&space;X_3)\&space;\cdot&space;X_1^{-0,053}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?E(Y)\&space;=\&space;Exp(1,409\&space;&plus;\&space;0,002\&space;\cdot\&space;X_2\&space;&plus;\&space;0,035\&space;\cdot\&space;X_3)\&space;\cdot&space;X_1^{-0,053}" title="E(Y)\ =\ Exp(1,409\ +\ 0,002\ \cdot\ X_2\ +\ 0,035\ \cdot\ X_3)\ \cdot X_1^{-0,053}" /></a>

Em 2019, a correlação entre o Ideb e a razão entre investimento estudantil e o PIB municipal foi de -0,29, indicando uma correlação fraca e negativa. Pelo método de seleção de Backward,as variáveis que tiveram mais relevância para o modelo foram a de investimento, o percentual de docentes com curso superior e a média de horas- aula diária.
Esse foi o único modelo em que se considerou a razão entre investimento e o PIB sem transformação logarítmica, visto que se a variável estivesse dessa forma os pressupostos dos resíduos não seriam aceitos.
Em conjunto, as variáveis preditoras conseguem explicar cerca de 11,45% da variação do Ideb. Neste modelo, quando a média de horas- aula diária e o percentual de docentes com curso superior são constantes, a cada acréscimo na razão entre o investimento por aluno do ensino fundamental e o PIB per capita há a redução de 24,06% do Ideb. 

<a href="https://www.codecogs.com/eqnedit.php?latex=E(Y)\&space;=\&space;Exp(1,696\&space;-\&space;0,275\&space;\cdot\&space;X_1\&space;&plus;\&space;0,001\&space;\cdot\&space;X_2\&space;&plus;\&space;0,023\&space;\cdot\&space;X_3)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?E(Y)\&space;=\&space;Exp(1,696\&space;-\&space;0,275\&space;\cdot\&space;X_1\&space;&plus;\&space;0,001\&space;\cdot\&space;X_2\&space;&plus;\&space;0,023\&space;\cdot\&space;X_3)" title="E(Y)\ =\ Exp(1,696\ -\ 0,275\ \cdot\ X_1\ +\ 0,001\ \cdot\ X_2\ +\ 0,023\ \cdot\ X_3)" /></a>

#

<h3>➡ Considerações Finais </h3>

- Embora fosse positiva, a correlação entre o Ideb e o investimento estudantil por aluno do ensino fundamental foi fraca;
- Em razão da heterogeneidade dos dados, o modelo foi ajustado somente para o Paraná. Para os anos em que foi feita a modelagem a correlação entre as variáveis também foi considerada fraca, além de ser negativa, indicando que o Ideb segue relação contraposta à razão entre o investimento educacional e o PIB per capita municipal;
- A razão entre o investimento e PIB per capita foi significativa como variável explicativa do Ideb para todos os anos em que foi ajustado o modelo. Ela teve um impacto negativo no modelo, de forma que toda vez que tivesse um acréscimo de uma unidade na razão entre investimento estudantil e PIB per capita haveria uma redução no Ideb;
- Os gastos com educação nos anos iniciais do ensino fundamental, na maioria dos municípios do Estado do Paraná, apresentaram ineficiência moderada e concluiram que fossem feitos esforços, para que a alocação dos gastos públicos fosse melhor, a fim de melhorar os níveis de eficiência dos municípios;
- Os achados deste trabalho ressaltam que não apenas mais investimento por aluno melhoraria os resultados de qualidade educacional.



















