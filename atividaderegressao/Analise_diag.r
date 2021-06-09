
#### Diagnóstico do Modelo de Regressão Linear Múltipla
# Arquivo "DadosEUA.txt"
# Descrição dos dados: variáveis referentes a 50 estados norte-americanos
# estado (nome do estado)
# pop (população estimada em julho de 1975)
# percap (renda percapta em 1974 em USD)
# analf (proporção de analfabetos em 1970)
# expvida (expectativa de vida em anos 1969-70)
# crime (taxa de criminalidade por 100.000 habitantes 1976)
# estud (porcentagem de estudantes que concluem o segundo grau 1970)
# ndias (número de dias do ano com temperatura abaixo de zero grau 
#        Celsius na cidade mais importante do estado)
# area (área do estado em milhas quadradas)


#O objetivo do estudo é tentar explicar a variável expvida 
#(nossa variável resposta) usando um modelo de regressão linear múltiplo
#dadas as variáveis explicativas
#percap, analf, crime, estud, ndias e dens, em que
#dens=pop/area.


## Leitura dos dados

library(readr)
DadosEUA <- read_table2("C:\\Users\\gecyn\\OneDrive\\Documentos\\UFBA\\2021.1\\_Métodos Estatísticos para Computação\\Disciplina para Pós\\Comandos no R\\Regressão\\DadosEUA.txt")

#Calculando algumas medidas resumos
summarytools::descr(DadosEUA, stats = c("mean", "sd", "min", "max"), 
                    transpose = TRUE)

library(xtable)
xtable(summarytools::descr(DadosEUA, 
                           stats = c("mean", "sd", 
                                     "min", "max"), 
                           transpose = TRUE))

### Boxplot para a variável "renda per capta" (percap)

ggplot(DadosEUA, aes(x = " ", y=percap)) +
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") +
  labs(x = " ", y = "Renda per capta")


### Boxplot com média

ggplot(DadosEUA, aes(x = " ", y=percap)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, 
               color="red", fill="red") +
  theme(legend.position="none") +
  labs(x = " ", y = "Renda per capta")

# Calculando a densidade dens=pop/area.
DadosEUA$dens <- DadosEUA$pop/DadosEUA$area

# Reordenando as colunas para facilitar a visualização

library(sjmisc)
library(dplyr)

DadosEUA <- DadosEUA %>%
  move_columns(c(expvida, dens), .before = 1) %>%
  move_columns(c(pop, estado), .after = area)

head(DadosEUA)
#xtable(head(DadosEUA))

### Correlação
#Algumas formas de apresentar a matriz de correlação

GGally::ggcorr(DadosEUA[,-c(8, 9, 10)], label=T, label_size = 3,
               low = "#F21A00",
               mid = "#EEEEEE",
               high = "#3B9AB2")

GGally::ggpairs(DadosEUA[,-c(8, 9, 10)],
                lower = list(continuous = "smooth")) 


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, aes=(accuracy = 0.01),
                fill="green", color="blue", ...)
  p
}

GGally::ggpairs(DadosEUA[,-c(8, 9, 10)],
                upper = list(continuous = "cor"),
                lower = list(continuous = my_fn),
                axisLabels="none")


# Análise de regressão
# O modelo:

m1 <- lm(expvida ~ dens + percap + analf + crime + estud + ndias, 
         data = DadosEUA)

# Resultado do modelo:
summary(m1)
#xtable(summary(m1))

#Tabela de análise de variância (ANOVA) para o modelo ajustado
anova(m1)
#xtable(anova(m1))

# Formatando os resultados encontrados, podemos construir 
# a tabela a seguir
SQReg <- sum(anova(m1)$"Sum Sq"[1:6])
glReg <- sum(anova(m1)$"Df"[1:6])

MyAnova <- function(modelo){
  m1 <- modelo
  SQReg <- round(sum(anova(m1)$"Sum Sq"[1:6]), 4)
  glReg <- sum(anova(m1)$"Df"[1:6])
  SQRes <- round(anova(m1)$"Sum Sq"[7], 4)
  glRes <- anova(m1)$"Df"[7]
  SQTotal <- round(SQReg + SQRes, 4)
  glTotal <- glReg + glRes
  QMReg <- round(SQReg/glReg, 4)
  QMRes <- round(SQRes/glRes, 4)
  MyF <- round(QMReg/QMRes, 4)
  vpF <- ifelse(pf(MyF, glReg, glRes, lower.tail = F) < 0.0001, "<0.001", 
                pf(MyF, glReg, glRes, lower.tail = F))
  ncolunas <- c("Fonte de Variação", "SQ", "gl", "F", "valor p") 
  Tanova <- data.frame(FV = c("Regressão",
                              "Resíduos",
                              "Total"),
                       SQ = c(SQReg, SQRes, SQTotal),
                       gl = c(glReg, glRes, glTotal),
                       QM = c(QMReg, QMRes, " "),
                       Est.F = c(MyF, " ", " "),
                       valor.p = c(vpF, " ", " ")
                       )
  Tanova
}

# Nova formatação (de acordo com a tabela vista em aula)
MyAnova(m1)
#xtable(MyAnova(m1))

# Valores preditos para expectativa de vida (expvida)
fitted(m1)

# Resíduos do modelo ajustado e algumas medidas de 
# diagnóstico de influência
# Residuos ordinarios
residuals(m1)

# Residuos padronizados
rstandard(m1)

# Residuos estudentizados
rstudent(m1)

# Medidas diagnósticas de observações influentes
head(lm.influence(m1))

m_infl <- influence.measures(m1)

names(m_infl)

head(m_infl$infmat)

#dfb.1_  dfb.dens  dfb.prcp  dfb.anlf  dfb.estd    dfb.ndis
# dffit cov.r   cook.d    hat

# leverages (hii)
m_infl$infmat[,11]
hatvalues(m1)

#Distância de Cook
m_infl$infmat[,10]
cooks.distance(m1)

#DFFITS
m_infl$infmat[,8]
dffits(m1)

#COVRATIO
m_infl$infmat[,9]
covratio(m1)

## Avaliação gráfica: Gráficos "padrões"

par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))

plot(m1, which = 4)
plot(m1, which = 5)
# Observação: As linhas tracejadas no gráfico são 
# limites (fixados em 0,5 e 1, por padrão)
# para avaliar os pontos em relação à distância de Cook.

### Usando outras funções/pacotes para a análise gráfica 
#  do modelo:


library(olsrr)

ols_plot_cooksd_bar(m1)
ols_plot_cooksd_chart(m1)

# Observação: Pontos identificados acima da linha vermelha 
# (limite dado por Threshold: 0.08 neste modelo) são potenciais 
# outliers levando em consideração a distância de Cook.

ols_plot_dfbetas(m1)
# Observação: Pontos identificados acima ou abaixo da linha 
# vermelha (limite dado por Threshold: 0.28 neste modelo)
# são potenciais pontos influentes levando em consideração os DFBETAS.

ols_plot_dffits(m1)
# Observação: Pontos identificados acima ou abaixo da linha 
# vermelha (limite dado por Threshold: 0.75 neste modelo) são 
# potenciais pontos influentes levando em consideração os DFFITS.

ols_plot_resid_stud(m1)

ols_plot_resid_stand(m1)

ols_plot_resid_lev(m1)
# Observação: Pontos identificados à direita da linha vermelha 
# (limite dado por Threshold: 0.28 neste modelo) ou acima
# ou abaixo das linhas vermelhas horizontais são classificados 
# conforme a legenda à direita da figura.

ols_plot_resid_stud_fit(m1)

ols_plot_resid_pot(m1)

###

### Gráfico da regressão parcial

car::avPlots(m1)
#Nessa figura, no primeiro gráfico no canto superior esquerdo, 
#temos que a expressão expvida|others significa que foi ajustado
#o modelo:
#  m2 <- lm(expvida ~ dens + percap + analf + crime + estud + ndias,
#           data = DadosEUA)
#e a expressão dens | others significa que foi ajustado o modelo:
#  m3 <- lm(dens ~ percap + analf + crime + estud + dias,
#           data = DadosEUA)

#No referido gráfico são plotados os pontos representados pelos 
#resíduos dos dois modelos ajustados - res(m3) versus res(m2). 
#Se os pontos estiverem em torno da reta em
#azul é um indicativo de que dens deve ser incluída no modelo.


### Gráfico dos resíduos parciais

car::crPlots(lm(expvida ~ dens + percap + analf + crime 
                + estud + ndias, data = DadosEUA))
#Nessa figura, o desejável é que os pontos estejam em
#torno da linha tracejada azul ou que a linha cheia rosa seja coincidente
#com a linha tracejada azul.
