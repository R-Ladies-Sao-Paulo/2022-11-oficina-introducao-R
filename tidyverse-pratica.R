################################################################################
################################################################################
##########
########## NUMERO E SUCESSO DE CANDIDATURAS MILITARES AO LEGISLATIVO NO BRASIL     
##########
########## Ana Carolina Moreno e Lucas Gelape
##########

# ANALISE: Evolucao da quantidade de militares e policiais no Legislativo
# INSPIRACAO: Reportagem do G1 "Numero de policiais e militares no 
#             Legislativo e quatro vezes maior do que o de 2014"
# https://g1.globo.com/politica/eleicoes/2018/eleicao-em-numeros/noticia/2018/10/08/numero-de-policiais-e-militares-no-legislativo-e-quatro-vezes-maior-do-que-o-de-2014.ghtml

# Em nossa atividades, vamos tentar replicar a materia apontada acima, 
# utilizando dados das eleicoes de 2022 e 2018

# Limpar o ambiente (se necessario)
rm(list=ls())

# Pacotes
# Instalar a versao em desenvolvimento do electionsBR, ja e compativel com 2022:
# devtools::install_github("silvadenisson/electionsBR")
library(electionsBR) # autoria: Fernando Meireles, Denisson Silva e Beatriz Costa
library(dplyr) # O dplyr ja esta instalado se voce instalou o tidyverse
library(ggplot2)

################################################################################

##### (1) IMPORTACAO DE DADOS: abrindo o nosso arquivo!

# Temos varias opcoes de como abrir arquivos no R. Nesse caso, como se tratam de 
# dados salvos em .csv (Repositorio TSE: https://dadosabertos.tse.jus.br/),
# vamos utilizar a funcao read.csv2
cand_2022 <- read.csv2("data/consulta_cand_2022_BRASIL.csv")

# Neste bootcamp, vamos trabalhar somente com dados em formato .csv. 
# Porém, se você tiver necessidade de abrir arquivos em outro formato, 
# leia [este capítulo do livro de Fernando Meireles e Denisson Silva](http://electionsbr.com/livro/importacao.html).

# Para nao ser necessario baixar os dados manualmente do TSE, a comunidade 
# #rstats desenvolveu um pacote, o electionsBR, que permite fazer
# isto utilizando somente uma funcao
cand_2022 <- candidate_fed(2022)

# Para entender as funcoes do pacote electionsBR:
# https://cran.r-project.org/web/packages/electionsBR/electionsBR.pdf

################################################################################

##### (2) INSPECAO DO BANCO DE DADOS: qual a "cara" dos nossos dados?

# Apos a importacao de dados, vamos usar o pacote dplyr para analisa-los

# Uma boa pratica ao importar um banco de dados que nao conhecemos e inspecionar
# algumas das suas caracteristicas. Podemos ver algumas delas no nosso painel 
# do ambiente. Mas tambem podemos fazer isso com algumas funcoes
View(cand_2022)
head(cand_2022)
glimpse(cand_2022)

# Nessa inspecao ja conseguimos notar alguns pontos importantes:
# - O banco de dados tem 29.314 obs (observacoes, ou linhas)
# - Conseguimos ver o tipo das variaveis
# - Conseguimos ver os nomes das variaveis, em especial daquelas com as 
#   quais iremos trabalhar: DESCRICAO_CARGO, DESCRICAO_OCUPACAO etc
# - Observamos que algumas variaveis texto estao com problemas de leitura

# Esse problema de abertura e comum em dados brasileiros, devido a caracteristicas
# do portugues. Isso se da pelo "encoding" do banco de dados, que necessita ser
# especificado em sua abertura. Como descobrimos qual o encoding correto? 
# Se a pessoa que produziu o banco de dados seguiu os melhores procedimentos 
# para tanto, existirá um arquivo "leia-me" com essas informações. Caso o banco 
# não venha acompanhado de um arquivo desse tipo, podemos abrir o arquivo e 
# verificar tais informações. No nosso caso, a leitura do banco sob a codificação 
# `"latin1"`, que permite a [leitura do alfabeto latino](https://pt.wikipedia.org/wiki/ISO/IEC_8859-1). 
# Outras opções que podem ser tentadas são a `"windows-1252"` ou a `"utf-8"`.
cand_2022 <- read.csv2("data/consulta_cand_2022_BRASIL.csv",
                   fileEncoding = "latin1")

glimpse(cand_2022)

################################################################################

##### (3) ANALISE DOS DADOS

# Agora que temos a nossa base importada de forma correta,
# podemos iniciar as analises!

# Ao inicio, discutimos que nao precisamos de todos as pessoas, so as 
# que concorrem a cargos no LEGISLATIVO (deputados/as federais, estaduais, 
# distritais e senadores/as). Entao vamos FILTRAR a base:
cand_leg_2022 <- cand_2022 %>%
  filter(DS_CARGO == "DEPUTADO ESTADUAL"
         | DS_CARGO == "DEPUTADO FEDERAL"
         | DS_CARGO == "DEPUTADO DISTRITAL"
         | DS_CARGO == "SENADOR")

# Agora que sabemos disso, podemos abrir a base direto do site do TSE mas com esse filtro
# Antes, vamos "zerar" nosso Environment, com uma funcao que remove todos os objetos criados:
rm(list = ls())

# OBS: A outra alternativa pra fazer isso eh clicar no botao da "vassourinha" em Environment!

# Em seguida, vamos unir os dois codigos acima em apenas um:
cand_leg_2022 <- read.csv2("data/consulta_cand_2022_BRASIL.csv",
                           fileEncoding = "latin1") %>%
  filter(DS_CARGO == "DEPUTADO ESTADUAL"
         | DS_CARGO == "DEPUTADO FEDERAL"
         | DS_CARGO == "DEPUTADO DISTRITAL"
         | DS_CARGO == "SENADOR")

# O simbolo | quer dizer OU. Com ele, eu digo pro computador me devolver somente
# as linhas em que a ocupacao da pessoa for QUALQUER UMA dessas opcoes acima.

# Agora precisamos achar a coluna que vai nos dar a informacao:
# Quantas pessoas sao policiais ou militares?
# Essa coluna/variavel se chama DESCRICAO_OCUPACAO
# Vamos usar a funcao DISTINCT para descobrir quais sao as possiveis ocupacoes existentes na base:
ocupacoes2022 <- cleg2022 %>%
  distinct(DS_CARGO)

# Vejam que sao 213 ocupacoes diferentes, incluindo "deputado" e "senador".
# Pra facilitar nossa busca, vamos organizar por ordem alfabetica:
ocupacoes2022 <- cleg2022 %>%
  distinct(DS_OCUPACAO) %>%
  arrange(DS_OCUPACAO)

# Agora sabemos que as tres ocupacoes que dizem respeito ao nosso levantamento sao:
# "BOMBEIRO MILITAR"
# "MEMBRO DAS FORÇAS ARMADAS"
# "MILITAR REFORMADO"
# "POLICIAL CIVIL"
# "POLICIAL MILITAR"

# P: Por que precisamos fazer essa pesquisa antes?
# R: Para saber COMO esses nomes estao escritos (com acento, sem acento, caixa alta, caixa baixa etc.)

# Na proxima etapa, vamos filtrar mais uma vez nossa base, agora apenas com as ocupacoes que queremos
cand_leg_mil_2022 <- cand_leg_2022 %>%
  filter(DS_OCUPACAO == "BOMBEIRO MILITAR"
         | DS_OCUPACAO == "MEMBRO DAS FORÇAS ARMADAS"
         | DS_OCUPACAO == "MILITAR REFORMADO"
         | DS_OCUPACAO == "POLICIAL CIVIL"
         | DS_OCUPACAO == "POLICIAL MILITAR")

# Por último, vamos considerar apenas as pessoas que foram considerados APTOS pela Justiça Eleitoral.
# Afinal, a base tem TODOS as pessoas, nada garante que TODOS entrem nos nossos requisitos.

# Se nao guardarmos o resultado de alguma operacao em um objeto, ele sera printado em nosso console
cand_leg_2022 %>% 
  distinct(DS_SITUACAO_CANDIDATURA)

cand_leg_mil_2022 <- cand_leg_mil_2022 %>%
  filter(DS_SITUACAO_CANDIDATURA == "APTO")

# Viram como eu nao criei um objeto "novo"? O Rstudio aceita que eu "atualize" um objeto que ja existe.
# Isso e bom, mas exige cuidado, pra nao gravar algo por cima.
# Mas esse problema pode ser facilmente corrigido, basta subir nas linhas anteriores e rodar o codigo de novo!

# Bom, agora queremos saber quantos desses candidatos foram eleitos.
# A coluna que mostra o resultado pra cada um deles se chama DESCRICAO_SIT_TOT_TURNO
# Vamos ver quais sao as possibilidades:
cand_leg_2022 %>% 
  distinct(DS_SIT_TOT_TURNO)

# Para treinar os operadores logicos, vamos usar a base original (cand_2022)
# E aplicar tanto o filtro da ocupacao quanto o filtro da situacao dos candidatos
cand_leg_mil_2022_eleitos <- cand_2022 %>%
  filter((DS_CARGO == "DEPUTADO ESTADUAL"
          | DS_CARGO == "DEPUTADO FEDERAL"
          | DS_CARGO == "DEPUTADO DISTRITAL"
          | DS_CARGO == "SENADOR")
         &
        (DS_OCUPACAO == "BOMBEIRO MILITAR"
         | DS_OCUPACAO == "MEMBRO DAS FORÇAS ARMADAS"
         | DS_OCUPACAO == "MILITAR REFORMADO"
         | DS_OCUPACAO == "POLICIAL CIVIL"
         | DS_OCUPACAO == "POLICIAL MILITAR")
         &
        DS_SITUACAO_CANDIDATURA == "APTO"
         &
        (DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"
         | DS_SIT_TOT_TURNO == "ELEITO POR QP"
         | DS_SIT_TOT_TURNO == "ELEITO"))

# Como eu tenho tres opcoes que me indicam que a pessoa foi eleita,
# mais uma vez eu uso o simbolo | no script, porque qualquer uma delas serve pra mim.
# MAS eu coloquei tambem o simbolo & no meio do script porque as QUATRO condicoes necessarias
# para a pessoa entrar no meu levantamento sao:
# 1- ser uma candidatura a algum orgao do Legislativo
# E
# 2- ser de uma das ocupacoes listadas
# E
# 3- ter tido a candidatura considerada APTA pelo TSE
# E
# 4- ter sido eleita, seja por media, pelo quociente partidario ou simplesmente "eleito"
# Notaram como precisei colocar as opcoes de ocupacao, e as da situacao na urna, dentro de parenteses?
# O motivo eh para "agrupar" as opcoes e fazer o computador entender que sao condicoes separadas

# Pronto! Sabemos que, nas eleicoes de 2022:
# 1321 pessoas que declaram sua ocupacao como militares ou policiais 
# se candidataram a um cargo no Legislativo
# e 41 conseguiram se eleger

################################################################################

# (3.1) EXERCICIO: Repita a mesma analise, mas com os dados das eleicoes de 2018:
# Pergunta 1: Total policiais ou militares que concorreram a cargo legislativo
# Pergunta 2: Do total acima, quantas pessoa de fato se elegeram?
# Dica: a base de 2018 tem nome semelhante e foi salva no mesmo caminho da de 2022



################################################################################

##### (4) VISUALIZACAO DOS RESULTADOS

# Podemos criar nosso proprio dataframe com esses resultados,
# e calcular a porcentagem de candidaturas bem sucedidas em cada ano:
ano <- c("eleicoes_2018", "eleicoes_2022")
militares_candidatos <- c(991,1321)
militares_eleitos <- c(49,25)

leg_mil <- data.frame(ano, militares_candidatos, militares_eleitos) %>%
  mutate(pct_eleitos = round(((( militares_eleitos * 100 ) / militares_candidatos)),1))

# Tambem podemos visualizar esses dados num grafico.
# O ggplot2 e um otimo pacote para a producao de graficos no R.
# A sua logica e baseada na producao de camadas que se sobrepoem

# Neste caso, faremos um grafico com o total de militares eleitos nos dois anos
# Utilizando um grafico de barras (as funcoes geom_bar e geom_col podem ser 
# confusas. Sempre que precisar, consulte a ajuda)
ggplot(leg_mil) +
  geom_col(aes(x = ano, y = militares_eleitos))

# Vemos que o grafico nao ficou muito bonito. Podemos personaliza-lo de diferentes
# formas (e gastar muito mais tempo que o recomendavel! haha)
ggplot(leg_mil) +
  geom_col(aes(x = ano, y = militares_eleitos)) +
  labs(title = "Policiais e militares no Legislativo",
       subtitle = "Evolução de 2018 a 2022",
       x = "Ano", y = "Quantidade de eleitos") +
  #scale_y_continuous(limits = c(0,100)) +
  theme_light()

################################################################################

# Ultima observacao: notaram que os numeros estao diferentes dos da reportagem?
# O motivo: as bases tambem mudam ao longo do tempo, o TSE esta sempre fazendo 
# atualizacoes, correcoes. A reportagem foi feita logo apos o anuncio dos eleitos.
# Tambem existem questoes juridicas (um candidato conseguir na Justica mudar 
# a situação da candidatura, por exemplo). Para o jornalismo isso atrapalha 
# porque a reportagem nao vai ser atualizada. Mas, para as ciencias sociais, 
# a diferenca numerica eh estatisticamente irrelevante. Nenhuma base vai refletir 
# a realidade com 100% de precisao, o importante e chegarmos o mais perto dela
# que consigamos, sempre seguindo uma metodologia robusta (e referendada por especialistas)
