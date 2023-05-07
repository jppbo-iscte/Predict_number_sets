# import libraries to use
library(readr)     # CSV file I/O, e.g. the read_csv function 
library(plyr)      # for mapvalues() function 
                   # always install plyr package before dplyr otherwise it will give warning messages.
library(rvest)
library(stringr)
library(rlist)
library(dplyr)      # for fast data manipulation(functions like mutate,select,arrange,filter,...)
library(ggplot2)    # for data visualisation
library(VIM)        # for Visualization and Imputation of Missing Values (KNN imputation)
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(Metrics)    # model performance metrics
library(car)
library(cowplot)  # grid plot
library(visdat)
library(neuralnet)
library(car)
library(olsrr)
library(lmtest)
library(tseries)
library(ROCR)
library(pROC)
library(usethis)
library(tibble)
library(e1071)
library(xgboost)
library(tidyverse)
library(reshape2)
library(Ecdat)
library(DiagrammeR)
library(mlr) # For parameter Tuning
library(parallel)
library(parallelMap)
library(partykit)
library(rpart)
library(rpart.plot)
library(ipred)
library(ranger)
library(scales)

###Folder Organization
##Folder Data contains all the data
##Inside the Folder Data exists:
#- Years: Where the Webscraping will store the results
#- Processed Data: Data from the Webscraping that was previously executed and recorded


#Sets the Working Directory as this Script's directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#If this variable is set to TRUE, the script will use pre-recorded, stored data
#If false, it'll get the Data from Webscraping. The Webscraping must be manually run for this to work
usePersistentDir <- TRUE

#Gets the file paths for all ATP and Challenger Matches
list_csv_files_atp <- list.files(path= paste0("../Data/",ifelse(usePersistentDir, "Processed Data/", ""),"Years"), pattern = "(19|20)[0-9][0-9]\\.csv", full.names = TRUE)
list_csv_files_challengers <- list.files(path=paste0("../Data/",ifelse(usePersistentDir, "Processed Data/", ""),"Years"), pattern = "(19|20)[0-9][0-9]\\_ch.csv", full.names = TRUE)

#Reads the ATP Files contents and set the tourney_category
matchesATP = do.call(rbind, lapply(list_csv_files_atp, function(x) read.csv(x, stringsAsFactors = FALSE)))
matchesATP$tourney_category <- "ATP"

#Reads the Challenger Files contents and set the tourney_category
matchesChallengers = do.call(rbind, lapply(list_csv_files_challengers, function(x) read.csv(x, stringsAsFactors = FALSE)))
matchesChallengers$tourney_category <- "Challenger"

#Merge the two dataframes
dados <- rbind(matchesATP, matchesChallengers)

# leitura do dataset
#path <- "../Data/Processed Data/Final_Data.csv"
#dados <- read.csv(path, header=TRUE, sep=",", dec=".", )


#################################################
############  Data Understanding ################
#################################################


### Análise Exploratória
#Análise das variáveis player_height_in_cm e opponent_height_in_cm
hist(dados$player_height_in_cm)
hist(dados$opponent_height_in_cm)



#Análise das variáveis tourney_category, pavement, player_hand, opponent_hand
attach(mtcars)
par(mfrow=c(2,2))
dados$tourney_category<- as.factor(dados$tourney_category)
plot(dados$tourney_category,main = "tourney_category",ylim=c(0,10000),xlab = "Categorias",ylab = "n.º de obs",col = "grey") #grafico



dados$pavement<- as.factor(dados$pavement)
plot(dados$pavement,main = "pavement",ylim=c(0,15000),xlab = "Tipo Piso",ylab = "n.º de obs",col = "grey") #grafico



dados$player_hand<- as.factor(dados$player_hand)
plot(dados$player_hand,main = "player_hand",ylim=c(0,15000),xlab = "Mão dominante jogador",ylab = "n.º de obs",col = "grey") #grafico



dados$opponent_hand<- as.factor(dados$opponent_hand)
plot(dados$opponent_hand,main = "opponent_hand",ylim=c(0,15000),xlab = "Mão dominante oponente",ylab = "n.º de obs",col = "grey") #grafico


#################################################
#############  Data Preparation #################
#################################################


# Divisão dos dados em conjuntos de treino e de teste 

# Atribuição de um valor à semente associada ao processo de seleção aleatória das observações que farão parte dos conjuntos de Treino e de Teste
set.seed(123) 

#decidimos fazer uma divisão de 70% para o conjunto de treino e 30% para o conjunto de teste
index_1<-sample(1:nrow(dados),round(nrow(dados)*0.7)) 
train_1<-dados[index_1, ]
teste_1<-dados[-index_1, ]



###################################################################
####  Data Visualization and Data Analysis
##################################################################

head(train_1)

head(teste_1)


# Structure of train dataset

str(train_1)


# Structure of test dataset

str(teste_1)



# dimensions of data sets

dim(train_1)
dim(teste_1)


# checking for any MISSING VALUES in train and test dataset - best option is summary()
# Summary of train dataset
summary(train_1)


# Summary of test dataset
summary(teste_1)


# Checking the missing (NA) values of the training data

summary(is.na(train_1))

## is.na() returns a logical vector which indicates the number of datapoints which are missing
## TRUE indicates missing.


# The vis_dat function (from visdat library) shows the variables, number of observations, and the type of each variable
# set the plot widht and height (resize plot)

options(repr.plot.width=10, repr.plot.height=8)

visdat::vis_dat(train_1,sort_type = FALSE)

### it is confirmed that the Item_Weight variable has missing values (eliminate the variable or substitute the missing values???)


# Some data visualization
# set the plot width and height (resize plot)

options(repr.plot.width=4, repr.plot.height=3)

# we will first plot and infer from the HISTOGRAMS of each continuous variable
hist(train_1$player_height_in_cm)
hist(train_1$opponent_height_in_cm)


#observando o conjunto de treino, verificamos que a variavel da altura tanto do jogador como do oponente tem 
#alguns valores bastante a baixo do 160 e alguns valores que indicariam que o jogador teria 5 metros

#para os casos dos jogadores com 5 metros, acreditamos que foi um erro de conversão de feet para cm, por isso
#decicimos alterar os valores manualmente

#todos os valores a baixo de 160 cm vão ser considerados NA, pois a altura de 160 é a mais baixa a fazer sentido
#Estes jogadores a baixo tinham valores como 0 e 15 cm, logo foram considerados NAs

#esta limpeza foi feita no conjunto de treino e no conjunto de teste

#Atualização da altura dos jogadores com erro de conversão de feet para cm no conjunto de treino
train_1 <- train_1 %>% mutate(player_height_in_cm = replace(player_height_in_cm, player_height_in_cm == 510, 177))
train_1 <- train_1 %>% mutate(player_height_in_cm = replace(player_height_in_cm, player_height_in_cm == 511, 180))
train_1 <- train_1 %>% mutate(opponent_height_in_cm = replace(opponent_height_in_cm, opponent_height_in_cm == 510, 177))
train_1 <- train_1 %>% mutate(opponent_height_in_cm = replace(opponent_height_in_cm, opponent_height_in_cm == 511, 180))

##Alteração da altura dos jogadores com altura inferior a 160cm no conjunto de treino
train_1 <- train_1 %>% mutate(player_height_in_cm = replace(player_height_in_cm, player_height_in_cm < 160, NA))
train_1 <- train_1 %>% mutate(opponent_height_in_cm = replace(opponent_height_in_cm, opponent_height_in_cm < 160, NA))



##decidimos que a variavel da altura vai ser do tipo numerico, para usarmos futuramente em cálculos
#train_1$player_height_in_cm <- as.numeric(train_1$player_height_in_cm)
#train_1$opponent_height_in_cm <- as.numeric(train_1$opponent_height_in_cm)
#
#teste_1$player_height_in_cm <- as.numeric(teste_1$player_height_in_cm)
#teste_1$opponent_height_in_cm <- as.numeric(teste_1$opponent_height_in_cm)

#observar o resultado da alteração das alturas no conjunto de treino
hist(train_1$player_height_in_cm)
hist(train_1$opponent_height_in_cm)


#observar a distribuição da altura do jogador e do oponente no conjunto de teste
hist(teste_1$player_height_in_cm)
hist(teste_1$opponent_height_in_cm)


#Atualização da altura dos jogadores com erro de conversão de feet para cm no conjunto de teste
teste_1 <- teste_1 %>% mutate(player_height_in_cm = replace(player_height_in_cm, player_height_in_cm == 510, 177))
teste_1 <- teste_1 %>% mutate(player_height_in_cm = replace(player_height_in_cm, player_height_in_cm == 511, 180))
teste_1 <- teste_1 %>% mutate(opponent_height_in_cm = replace(opponent_height_in_cm, opponent_height_in_cm == 510, 177))
teste_1 <- teste_1 %>% mutate(opponent_height_in_cm = replace(opponent_height_in_cm, opponent_height_in_cm == 511, 180))


##Alteração da altura dos jogadores com altura inferior a 160cm no conjunto de teste
teste_1 <- teste_1 %>% mutate(player_height_in_cm = replace(player_height_in_cm, player_height_in_cm < 160, NA))
teste_1 <- teste_1 %>% mutate(opponent_height_in_cm = replace(opponent_height_in_cm, opponent_height_in_cm < 160, NA))


#observar o resultado da alteração das alturas no conjunto de teste
hist(teste_1$player_height_in_cm)
hist(teste_1$opponent_height_in_cm)


#todas as restantes variaveis são variaveis que estão em character
#desta forma fizemos uma analise das variáveis atraves da observação do dataset


#começamos por prestar atenção à variavel do nome do oponente

#verificamos que existiam variaveis cujo nome do adversário era Bye
#quando o nome do adversário é este, significa que o jogador que venceu tinha um ranking alto, logo, não necessitou de jogar aquela ronda
#como nã existiu encontro, decidimos retira todas as linhas onde o adversário fosse Bye
#tanto no conjunto de treino como no conjunto de teste
train_1 <- train_1[!(train_1$opponent=="Bye"),]
teste_1 <- teste_1[!(teste_1$opponent=="Bye"),]



#de seguida analisamos a variavel sets_raw


#observamos que alguns jogos acabavam com  (W/O), outros com (DEF) e ainda tinhamos jogos a terminar por (RET)
#percebemos que jogos onde o resultado foi (W/O) foram jogos onde um dos jogadores decidiu não jogar, 
#logo é uma vitoria onde não houve encontro.
#os jogos terminados em (RET) são jogos onde o jogador que perdeu não conseguiu terminar o encontro, maior parte do casos devido a uma lesão
#assim, decisimos que deveriamos retirar os dados referentes a estes encontros uma vez que, sendo o objetivo prever
#o numero de sets de um jogo, se o jogador não coseguir terminar, não temos uma resposta final para colocar

#os jogos terminados em (DEF) são encontros onde o jogador que perdeu foi desqualificado
#decidimos retirar estes dados pois o encontro não terminou, logo não temos um numero de set que ajuda para a resposta final

#encontramos ainda alguns resultados que são impossiveis de acontecer, como por exemplo 642
#valores deste estilo foram tambem retirados do dataset


list_1 <- unique(unlist(str_split(teste_1$sets_raw, ' '))) # 31 20 31 41 40 11 50 52 21 32 50 52 21 32 10 '' 30 42 22 44 01 43 53 53 53 45 02
list_2 <- unique(unlist(str_split(train_1$sets_raw, ' ')))
setdiff(list_2, list_1)
list_1


remove_stuff <- function(e) { 
  if(isTRUE(grepl('(RET)', e)) || isTRUE(grepl('(W/O)', e) ||  isTRUE(grepl('32', e)) ||  isTRUE(grepl('30', e)) || isTRUE(grepl('(DEF)', e)) || isTRUE('NA') || isTRUE(grepl('642', e)) || isTRUE(grepl('465', e)) || isTRUE(grepl('603', e)) || isTRUE(grepl('10', e)) || isTRUE(grepl('636', e)))) 
  {
    NA
  } else {
    e
  }
}

train_1$sets_raw <- lapply(train_1$sets_raw, remove_stuff)
teste_1$sets_raw <- lapply(teste_1$sets_raw, remove_stuff)

train_1 <- train_1[which(!train_1$sets_raw %in% c(NA, 'NANA', 'NA', NULL, 'NULL')),]
teste_1 <- teste_1[which(!teste_1$sets_raw %in% c(NA, 'NANA', 'NA', NULL, 'NULL')),]




iterate <- function (e) {
  
  if (isTRUE(nchar(e)==2) & isTRUE(as.numeric(substr(e, 1, 1)) > as.numeric(substr(e, 2, 2)))) {
    'P'
  } else if (isTRUE(nchar(e)==2) & isTRUE(as.numeric(substr(e, 1, 1)) < as.numeric(substr(e, 2, 2)))) {
    'O'
  } else if (isTRUE(nchar(e)==3) & isTRUE(isTRUE(as.numeric(substr(e, 1, 1))!=7) & isTRUE(as.numeric(substr(e, 2, 2))!=6)) & isTRUE(isTRUE(as.numeric(substr(e, 1, 1))!=6) & isTRUE(as.numeric(substr(e, 2, 2))!=7))) {
    if (isTRUE((as.numeric(substr(e, 1, 1)) + as.numeric(substr(e, 2, 2))) < as.numeric(substr(e, 3, 3)))) {
      'P'
    } else { 
      'O'
    }
  } else if (isTRUE(nchar(e)==4) & isTRUE(isTRUE(as.numeric(substr(e, 1, 1))!=7) & isTRUE(as.numeric(substr(e, 2, 2))!=6)) & isTRUE(isTRUE(as.numeric(substr(e, 1, 1))!=6) & isTRUE(as.numeric(substr(e, 2, 2))!=7))) {
    if (isTRUE((as.numeric(substr(e, 1, 1)) + as.numeric(substr(e, 2, 2))) > as.numeric(substr(e, 3, 3))+as.numeric(substr(e, 4, 4)))) {
      'P'
    } else {
      'O'
    }
  } else if (isTRUE(e == '1715')) {
    'P'
  } else if (isTRUE(e == '1614')) {
    'P'
  } else if (isTRUE(e == '1816')) {
    'P'
  } else if (isTRUE(e == '1618')) {
    'O'
  } else if (isTRUE(e == '1517')) {
    'O'
  } else if (isTRUE(e == '1416')) {
    'O'
  } else if (isTRUE(nchar(e)==3) & isTRUE(as.numeric(substr(e, 1, 1))==7) & isTRUE(as.numeric(substr(e, 2, 2))==6)) {
    'P'
  } else if (isTRUE(nchar(e)==3) & isTRUE(as.numeric(substr(e, 2, 2))==7) & isTRUE(as.numeric(substr(e, 1, 1))==6)) {
    'O'
  } else if (isTRUE(nchar(e)==4) & isTRUE(as.numeric(substr(e, 1, 1))==7) & isTRUE(as.numeric(substr(e, 2, 2))==6)) {
    'P'
  } else if (isTRUE(nchar(e)==4) & isTRUE(as.numeric(substr(e, 1, 1))==6) & isTRUE(as.numeric(substr(e, 2, 2))==7)) {
    'O'
  }
}


sets_raw_transform <- function(s) {
  a <- list()
  for (e in str_split(s, " ")) {
    a <- append(a, sapply(e, iterate))
  }
  paste(a,collapse = '')
}

two_or_3 <- function(ee) {
  count_P <- 0
  count_O <- 0
  for (e in as.list(strsplit(ee, '')[[1]])) {
    if (isTRUE(e == "P")) {
      count_P <- count_P + 1
    } else if (isTRUE(e == "O")) {
      count_O <- count_O + 1
    }
  }
  if (isTRUE((count_P+count_O)==3) & isTRUE(count_P==2)) {
    3
  } else if (isTRUE((count_P+count_O)==3) & isTRUE(count_O==2)){
    3
  } else if (isTRUE((count_P+count_O)==2) & isTRUE(count_O==2)) {
    3
  } else if (isTRUE((count_P+count_O)==2) & isTRUE(count_P==2)) {
    3
  } else {
    5
  }
}

#####################################################################################################################

train_1$sets <- lapply(train_1$sets_raw, sets_raw_transform)
train_1$number_of_played_sets <- lapply(train_1$sets, tr<- function(e) {nchar(e)})
train_1$best_of_3_or_of_5 <- lapply(train_1$sets, two_or_3)

teste_1$sets <- lapply(teste_1$sets_raw, sets_raw_transform)
teste_1$number_of_played_sets <- lapply(teste_1$sets, tr<- function(e) {nchar(e)})
teste_1$best_of_3_or_of_5 <- lapply(teste_1$sets, two_or_3)



#fazemos uma observação do numero de encontros à melhor de 3 sets e à melhor de 5 sets no conjunto de treino e conjunto de teste
nrow(train_1[train_1$best_of_3_or_of_5 == 3, ])
nrow(train_1[train_1$best_of_3_or_of_5 == 5, ])

nrow(teste_1[teste_1$best_of_3_or_of_5 == 3, ])
nrow(teste_1[teste_1$best_of_3_or_of_5 == 5, ])

#Por termos uma quantidade muito superior de jogos à melhor de 3 sets, decidiu-se retirar todos os jogos à melhor de 5


train_1 <- train_1[!(train_1$best_of_3_or_of_5==5),]
teste_1 <- teste_1[!(teste_1$best_of_3_or_of_5==5),]


#desta forma, com todos os jogos à melhor de 5 sets retirados, juntamente com W/O, encontros que terminaram ou em RET ou em DEF 
#tambem retirados, a nossa variável target varia apenas entre 2 e 3 sets

#as variaveis sets e best_of_3_or_5 foram criadas para termos forma de perceber qual a distribuição dos jogos à melhor de 
# 3 e jogos à melhor de 5 sets
#uma vez que descobrimos e fizemos a respetiva limpeza, estas variaveis podem ser retiradas

train_1$sets <-NULL

train_1$best_of_3_or_of_5<-NULL

teste_1$sets<-NULL

teste_1$best_of_3_or_of_5<-NULL



#a variavel number_of_sets_played e sets_raw são do tipo list
#decidimos coloca-las do tipo numeric e character respetivamente

typeof(train_1$number_of_played_sets)
typeof(train_1$sets_raw)

train_1$sets_raw <- as.character(train_1$sets_raw)
train_1$number_of_played_sets <- as.numeric(train_1$number_of_played_sets)
teste_1$sets_raw <- as.character(teste_1$sets_raw)
teste_1$number_of_played_sets <- as.numeric(teste_1$number_of_played_sets)

#variavel player_hand e opponent_hand

#observamos os valores que as variaveis podem ter
unique(train_1$player_hand)
unique(train_1$opponent_hand)


#observação do numero de registos onde algum dos jogadores é Ambidestro no conjunto de treino
nrow(train_1[train_1$player_hand == "Ambidextrous", ])
nrow(train_1[train_1$opponent_hand == "Ambidextrous", ])


#observação da distribuição de jogadores destros e canhotos no conjunto de treino e teste
nrow(train_1[train_1$player_hand == "Right-Handed", ])
nrow(train_1[train_1$player_hand == "Left-Handed", ])

nrow(teste_1[teste_1$opponent_hand == "Right-Handed", ])
nrow(teste_1[teste_1$opponent_hand == "Left-Handed", ])



#visto que a moda das variaveis player hand e opponent hand é "destro" e o número de ambidestro não chega sequer perto 
#dos acima apresentados, decidiu-se imputar estes valores com "Right-Handed".
#A mesma decisão foi tomada para os NAs e vazios.

train_1 <- train_1 %>% mutate(player_hand = replace(player_hand, player_hand == "", "Right-Handed"))

train_1 <- train_1 %>% mutate(player_hand = replace(player_hand, player_hand == is.na(opponent_hand), "Right-Handed"))

train_1 <- train_1 %>% mutate(player_hand = replace(player_hand, player_hand == "Ambidextrous", "Right-Handed"))

train_1 <- train_1 %>% mutate(opponent_hand = replace(opponent_hand, opponent_hand == "", "Right-Handed"))

train_1 <- train_1 %>% mutate(opponent_hand = replace(opponent_hand, opponent_hand == is.na(opponent_hand), "Right-Handed"))

train_1 <- train_1 %>% mutate(opponent_hand = replace(opponent_hand, opponent_hand == "Ambidextrous", "Right-Handed"))



teste_1 <- teste_1 %>% mutate(player_hand = replace(player_hand, player_hand == "", "Right-Handed"))

teste_1 <- teste_1 %>% mutate(player_hand = replace(player_hand, player_hand == is.na(opponent_hand), "Right-Handed"))

teste_1 <- teste_1 %>% mutate(player_hand = replace(player_hand, player_hand == "Ambidextrous", "Right-Handed"))

teste_1 <- teste_1 %>% mutate(opponent_hand = replace(opponent_hand, opponent_hand == "", "Right-Handed"))

teste_1 <- teste_1 %>% mutate(opponent_hand = replace(opponent_hand, opponent_hand == is.na(opponent_hand), "Right-Handed"))

teste_1 <- teste_1 %>% mutate(opponent_hand = replace(opponent_hand, opponent_hand == "Ambidextrous", "Right-Handed"))









#variavel do player_ranking e opponent_ranking

#verificamos que alguns rankings tinham um T à sua frente
#depois de pesquisar o que este significava, percebemos que o T era referente ao ranking de certo jogador estar igual ao de alguém
#dai usar-se o T de Tied. Decidimos retirar porque queremos transformar a variavel dos rankings numa variavel numerica


##substituir o "T" por "" (vazio)

train_1$player_ranking_last_month <- str_replace_all(train_1$player_ranking_last_month, "T", "")
train_1$opponent_ranking_last_month <- str_replace_all(train_1$opponent_ranking_last_month, "T", "")

teste_1$player_ranking_last_month <- str_replace_all(teste_1$player_ranking_last_month, "T", "")
teste_1$opponent_ranking_last_month <- str_replace_all(teste_1$opponent_ranking_last_month, "T", "")


#deparamo-nos com alguns jogadores tinham o ranking 0. Uma vez que isso é impossivel, decidimos transformar estes valores em NA

train_1 <- train_1 %>% mutate(player_ranking_last_month = replace(player_ranking_last_month, player_ranking_last_month == 0, NA))
train_1 <- train_1 %>% mutate(opponent_ranking_last_month = replace(opponent_ranking_last_month, opponent_ranking_last_month == 0, NA))

teste_1 <- teste_1 %>% mutate(player_ranking_last_month = replace(player_ranking_last_month, player_ranking_last_month == 0, NA))
teste_1 <- teste_1 %>% mutate(opponent_ranking_last_month = replace(opponent_ranking_last_month, opponent_ranking_last_month == 0, NA))


#tranformação das variaveis player_ranking_last_month e opponent_ranking_last_month em variaveis numéricas 
#tanto no conjunto de treino como no conjunto de teste

train_1$player_ranking_last_month <- as.numeric(train_1$player_ranking_last_month)
train_1$opponent_ranking_last_month <- as.numeric(train_1$opponent_ranking_last_month)

teste_1$player_ranking_last_month <- as.numeric(teste_1$player_ranking_last_month)
teste_1$opponent_ranking_last_month <- as.numeric(teste_1$opponent_ranking_last_month)




####################################################################################################

#decidimos que era importante criar uma variavel para sabermos a idade de cada jogador

####################################################################
#   CRIAÇÃO DE VARIAVEL DA IDADE PARA O CONJUNTO DE TREINO  ########
####################################################################


#percorre cada linha da data do torneio e cria uma variavel provisoria para guardar o ano em que o torneio foi realizado

ajuda_tournament<-1

for(date_t in train_1$date){
  
  train_1$year_of_tournament[ajuda_tournament] <- str_split(date_t, "-",simplify = FALSE)[[1]][1] 
  
  ajuda_tournament<-ajuda_tournament+1
  
}

#colocar a variavel do ano do torneio como um valor inteiro
train_1$year_of_tournament <- as.integer(train_1$year_of_tournament)


#percorre cada linha da data de nascimento do jogador que venceu e cria uma variavel provisoria com o ano em que este nasceu

ajuda_ano_player<-1

for(date_p in train_1$player_birthday){
  
  train_1$year_of_birth[ajuda_ano_player] <- str_split(date_p, "-",simplify = FALSE)[[1]][1] 
  
  ajuda_ano_player<-ajuda_ano_player+1
  
}

#colocar a variavel do ano em que o jogador nasceu como um valor inteiro
train_1$year_of_birth <- as.integer(train_1$year_of_birth)


#subtrair a data do torneio com a data em que o jogador nasceu para obter a idade que o jogador tinha quando disputou o torneio
train_1$player_age<- abs(train_1$year_of_tournament - train_1$year_of_birth)

train_1$player_age <- as.numeric(train_1$player_age)



#percorre cada linha da data de nascimento do jogador que perdeu e cria uma variavel provisoria com o ano em que este nasceu

ajuda_ano_opponent<-1

for(date_o in train_1$opponent_birthday){
  
  train_1$year_of_birth_o[ajuda_ano_opponent] <- str_split(date_o, "-",simplify = FALSE)[[1]][1] 
  
  ajuda_ano_opponent<-ajuda_ano_opponent+1
  
}

#colocar a variavel do ano em que o jogador nasceu como um valor inteiro
train_1$year_of_birth_o <- as.integer(train_1$year_of_birth_o)



#subtrair a data do torneio com a data em que o jogador nasceu para obter a idade que o jogador tinha quando disputou o torneio
train_1$opponent_age<- abs(train_1$year_of_tournament - train_1$year_of_birth_o)

train_1$opponent_age <- as.numeric(train_1$opponent_age)



#uma vez criadas as variaveis de idade para o jogador vencedor e para o oponente
#podemos eliminar as variaveis provisórias
train_1$year_of_birth<-NULL
train_1$year_of_birth_o<-NULL
train_1$year_of_tournament<-NULL


####################################################################
#   CRIAÇÃO DE VARIAVEL DA IDADE PARA O CONJUNTO DE TESTE  ########
####################################################################


#percorre cada linha da data do torneio e cria uma variavel provisoria para guardar o ano em que o torneio foi realizado

ajuda_tournament<-1

for(date_t in teste_1$date){
  
  teste_1$year_of_tournament[ajuda_tournament] <- str_split(date_t, "-",simplify = FALSE)[[1]][1] 
  
  ajuda_tournament<-ajuda_tournament+1
  
}

#colocar a variavel do ano do torneio como um valor inteiro
teste_1$year_of_tournament <- as.integer(teste_1$year_of_tournament)



#percorre cada linha da data de nascimento do jogador que venceu e cria uma variavel provisoria com o ano em que este nasceu

ajuda_ano_player<-1

for(date_p in teste_1$player_birthday){
  
  teste_1$year_of_birth[ajuda_ano_player] <- str_split(date_p, "-",simplify = FALSE)[[1]][1] 
  
  ajuda_ano_player<-ajuda_ano_player+1
  
}

#colocar a variavel do ano em que o jogador nasceu como um valor inteiro
teste_1$year_of_birth <- as.integer(teste_1$year_of_birth)


#subtrair a data do torneio com a data em que o jogador nasceu para obter a idade que o jogador tinha quando disputou o torneio
teste_1$player_age<- abs(teste_1$year_of_tournament - teste_1$year_of_birth)


teste_1$player_age <- as.numeric(teste_1$player_age)




#percorre cada linha da data de nascimento do jogador que perdeu e cria uma variavel provisoria com o ano em que este nasceu

ajuda_ano_opponent<-1

for(date_o in teste_1$opponent_birthday){
  
  teste_1$year_of_birth_o[ajuda_ano_opponent] <- str_split(date_o, "-",simplify = FALSE)[[1]][1] 
  
  ajuda_ano_opponent<-ajuda_ano_opponent+1
  
}

#colocar a variavel do ano em que o jogador nasceu como um valor inteiro
teste_1$year_of_birth_o <- as.integer(teste_1$year_of_birth_o)

#subtrair a data do torneio com a data em que o jogador nasceu para obter a idade que o jogador tinha quando disputou o torneio
teste_1$opponent_age<- abs(teste_1$year_of_tournament - teste_1$year_of_birth_o)

teste_1$opponent_age <- as.numeric(teste_1$opponent_age)




#uma vez criadas as variaveis de idade para o jogador vencedor e para o oponente
#podemos eliminar as variaveis provisórias

teste_1$year_of_birth<-NULL
teste_1$year_of_birth_o<-NULL
teste_1$year_of_tournament<-NULL




##################################################################
#########     ELIMINAR VARIAVEIS DESNECESSÁRIAS   ################
##################################################################


#como criamos a variavel de idade do jogador, decidimos apagar a variavel da data de nascimento
train_1$player_birthday<-NULL
train_1$opponent_birthday<-NULL

teste_1$player_birthday<-NULL
teste_1$opponent_birthday<-NULL

#Decidimos retirar a variavel tourney_badge e X uma vez que não têm qualquer tipo de influência
#Foram variaveis importantes para a nossa localização no web-scrapping

#Quanto aos nomes dos jogadores e derrotados, uma vez que temos as caracteristicas de cada um, o nome não vai influênciar em nada
#a nossa variavel target, dai termos optado por excluir

#tambem achamos que a variavel prize não é importante para a nossa variavel target, sendo assim excluida

train_1$tourney_badge<-NULL
train_1$X<-NULL
train_1$player<-NULL
train_1$opponent<-NULL
train_1$prize<-NULL

teste_1$tourney_badge<-NULL
teste_1$X<-NULL
teste_1$player<-NULL
teste_1$opponent<-NULL
teste_1$prize<-NULL



################################### PREPARAÇÃO DOS DADOS ###############################


####################  TRATAMENTO DO RUIDO DOS DADOS  ##########################

# AVERIGUAÇÃO DA EXISTÊNCIA DE REGISTOS DUPLICADOS #####

nrow(train_1[duplicated(train_1, fromLast=TRUE),])
nrow(train_1[duplicated(teste_1, fromLast=TRUE),])



########  TRATAMENTO DE VALORES OMISSOS   ######
summary(is.na(train_1)) 


#verificação do numero de linhas em que as variaveis player_full_url e opponent_full_url estão a null
#sempre que uma destas variaveis for um NA, significa que não obtivemos qualquer informação do jogador

nrow(head(train_1[which(is.na(train_1$player_full_url)),], 100)) 
nrow(head(train_1[which(is.na(train_1$opponent_full_url)),], 100))


#retiramos da bse de dados estas linhas, para reduzr no numero de NAs
train_1 <- train_1[!((is.na(train_1$player_full_url))),]
train_1 <- train_1[!((is.na(train_1$opponent_full_url))),]

teste_1 <- teste_1[!((is.na(teste_1$player_full_url))),]
teste_1 <- teste_1[!((is.na(teste_1$opponent_full_url))),]



#uma vez retiradas as linhas com NAs, podemos tambem retirar estas variaveis
#uma vez que foram bastante uteis no web-scrapping mas não acrescenta qualquer tipo de informação para a variavel target
train_1$player_full_url<-NULL
train_1$opponent_full_url<-NULL


teste_1$player_full_url<-NULL
teste_1$opponent_full_url<-NULL




##############################################
########## PRIMEIRO DATASET ##################
##############################################


# como temos variaveis que tem um numero bastante avultado de NAs, decidimos ver a influencia que pode haver no caso de os retirarmos todos

Train_1stTry <- train_1
Test_1stTry <- teste_1


#retirar todos os NAs do dataset
Train_1stTry<- na.omit(Train_1stTry)
Test_1stTry<- na.omit(Test_1stTry)


##############################################
########## SEGUNDO DATASET ###################
##############################################


Train_2ndTry <- train_1
Test_2ndTry <- teste_1

###################################################################
#####   SUBSTITUIÇÃO DE NAs POR KNN NO CONJUNTO DE TREINO   #######
###################################################################


#aplicação do metodo KNN no conjunto de treino
Kvizinhos_train <- Train_2ndTry

k=sqrt(nrow(Train_2ndTry))


# o Método knn vai subsituir os valores NA pelo média dos k vizinhos mais próximos
Kvizinhos_train <- kNN(Kvizinhos_train, variable = c("player_ranking_last_month","opponent_ranking_last_month","player_height_in_cm","opponent_height_in_cm","player_age","opponent_age"), k= 96) # k=sqrt(9248)=96.16, raiz quadrada do número de observações


summary(is.na(Kvizinhos_train))


str(Train_2ndTry)


## Criação de um dataframe sem as colunas player_ranking_last_month_imp, opponent_ranking_last_month_imp, 
# player_height_in_cm_imp, opponent_height_in_cm_imp, player_age_imp e opponent_age_imp que são criadas por aplicação do método knn
Kvizinhos_train <- subset(Kvizinhos_train,select = tourney_name:opponent_age)






###############    IDENTIFICAÇÃO E ALTERAÇÃO DOS OUTLIERS   ###########

###### OUTLIERS opponent_ranking_last_month ########
boxplot(Kvizinhos_train$opponent_ranking_last_month,
        main = "Ranking do Jogador oponente",
        xlab = "Ranking do oponente",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(Kvizinhos_train$opponent_ranking_last_month,main = "Ranking do Jogador oponente",
     xlab = "Ranking do oponente",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_train$opponent_ranking_last_month)

# 72 é o primeiro quartil (Q1)
# 294 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 72 - 1.5*IQR(Kvizinhos_train$opponent_ranking_last_month)

marca_i  # valor de marca_i

marca_s <- 294 + 1.5*IQR(Kvizinhos_train$opponent_ranking_last_month)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_train$opponent_ranking_last_month, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_train$opponent_ranking_last_month[Kvizinhos_train$opponent_ranking_last_month < marca_i] <- caps[1]
Kvizinhos_train$opponent_ranking_last_month[Kvizinhos_train$opponent_ranking_last_month > marca_s] <- caps[2]


boxplot(Kvizinhos_train$opponent_ranking_last_month,
        main = "Ranking do Jogador oponente (valores aparados)",
        xlab = "Ranking do oponente",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)



###################################################################################################################


###### OUTLIERS player_ranking_last_month ########
boxplot(Kvizinhos_train$player_ranking_last_month,
        main = "Ranking do Jogador vencedor",
        xlab = "Ranking do jogador",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(Kvizinhos_train$player_ranking_last_month,main = "Ranking do Jogador vencedor",
     xlab = "Ranking do jogador",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_train$player_ranking_last_month)

# 44 é o primeiro quartil (Q1)
# 239 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 44 - 1.5*IQR(Kvizinhos_train$player_ranking_last_month)

marca_i  # valor de marca_i

marca_s <- 239 + 1.5*IQR(Kvizinhos_train$player_ranking_last_month)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_train$player_ranking_last_month, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_train$player_ranking_last_month[Kvizinhos_train$player_ranking_last_month < marca_i] <- caps[1]
Kvizinhos_train$player_ranking_last_month[Kvizinhos_train$player_ranking_last_month > marca_s] <- caps[2]


boxplot(Kvizinhos_train$player_ranking_last_month,
        main = "Ranking do Jogador vencedor (valores aparados)",
        xlab = "Ranking do jogador",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)






###################################################################################################################


###### OUTLIERS player_height_in_cm ########
boxplot(Kvizinhos_train$player_height_in_cm,
        main = "Altura Jogador vencedor",
        xlab = "Altura do jogador",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(Kvizinhos_train$player_height_in_cm,main = "Altura do Jogador vencedor",
     xlab = "Altura do jogador",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_train$player_height_in_cm)

# 180 é o primeiro quartil (Q1)
# 188 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 180 - 1.5*IQR(Kvizinhos_train$player_height_in_cm)

marca_i  # valor de marca_i

marca_s <- 188 + 1.5*IQR(Kvizinhos_train$player_height_in_cm)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_train$player_height_in_cm, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_train$player_height_in_cm[Kvizinhos_train$player_height_in_cm < marca_i] <- caps[1]
Kvizinhos_train$player_height_in_cm[Kvizinhos_train$player_height_in_cm > marca_s] <- caps[2]


boxplot(Kvizinhos_train$player_height_in_cm,
        main = "Altura do Jogador vencedor (valores aparados)",
        xlab = "Altura do jogador",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)




###################################################################################################################


###### OUTLIERS opponent_height_in_cm ########
boxplot(Kvizinhos_train$opponent_height_in_cm,
        main = "Altura do Jogador Oponente",
        xlab = "Altura do Oponente",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(Kvizinhos_train$opponent_height_in_cm,main = "Altura do Jogador Oponente",
     xlab = "Altura do Oponente",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_train$opponent_height_in_cm)

# 180 é o primeiro quartil (Q1)
# 185 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 180 - 1.5*IQR(Kvizinhos_train$opponent_height_in_cm)

marca_i  # valor de marca_i

marca_s <- 185 + 1.5*IQR(Kvizinhos_train$opponent_height_in_cm)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_train$opponent_height_in_cm, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_train$opponent_height_in_cm[Kvizinhos_train$opponent_height_in_cm < marca_i] <- caps[1]
Kvizinhos_train$opponent_height_in_cm[Kvizinhos_train$opponent_height_in_cm > marca_s] <- caps[2]


boxplot(Kvizinhos_train$opponent_height_in_cm,
        main = "Altura do Jogador Oponente (valores aparados)",
        xlab = "Altura do Altura",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)



#######################################################################
###############    IDENTIFICAÇÃO E ALTERAÇÃO DOS OUTLIERS   ###########
#######################################################################

###### OUTLIERS player_age ########
boxplot(Kvizinhos_train$player_age,
        main = "Idade do Jogador Vencedor",
        xlab = "Idade do Vencedor",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(Kvizinhos_train$player_age,main = "Idade do Jogador Vencedor",
     xlab = "Idade do Vencedor",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_train$player_age)

# 22 é o primeiro quartil (Q1)
# 28 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 22 - 1.5*IQR(Kvizinhos_train$player_age)

marca_i  # valor de marca_i

marca_s <- 28 + 1.5*IQR(Kvizinhos_train$player_age)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_train$player_age, probs=c(.05, .95), na.rm = T)
caps

Kvizinhos_train$player_age[Kvizinhos_train$player_age < marca_i] <- caps[1]
Kvizinhos_train$player_age[Kvizinhos_train$player_age > marca_s] <- caps[2]



boxplot(Kvizinhos_train$player_age,
        main = "Idade do Jogador Vencedor (valores aparados)",
        xlab = "Idade do Vencedor",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)




###################################################################################################################


###############    IDENTIFICAÇÃO E ALTERAÇÃO DOS OUTLIERS   ###########

###### OUTLIERS opponent_age ########
boxplot(Kvizinhos_train$opponent_age,
        main = "Idade do Jogador Oponente",
        xlab = "Idade do Oponente",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(Kvizinhos_train$opponent_age,main = "Idade do Jogador Oponente",
     xlab = "Idade do Oponente",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_train$opponent_age)

# 22 é o primeiro quartil (Q1)
# 28 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 22 - 1.5*IQR(Kvizinhos_train$opponent_age)

marca_i  # valor de marca_i

marca_s <- 28 + 1.5*IQR(Kvizinhos_train$opponent_age)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_train$opponent_age, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_train$opponent_age[Kvizinhos_train$opponent_age < marca_i] <- caps[1]
Kvizinhos_train$opponent_age[Kvizinhos_train$opponent_age > marca_s] <- caps[2]



boxplot(Kvizinhos_train$opponent_age,
        main = "Idade do Jogador Oponente (valores aparados)",
        xlab = "Idade do Oponente",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


###################################################################################################################


#substituição dos valores tratados pelo dataset Kvizinhos_train no dataset Train_2ndTry

Train_2ndTry$player_height_in_cm <- Kvizinhos_train$player_height_in_cm
Train_2ndTry$opponent_height_in_cm <- Kvizinhos_train$opponent_height_in_cm
Train_2ndTry$player_ranking_last_month <- Kvizinhos_train$player_ranking_last_month
Train_2ndTry$opponent_ranking_last_month <- Kvizinhos_train$opponent_ranking_last_month
Train_2ndTry$player_age <- Kvizinhos_train$player_age
Train_2ndTry$opponent_age <- Kvizinhos_train$opponent_age


Train_2ndTry$player_height_in_cm<- as.integer(Train_2ndTry$player_height_in_cm)
Train_2ndTry$opponent_height_in_cm<- as.integer(Train_2ndTry$opponent_height_in_cm)
Train_2ndTry$player_ranking_last_month<- as.integer(Train_2ndTry$player_ranking_last_month)
Train_2ndTry$opponent_ranking_last_month<- as.integer(Train_2ndTry$opponent_ranking_last_month)





###################################################################
#####   SUBSTITUIÇÃO DE NAs POR KNN NO CONJUNTO DE TESTE   #######
###################################################################

#aplicação do metodo KNN no conjunto de teste
Kvizinhos_test <- Test_2ndTry

k_t=sqrt(nrow(Test_2ndTry))


# o Método knn vai subsituir os valores NA pelo média dos k vizinhos mais próximos
Kvizinhos_test <- kNN(Kvizinhos_test, variable = c("player_ranking_last_month","opponent_ranking_last_month","player_height_in_cm","opponent_height_in_cm","player_age","opponent_age"), k= 63) # k=sqrt(3953)=62.87, raiz quadrada do número de observações





## Criação de um dataframe sem as colunas player_ranking_last_month_imp, opponent_ranking_last_month_imp, 
# player_height_in_cm_imp, opponent_height_in_cm_imp, player_age_imp e opponent_age_imp que são criadas por aplicação do método knn
Kvizinhos_test <- subset(Kvizinhos_test,select = tourney_name:opponent_age)




###############    IDENTIFICAÇÃO E ALTERAÇÃO DOS OUTLIERS   ###########

###### OUTLIERS opponent_ranking_last_month ########
boxplot(Kvizinhos_test$opponent_ranking_last_month,
        main = "Ranking do Jogador Oponente",
        xlab = "Ranking do Oponente",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(Kvizinhos_test$opponent_ranking_last_month,main = "Ranking do Jogador Oponente",
     xlab = "Ranking do Oponente",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_test$opponent_ranking_last_month)

# 70 é o primeiro quartil (Q1)
# 294 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 70 - 1.5*IQR(Kvizinhos_test$opponent_ranking_last_month)

marca_i  # valor de marca_i

marca_s <- 294 + 1.5*IQR(Kvizinhos_test$opponent_ranking_last_month)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_test$opponent_ranking_last_month, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_test$opponent_ranking_last_month[Kvizinhos_test$opponent_ranking_last_month < marca_i] <- caps[1]
Kvizinhos_test$opponent_ranking_last_month[Kvizinhos_test$opponent_ranking_last_month > marca_s] <- caps[2]


boxplot(Kvizinhos_test$opponent_ranking_last_month,
        main = "Ranking do Jogador oponente (valores aparados)",
        xlab = "Ranking do oponente",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)



###################################################################################################################


###### OUTLIERS player_ranking_last_month ########
boxplot(Kvizinhos_test$player_ranking_last_month,
        main = "Ranking do Jogador vencedor",
        xlab = "Ranking do jogador",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(Kvizinhos_test$player_ranking_last_month,main = "Ranking do Jogador vencedor",
     xlab = "Ranking do jogador",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_test$player_ranking_last_month)

# 45 é o primeiro quartil (Q1)
# 238 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 45 - 1.5*IQR(Kvizinhos_test$player_ranking_last_month)

marca_i  # valor de marca_i

marca_s <- 238 + 1.5*IQR(Kvizinhos_test$player_ranking_last_month)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_test$player_ranking_last_month, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_test$player_ranking_last_month[Kvizinhos_test$player_ranking_last_month < marca_i] <- caps[1]
Kvizinhos_test$player_ranking_last_month[Kvizinhos_test$player_ranking_last_month > marca_s] <- caps[2]


boxplot(Kvizinhos_test$player_ranking_last_month,
        main = "Ranking do Jogador vencedor (valores aparados)",
        xlab = "Ranking do jogador",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)




###################################################################################################################


###### OUTLIERS player_height_in_cm ########
boxplot(Kvizinhos_test$player_height_in_cm,
        main = "Altura do Jogador vencedor",
        xlab = "Altura do jogador",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)

hist(Kvizinhos_test$player_height_in_cm,main = "Altura do Jogador vencedor",
     xlab = "Altura do jogador",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_test$player_height_in_cm)

# 180 é o primeiro quartil (Q1)
# 188 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 180 - 1.5*IQR(Kvizinhos_test$player_height_in_cm)

marca_i  # valor de marca_i

marca_s <- 188 + 1.5*IQR(Kvizinhos_test$player_height_in_cm)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_test$player_height_in_cm, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_test$player_height_in_cm[Kvizinhos_test$player_height_in_cm < marca_i] <- caps[1]
Kvizinhos_test$player_height_in_cm[Kvizinhos_test$player_height_in_cm > marca_s] <- caps[2]


boxplot(Kvizinhos_test$player_height_in_cm,
        main = "Altura do Jogador vencedor (valores aparados)",
        xlab = "Altura do jogador",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)




###################################################################################################################


###### OUTLIERS opponent_height_in_cm ########
boxplot(Kvizinhos_test$opponent_height_in_cm,
        main = "Altura do Jogador derrotado",
        xlab = "Altura do derrotado",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


hist(Kvizinhos_test$opponent_height_in_cm,main = "Altura do Jogador derrotado",
     xlab = "Altura doderrotado",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_test$opponent_height_in_cm)

# 180 é o primeiro quartil (Q1)
# 185 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 180 - 1.5*IQR(Kvizinhos_test$opponent_height_in_cm)

marca_i  # valor de marca_i

marca_s <- 185 + 1.5*IQR(Kvizinhos_test$opponent_height_in_cm)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_test$opponent_height_in_cm, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_test$opponent_height_in_cm[Kvizinhos_test$opponent_height_in_cm < marca_i] <- caps[1]
Kvizinhos_test$opponent_height_in_cm[Kvizinhos_test$opponent_height_in_cm > marca_s] <- caps[2]


boxplot(Kvizinhos_test$opponent_height_in_cm,
        main = "Altura do Jogador derrotado (valores aparados)",
        xlab = "Altura do derrotado",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)




####################################################################################




###############    IDENTIFICAÇÃO E ALTERAÇÃO DOS OUTLIERS   ###########

###### OUTLIERS player_age ########
boxplot(Kvizinhos_test$player_age,
        main = "Idade do jogador vencedor",
        xlab = "Idade do vencedor",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)

hist(Kvizinhos_test$player_age,main = "Idade do jogador vencedor",
     xlab = "Idade do vencedor",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_test$player_age)

# 22 é o primeiro quartil (Q1)
# 28 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 22 - 1.5*IQR(Kvizinhos_test$player_age)

marca_i  # valor de marca_i

marca_s <- 28 + 1.5*IQR(Kvizinhos_test$player_age)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_test$player_age, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_test$player_age[Kvizinhos_test$player_age < marca_i] <- caps[1]
Kvizinhos_test$player_age[Kvizinhos_test$player_age > marca_s] <- caps[2]


boxplot(Kvizinhos_test$player_age,
        main = "Idade do jogador vencedor (valores aparados)",
        xlab = "Idade vencedor",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)



###################################################################################################################


###############    IDENTIFICAÇÃO E ALTERAÇÃO DOS OUTLIERS   ###########

###### OUTLIERS opponent_age ########
boxplot(Kvizinhos_test$opponent_age,
        main = "Idade do jogador derrotado",
        xlab = "Idade derrotado",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)

hist(Kvizinhos_test$opponent_age,main = "Idade do jogador derrotado",
     xlab = "Idade derrotado",
     ylab = "Observações",
     border = "black")

summary(Kvizinhos_test$opponent_age)

# 22 é o primeiro quartil (Q1)
# 28 é o terceiro quartil (Q3)

# marca_i  será usada para identificar outliers inferiores

marca_i <- 22 - 1.5*IQR(Kvizinhos_test$opponent_age)

marca_i  # valor de marca_i

marca_s <- 28 + 1.5*IQR(Kvizinhos_test$opponent_age)

marca_s  # valor de marca_s


caps <- quantile(Kvizinhos_test$opponent_age, probs=c(.05, .95), na.rm = T)
caps
Kvizinhos_test$opponent_age[Kvizinhos_test$opponent_age < marca_i] <- caps[1]
Kvizinhos_test$opponent_age[Kvizinhos_test$opponent_age > marca_s] <- caps[2]


boxplot(Kvizinhos_test$opponent_age,
        main = "Idade do jogador derrotado (valores aparados)",
        xlab = "Idade derrotado",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


###################################################################################################################

#substituição dos valores tratados pelo dataset Kvizinhos_test no dataset Test_2ndTry

Test_2ndTry$player_height_in_cm <- Kvizinhos_test$player_height_in_cm
Test_2ndTry$opponent_height_in_cm <- Kvizinhos_test$opponent_height_in_cm
Test_2ndTry$player_ranking_last_month <- Kvizinhos_test$player_ranking_last_month
Test_2ndTry$opponent_ranking_last_month <- Kvizinhos_test$opponent_ranking_last_month
Test_2ndTry$player_age <- Kvizinhos_test$player_age
Test_2ndTry$opponent_age <- Kvizinhos_test$opponent_age


Test_2ndTry$player_height_in_cm<- as.integer(Test_2ndTry$player_height_in_cm)
Test_2ndTry$opponent_height_in_cm<- as.integer(Test_2ndTry$opponent_height_in_cm)
Test_2ndTry$player_ranking_last_month<- as.integer(Test_2ndTry$player_ranking_last_month)
Test_2ndTry$opponent_ranking_last_month<- as.integer(Test_2ndTry$opponent_ranking_last_month)


#uma vez que o quarto dataset vai ser com a base da substituição dos knn do segundo dataset,
#o Train_4thTry e o Test_4thTry vão ser atribuidos

Train_4thTry <- Train_2ndTry
Test_4thTry <- Test_2ndTry



Train_2ndTry <- Train_2ndTry[!(is.na(Train_2ndTry$player_country_of_origin)),]
Train_2ndTry <- Train_2ndTry[!(is.na(Train_2ndTry$opponent_country_of_origin)),]

Test_2ndTry <- Test_2ndTry[!(is.na(Test_2ndTry$player_country_of_origin)),]
Test_2ndTry <- Test_2ndTry[!(is.na(Test_2ndTry$opponent_country_of_origin)),]

##############################################
########## TERCEIRO DATASET ##################
##############################################


Train_3rdTry<-train_1
Test_3rdTry<- teste_1



################################################################################
#####   CRIAÇÃO DE NOVAS VARIAVEIS  para o conjnto de treino   #################
################################################################################


#calcula a diferenca de idades entre os 2 jogadores
Train_3rdTry$difference_of_players_age <- (Train_3rdTry$player_age - Train_3rdTry$opponent_age)

#calcula a media de idades entre os 2 jogadores
Train_3rdTry$average_age <- round(rowMeans(Train_3rdTry[,c('player_age','opponent_age')], na.rm=FALSE),0)

#variavel onde se ambos os jogadores forem espanhois é igual a 2, se um dos jogadores for espanhol é igual a 1 e se nenhum jogador for espanhol é igual a 0
Train_3rdTry$home_country <- with(Train_3rdTry, ifelse((player_country_of_origin =='Spain' & opponent_country_of_origin =='Spain'), 2, ifelse((player_country_of_origin =='Spain'|opponent_country_of_origin =='Spain'), 1, 0)))

##variavel com a média de alturas entre os 2 jogadores
Train_3rdTry$average_height <- round(rowMeans(Train_3rdTry[,c('player_height_in_cm','opponent_height_in_cm')], na.rm=FALSE),0)

##variavel com a diferença de alturas entre o jogador vencedor e o derrotado
#caso o vencedor seja mais baixo, a variavel vai ser negativa
Train_3rdTry$height_difference <- (Train_3rdTry$player_height_in_cm- Train_3rdTry$opponent_height_in_cm)

##calcula a média dos rankings dos 2 jogadores
Train_3rdTry$average_ranking_last_month <- round(rowMeans(Train_3rdTry[,c('player_ranking_last_month','opponent_ranking_last_month')], na.rm=FALSE),0)


##variavel com a diferença de rankings entre o jogador vencedor e o derrotado
#caso o vencedor seja tenha um ranking pior(mais alto), a variavel vai ser negativa
Train_3rdTry$difference_between_rankings_last_month <- (Train_3rdTry$player_ranking_last_month - Train_3rdTry$opponent_ranking_last_month)





#criação da variavel DUMMY para saber de é atp ou challenger
ajuda_tourney_category<-1

for(help_category in Train_3rdTry$tourney_category ){
  
  if(grepl("ATP" , help_category)){
    
    Train_3rdTry$tourney_category_dummy[ajuda_tourney_category]="0"
    
  }
  if(grepl("Challenger" , help_category)){
    
    Train_3rdTry$tourney_category_dummy[ajuda_tourney_category]="1"
    
  }
  ajuda_tourney_category<-ajuda_tourney_category +1
}

Train_3rdTry$tourney_category_dummy <- as.integer(Train_3rdTry$tourney_category_dummy)


#### criação de uma variavel para ver se os 2 jogadores são destros, ou se algum ? canhoto, ou e ambos s?o canhotos
# se acharem desnecess?rio apaga-se 

counter_player_hand<-1

for(hand_player_help in Train_3rdTry$player_hand){
  
  if(is.na(hand_player_help)){
    Train_3rdTry$player_hand_binario[counter_player_hand] =""
  } else if( grepl("Right-Handed" , hand_player_help)){
    Train_3rdTry$player_hand_binario[counter_player_hand] ="0"
  } else if( grepl("Left-Handed" , hand_player_help)){
    Train_3rdTry$player_hand_binario[counter_player_hand] ="1"
  } else if(grepl("" , hand_player_help)){
    Train_3rdTry$player_hand_binario[counter_player_hand] =""
    
  } 
  counter_player_hand<-counter_player_hand+ 1
}

Train_3rdTry$player_hand_binario <- as.integer(Train_3rdTry$player_hand_binario)





counter_opponent_hand<-1

for(hand_opponent_help in Train_3rdTry$opponent_hand){
  
  if(is.na(hand_opponent_help)){
    Train_3rdTry$opponent_hand_binario[counter_opponent_hand] = ""
  } else if( grepl("Right-Handed" , hand_opponent_help)){
    Train_3rdTry$opponent_hand_binario[counter_opponent_hand] ="0"
  } else if( grepl("Left-Handed" , hand_opponent_help)){
    Train_3rdTry$opponent_hand_binario[counter_opponent_hand] ="1"
  } else if(grepl("" , hand_opponent_help)){
    Train_3rdTry$opponent_hand_binario[counter_opponent_hand] =""
    
  } 
  
  counter_opponent_hand<-counter_opponent_hand+ 1
}


Train_3rdTry$opponent_hand_binario <- as.integer(Train_3rdTry$opponent_hand_binario)



#se for um jogo entre dois destros =0
#se fr um jogo entre dois canhotos =2
# se for um jog entre maos diferentes =1
Train_3rdTry$game_type_of_hand <- with(Train_3rdTry, ifelse((player_hand_binario ==0 & opponent_hand_binario == 0), 0, ifelse(((player_hand_binario == 0 & opponent_hand_binario == 1)|(player_hand_binario == 1 & opponent_hand_binario == 0)), 1,ifelse((player_hand_binario ==1 & opponent_hand_binario == 1), 2, -1))))




Train_3rdTry$opponent_hand_binario<-NULL
Train_3rdTry$player_hand_binario<-NULL

Train_3rdTry$player_ranking_last_month <-NULL
Train_3rdTry$opponent_ranking_last_month <-NULL
Train_3rdTry$player_height_in_cm <-NULL
Train_3rdTry$player_country_of_origin <-NULL
Train_3rdTry$player_hand <-NULL
Train_3rdTry$opponent_height_in_cm <-NULL
Train_3rdTry$opponent_country_of_origin <-NULL
Train_3rdTry$opponent_hand <-NULL
Train_3rdTry$player_age <-NULL
Train_3rdTry$opponent_age <-NULL
Train_3rdTry$tourney_category <-NULL


################################################################################
#####   CRIAÇÃO DE NOVAS VARIAVEIS  para o conjunto de teste   #################
################################################################################





#calcula a diferenca de idades entre os 2 jogadores
Test_3rdTry$difference_of_players_age <- ( Test_3rdTry$player_age - Test_3rdTry$opponent_age)

#calcula a media de idades entre os 2 jogadores
Test_3rdTry$average_age <- round(rowMeans(Test_3rdTry[,c('player_age','opponent_age')], na.rm=FALSE),0)


#variavel onde se ambos os jogadores forem espanhois é igual a 2, se um dos jogadores for espanhol é igual a 1 e se nenhum jogador for espanhol é igual a 0
Test_3rdTry$home_country <- with(Test_3rdTry, ifelse((player_country_of_origin =='Spain' & opponent_country_of_origin =='Spain'), 2, ifelse((player_country_of_origin =='Spain'|opponent_country_of_origin =='Spain'), 1, 0)))

##variavel com a média de alturas entre os 2 jogadores
Test_3rdTry$average_height <- round(rowMeans(Test_3rdTry[,c('player_height_in_cm','opponent_height_in_cm')], na.rm=FALSE),0)

##variavel com a diferença de alturas entre o jogador vencedor e o derrotado
#caso o vencedor seja mais baixo, a variavel vai ser negativa
Test_3rdTry$height_difference <- (Test_3rdTry$player_height_in_cm- Test_3rdTry$opponent_height_in_cm)


##calcula a média dos rankings dos 2 jogadores
Test_3rdTry$average_ranking_last_month <- round(rowMeans(Test_3rdTry[,c('player_ranking_last_month','opponent_ranking_last_month')], na.rm=FALSE),0)


##variavel com a diferença de rankings entre o jogador vencedor e o derrotado
#caso o vencedor seja tenha um ranking pior(mais alto), a variavel vai ser negativa
Test_3rdTry$difference_between_rankings_last_month <- (Test_3rdTry$player_ranking_last_month - Test_3rdTry$opponent_ranking_last_month)






#criação da variavel DUMMY para saber de é atp ou challenger
ajuda_tourney_category<-1

for(help_category in Test_3rdTry$tourney_category ){
  
  if(grepl("ATP" , help_category)){
    
    Test_3rdTry$tourney_category_dummy[ajuda_tourney_category]="0"
    
  }
  if(grepl("Challenger" , help_category)){
    
    Test_3rdTry$tourney_category_dummy[ajuda_tourney_category]="1"
    
  }
  ajuda_tourney_category<-ajuda_tourney_category +1
}

Test_3rdTry$tourney_category_dummy <- as.integer(Test_3rdTry$tourney_category_dummy)





#### criamos uma variavel para ver se os 2 jogadores s?o destros, ou se algum ? canhoto, ou e ambos s?o canhotos
# se acharem desnecess?rio apaga-se 

counter_player_hand<-1

for(hand_player_help in Test_3rdTry$player_hand){
  
  if(is.na(hand_player_help)){
    Test_3rdTry$player_hand_binario[counter_player_hand] =""
  } else if( grepl("Right-Handed" , hand_player_help)){
    Test_3rdTry$player_hand_binario[counter_player_hand] ="0"
  } else if( grepl("Left-Handed" , hand_player_help)){
    Test_3rdTry$player_hand_binario[counter_player_hand] ="1"
  } else if(grepl("" , hand_player_help)){
    Test_3rdTry$player_hand_binario[counter_player_hand] =""
    
  } 
  counter_player_hand<-counter_player_hand+ 1
}

Test_3rdTry$player_hand_binario <- as.integer(Test_3rdTry$player_hand_binario)





counter_opponent_hand<-1

for(hand_opponent_help in Test_3rdTry$opponent_hand){
  
  if(is.na(hand_opponent_help)){
    Test_3rdTry$opponent_hand_binario[counter_opponent_hand] = ""
  } else if( grepl("Right-Handed" , hand_opponent_help)){
    Test_3rdTry$opponent_hand_binario[counter_opponent_hand] ="0"
  } else if( grepl("Left-Handed" , hand_opponent_help)){
    Test_3rdTry$opponent_hand_binario[counter_opponent_hand] ="1"
  } else if(grepl("" , hand_opponent_help)){
    Test_3rdTry$opponent_hand_binario[counter_opponent_hand] =""
    
  } 
  
  counter_opponent_hand<-counter_opponent_hand+ 1
}


Test_3rdTry$opponent_hand_binario <- as.integer(Test_3rdTry$opponent_hand_binario)



#se for um jogo entre dois destros =0
#se fr um jogo entre dois canhotos =2
# se for um jog entre maos diferentes =1
Test_3rdTry$game_type_of_hand <- with(Test_3rdTry, ifelse((player_hand_binario ==0 & opponent_hand_binario == 0), 0, ifelse(((player_hand_binario == 0 & opponent_hand_binario == 1)|(player_hand_binario == 1 & opponent_hand_binario == 0)), 1,ifelse((player_hand_binario ==1 & opponent_hand_binario == 1), 2, -1))))




Test_3rdTry$opponent_hand_binario<-NULL
Test_3rdTry$player_hand_binario<-NULL


Test_3rdTry$player_ranking_last_month <-NULL
Test_3rdTry$opponent_ranking_last_month <-NULL
Test_3rdTry$player_height_in_cm <-NULL
Test_3rdTry$player_country_of_origin <-NULL
Test_3rdTry$player_hand <-NULL
Test_3rdTry$opponent_height_in_cm <-NULL
Test_3rdTry$opponent_country_of_origin <-NULL
Test_3rdTry$opponent_hand <-NULL
Test_3rdTry$player_age <-NULL
Test_3rdTry$opponent_age <-NULL
Test_3rdTry$tourney_category <-NULL




#as variaveis dos rankings e das alturas são as principais fontes de NA do nosso dataset
#decidimos para este dataset retirar todos os NAs destas variaveis

summary(is.na(Train_3rdTry))


Train_3rdTry<- na.omit(Train_3rdTry)
Test_3rdTry <- na.omit(Test_3rdTry)


##############################################
########## QUARTO DATASET ####################
##############################################




################################################################################
#####   CRIAÇÃO DE NOVAS VARIAVEIS  para o conjnto de treino   #################
################################################################################


#calcula a diferenca de idades entre os 2 jogadores
Train_4thTry$difference_of_players_age <- (Train_4thTry$player_age - Train_4thTry$opponent_age)

#calcula a media de idades entre os 2 jogadores
Train_4thTry$average_age <- round(rowMeans(Train_4thTry[,c('player_age','opponent_age')], na.rm=FALSE),0)

##Create new column where 2=both players are playing at home, 1= one of the players is playing at home, 0=none of the players is playing at home
Train_4thTry$home_country <- with(Train_4thTry, ifelse((player_country_of_origin =='Spain' & opponent_country_of_origin =='Spain'), 2, ifelse((player_country_of_origin =='Spain'|opponent_country_of_origin =='Spain'), 1, 0)))

##Create new column with average height between player and opponent
Train_4thTry$average_height <- round(rowMeans(Train_4thTry[,c('player_height_in_cm','opponent_height_in_cm')], na.rm=FALSE),0)

##Create new colum with height difference between player and opponent
Train_4thTry$height_difference <- (Train_4thTry$player_height_in_cm- Train_4thTry$opponent_height_in_cm)

##Create new colum with the average ranking in the game between player and opponent
Train_4thTry$average_ranking_last_month <- round(rowMeans(Train_4thTry[,c('player_ranking_last_month','opponent_ranking_last_month')], na.rm=FALSE),0)


##Create new colum with the difference of ranking in the game between player and opponent
Train_4thTry$difference_between_rankings_last_month <- (Train_4thTry$player_ranking_last_month - Train_4thTry$opponent_ranking_last_month)







#criação da variavel DUMMY para saber de é atp ou challenger
ajuda_tourney_category<-1

for(help_category in Train_4thTry$tourney_category ){
  
  if(grepl("ATP" , help_category)){
    
    Train_4thTry$tourney_category_dummy[ajuda_tourney_category]="0"
    
  }
  if(grepl("Challenger" , help_category)){
    
    Train_4thTry$tourney_category_dummy[ajuda_tourney_category]="1"
    
  }
  ajuda_tourney_category<-ajuda_tourney_category +1
}

Train_4thTry$tourney_category_dummy <- as.integer(Train_4thTry$tourney_category_dummy)





#### criação de uma variavel para ver se os 2 jogadores são destros, ou se algum ? canhoto, ou e ambos s?o canhotos
# se acharem desnecess?rio apaga-se 

counter_player_hand<-1

for(hand_player_help in Train_4thTry$player_hand){
  
  if(is.na(hand_player_help)){
    Train_4thTry$player_hand_binario[counter_player_hand] =""
  } else if( grepl("Right-Handed" , hand_player_help)){
    Train_4thTry$player_hand_binario[counter_player_hand] ="0"
  } else if( grepl("Left-Handed" , hand_player_help)){
    Train_4thTry$player_hand_binario[counter_player_hand] ="1"
  } else if(grepl("" , hand_player_help)){
    Train_4thTry$player_hand_binario[counter_player_hand] =""
    
  } 
  counter_player_hand<-counter_player_hand+ 1
}

Train_4thTry$player_hand_binario <- as.integer(Train_4thTry$player_hand_binario)





counter_opponent_hand<-1

for(hand_opponent_help in Train_4thTry$opponent_hand){
  
  if(is.na(hand_opponent_help)){
    Train_4thTry$opponent_hand_binario[counter_opponent_hand] = ""
  } else if( grepl("Right-Handed" , hand_opponent_help)){
    Train_4thTry$opponent_hand_binario[counter_opponent_hand] ="0"
  } else if( grepl("Left-Handed" , hand_opponent_help)){
    Train_4thTry$opponent_hand_binario[counter_opponent_hand] ="1"
  } else if(grepl("" , hand_opponent_help)){
    Train_4thTry$opponent_hand_binario[counter_opponent_hand] =""
    
  } 
  
  counter_opponent_hand<-counter_opponent_hand+ 1
}


Train_4thTry$opponent_hand_binario <- as.integer(Train_4thTry$opponent_hand_binario)



#se for um jogo entre dois destros =0
#se fr um jogo entre dois canhotos =2
# se for um jog entre maos diferentes =1
Train_4thTry$game_type_of_hand <- with(Train_4thTry, ifelse((player_hand_binario ==0 & opponent_hand_binario == 0), 0, ifelse(((player_hand_binario == 0 & opponent_hand_binario == 1)|(player_hand_binario == 1 & opponent_hand_binario == 0)), 1,ifelse((player_hand_binario ==1 & opponent_hand_binario == 1), 2, -1))))




Train_4thTry$opponent_hand_binario<-NULL
Train_4thTry$player_hand_binario<-NULL

Train_4thTry$player_ranking_last_month <-NULL
Train_4thTry$opponent_ranking_last_month <-NULL
Train_4thTry$player_height_in_cm <-NULL
Train_4thTry$player_country_of_origin <-NULL
Train_4thTry$player_hand <-NULL
Train_4thTry$opponent_height_in_cm <-NULL
Train_4thTry$opponent_country_of_origin <-NULL
Train_4thTry$opponent_hand <-NULL
Train_4thTry$player_age <-NULL
Train_4thTry$opponent_age <-NULL
Train_4thTry$tourney_category <-NULL




################################################################################
#####   CRIAÇÃO DE NOVAS VARIAVEIS  para o conjunto de teste   #################
################################################################################





#calcula a diferenca de idades entre os 2 jogadores
Test_4thTry$difference_of_players_age <- ( Test_4thTry$player_age - Test_4thTry$opponent_age)

#calcula a media de idades entre os 2 jogadores
Test_4thTry$average_age <- round(rowMeans(Test_4thTry[,c('player_age','opponent_age')], na.rm=FALSE),0)

##Create new column where 2=both players are playing at home, 1= one of the players is playing at home, 0=none of the players is playing at home
Test_4thTry$home_country <- with(Test_4thTry, ifelse((player_country_of_origin =='Spain' & opponent_country_of_origin =='Spain'), 2, ifelse((player_country_of_origin =='Spain'|opponent_country_of_origin =='Spain'), 1, 0)))

##Create new column with average height between player and opponent
Test_4thTry$average_height <- round(rowMeans(Test_4thTry[,c('player_height_in_cm','opponent_height_in_cm')], na.rm=FALSE),0)

##Create new colum with height difference between player and opponent
Test_4thTry$height_difference <- (Test_4thTry$player_height_in_cm- Test_4thTry$opponent_height_in_cm)


##Create new colum with the average ranking in the game between player and opponent
Test_4thTry$average_ranking_last_month <- round(rowMeans(Test_4thTry[,c('player_ranking_last_month','opponent_ranking_last_month')], na.rm=FALSE),0)


##Create new colum with the difference of ranking in the game between player and opponent
Test_4thTry$difference_between_rankings_last_month <- (Test_4thTry$player_ranking_last_month - Test_4thTry$opponent_ranking_last_month)




#criação da variavel DUMMY para saber de é atp ou challenger
ajuda_tourney_category<-1

for(help_category in Test_4thTry$tourney_category ){
  
  if(grepl("ATP" , help_category)){
    
    Test_4thTry$tourney_category_dummy[ajuda_tourney_category]="0"
    
  }
  if(grepl("Challenger" , help_category)){
    
    Test_4thTry$tourney_category_dummy[ajuda_tourney_category]="1"
    
  }
  ajuda_tourney_category<-ajuda_tourney_category +1
}

Test_4thTry$tourney_category_dummy <- as.integer(Test_4thTry$tourney_category_dummy)






#### criamos uma variavel para ver se os 2 jogadores s?o destros, ou se algum ? canhoto, ou e ambos s?o canhotos
# se acharem desnecess?rio apaga-se 

counter_player_hand<-1

for(hand_player_help in Test_4thTry$player_hand){
  
  if(is.na(hand_player_help)){
    Test_4thTry$player_hand_binario[counter_player_hand] =""
  } else if( grepl("Right-Handed" , hand_player_help)){
    Test_4thTry$player_hand_binario[counter_player_hand] ="0"
  } else if( grepl("Left-Handed" , hand_player_help)){
    Test_4thTry$player_hand_binario[counter_player_hand] ="1"
  } else if(grepl("" , hand_player_help)){
    Test_4thTry$player_hand_binario[counter_player_hand] =""
    
  } 
  counter_player_hand<-counter_player_hand+ 1
}

Test_4thTry$player_hand_binario <- as.integer(Test_4thTry$player_hand_binario)





counter_opponent_hand<-1

for(hand_opponent_help in Test_4thTry$opponent_hand){
  
  if(is.na(hand_opponent_help)){
    Test_4thTry$opponent_hand_binario[counter_opponent_hand] = ""
  } else if( grepl("Right-Handed" , hand_opponent_help)){
    Test_4thTry$opponent_hand_binario[counter_opponent_hand] ="0"
  } else if( grepl("Left-Handed" , hand_opponent_help)){
    Test_4thTry$opponent_hand_binario[counter_opponent_hand] ="1"
  } else if(grepl("" , hand_opponent_help)){
    Test_4thTry$opponent_hand_binario[counter_opponent_hand] =""
    
  } 
  
  counter_opponent_hand<-counter_opponent_hand+ 1
}


Test_4thTry$opponent_hand_binario <- as.integer(Test_4thTry$opponent_hand_binario)



#se for um jogo entre dois destros =0
#se fr um jogo entre dois canhotos =2
# se for um jog entre maos diferentes =1
Test_4thTry$game_type_of_hand <- with(Test_4thTry, ifelse((player_hand_binario ==0 & opponent_hand_binario == 0), 0, ifelse(((player_hand_binario == 0 & opponent_hand_binario == 1)|(player_hand_binario == 1 & opponent_hand_binario == 0)), 1,ifelse((player_hand_binario ==1 & opponent_hand_binario == 1), 2, -1))))



summary(Test_4thTry)

Test_4thTry$opponent_hand_binario<-NULL
Test_4thTry$player_hand_binario<-NULL


Test_4thTry$player_ranking_last_month <-NULL
Test_4thTry$opponent_ranking_last_month <-NULL
Test_4thTry$player_height_in_cm <-NULL
Test_4thTry$player_country_of_origin <-NULL
Test_4thTry$player_hand <-NULL
Test_4thTry$opponent_height_in_cm <-NULL
Test_4thTry$opponent_country_of_origin <-NULL
Test_4thTry$opponent_hand <-NULL
Test_4thTry$player_age <-NULL
Test_4thTry$opponent_age <-NULL
Test_4thTry$tourney_category <-NULL

Train_4thTry <- Train_4thTry[!(is.na(Train_4thTry$home_country)),]
Test_4thTry <- Test_4thTry[!(is.na(Test_4thTry$home_country)),]




#write.table(Train_1stTry, file= "C:\\Users\\Joao_Bicho\\Desktop\\Faculdade\\Mestrado\\Projeto_MTCD_MP\\Conjuntos Treino e teste\\Train_1stTry.csv", sep=",")
#
#write.table(Train_2ndTry, file= "C:\\Users\\Joao_Bicho\\Desktop\\Faculdade\\Mestrado\\Projeto_MTCD_MP\\Conjuntos Treino e teste\\Train_2ndTry.csv", sep=",")
#
#
#write.table(Train_3rdTry, file= "C:\\Users\\Joao_Bicho\\Desktop\\Faculdade\\Mestrado\\Projeto_MTCD_MP\\Conjuntos Treino e teste\\Train_3rdTry.csv", sep=",")
#
#
#write.table(Train_4thTry, file= "C:\\Users\\Joao_Bicho\\Desktop\\Faculdade\\Mestrado\\Projeto_MTCD_MP\\Conjuntos Treino e teste\\Train_4thTry.csv", sep=",")
#
#
#write.table(Test_1stTry, file= "C:\\Users\\Joao_Bicho\\Desktop\\Faculdade\\Mestrado\\Projeto_MTCD_MP\\Conjuntos Treino e teste\\Test_1stTry.csv", sep=",")
#
#
#write.table(Test_2ndTry, file= "C:\\Users\\Joao_Bicho\\Desktop\\Faculdade\\Mestrado\\Projeto_MTCD_MP\\Conjuntos Treino e teste\\Test_2ndTry.csv", sep=",")
#
#
#write.table(Test_3rdTry, file= "C:\\Users\\Joao_Bicho\\Desktop\\Faculdade\\Mestrado\\Projeto_MTCD_MP\\Conjuntos Treino e teste\\Test_3rdTry.csv", sep=",")
#
#
#write.table(Test_4thTry, file= "C:\\Users\\Joao_Bicho\\Desktop\\Faculdade\\Mestrado\\Projeto_MTCD_MP\\Conjuntos Treino e teste\\Test_4thTry.csv", sep=",")


##################################################
################ MODELING ########################
##################################################


####################################################################
###################  Logistic Regression  ##########################
####################################################################


#tentámos fatorizar todas as vriaveis categóricas, mas não resultou
#por exemplo, a variavel tourney_name, se for feito um unique do conjunto de treino, verificamos que temos 51 nomes de torneios diferentes
#o que aconetece, é quecomo são tantos nomes, aparecem nomes no conjunto de treino que não aparecem no conjunto de teste
#logo quando tentamos correr o predict para fazer previsão de resultados, temos problemas com os levels

#solução final que funcionou foi fazer a regressão logistica apenas com os valores numericos e a variavel target como factor

  
  
  str(Train_1stTry)
  #
  train_log<- Train_1stTry[-c(1,2,3,4,7,9,10,12,13,14)]
  
  train_log <-select(train_log, 1,2,3,4,6,7,5)
  
  str(train_log)
  
  str(Test_1stTry)
  test_log<- Test_1stTry[-c(1,2,3,4,7,9,10,12,13,14)]
  
  test_log <-select(test_log, 1,2,3,4,6,7,5)
  
  
  str(test_log)
  
  
  #retirar a variavel target para um dataset auxiliar
  #atraves deste dataset vamos verificar se as variaveis são independentes ou multicolineares
  train_1_log_correlation <-train_log[-c(7)]
  
  str(train_1_log_correlation)
  
  #função que verifica se as variaveis são multicolineares
  round(cor(train_1_log_correlation),3)
  
  #As variaveis player_ranking_last_month e opponent_ranking_last_month eram multicolineares, com uma multicolinearidade de 0.459
  #Tinhamos de retirar uma das variaveis, acabamos por escolher a variavel opponent_ranking_last_month
  train_log <-train_log[-c(2)]
  test_log <-test_log[-c(2)]
  
  
  train_log$number_of_played_sets <- as.factor(train_log$number_of_played_sets)
  test_log$number_of_played_sets <- as.factor(test_log$number_of_played_sets)
  
  
  
  # Logistic Regression 1st Try Model
  log_1_try<-glm(number_of_played_sets ~ ., data = train_log,family="binomial")
  
  # Logistic Regression 1st Try Model Summary
  summary(log_1_try)
  
  
  
  # Teste do modelo criado (model) com os dados não vistos do conjunto teste (teste_1)
  model_res<-predict(log_1_try,test_log,type="response")
  model_table<-cbind(pred=round(model_res,3),Class=test_log$number_of_played_sets)
  model_table<-data.frame(model_table)
  head(model_table)
  
  
  
  # Criação da Matriz de Confusão e Cálculo das Métricas Accuracy, Sensitivity, Specificity, ...
  # Suposição: se probabilidade estimada > 0.5 então cliente subscreve depósito a prazo ("yes"=2); 
  # Suposição: se probabilidade estimada <= 0.5 então o cliente não subscreve depósito a prazo ("no"=1)
  model_res1<-ifelse(model_res>0.5,2,1)
  model_pred<-factor(model_res1,level=c(1,2),labels=c(2,3))
  confusionMatrix(model_pred,test_log$number_of_played_sets)
  

#face aos resultados obtidos, verifcamos que o numero de observações com a variavel target a 2 era o dobro do numero de observações com a variavel target a 3
#desta forma, decidimos obtar pelo undersampling para tentar obter melhores resultados

nrow(train_log[train_log$number_of_played_sets == 2, ])

nrow(train_log[train_log$number_of_played_sets == 3, ])


nrow(test_log[test_log$number_of_played_sets == 2, ])

nrow(test_log[test_log$number_of_played_sets == 3, ])




###########################################################
#########   1 DATASET     ##############################
##########################################################
str(train_log)


# Balanceamento
set.seed(200)
train_down1_LOG <- downSample(train_log[,-6],train_log[,6]) 

names(train_down1_LOG)[6] <- "number_of_played_sets"



# Representação gráfica da variável alvo (y) com os dados do conjunto train após o balanceamento (train_down1_LOG)
plot(train_down1_LOG$number_of_played_sets,main = "Variável Alvo (number_of_played_sets) - Dados de Treino após Balanceamento (under-sampling)",
     xlab = "Valores da variável number_of_played_sets", ylab = "nº de observações",col = "orange")

summary(train_down1_LOG)




# Logistic Regression 1st Try Model
log_2_try<-glm(number_of_played_sets ~ ., data = train_down1_LOG,family="binomial")

# Logistic Regression 1st Try Model Summary
summary(log_2_try)



# Teste do modelo criado (model) com os dados não vistos do conjunto teste (teste_1)
model_res<-predict(log_2_try,test_log,type="response")
model_table<-cbind(pred=round(model_res,3),Class=test_log$number_of_played_sets)
model_table<-data.frame(model_table)
head(model_table)



# Criação da Matriz de Confusão e Cálculo das Métricas Accuracy, Sensitivity, Specificity, ...
# Suposição: se probabilidade estimada > 0.5 então cliente subscreve depósito a prazo ("yes"=2); 
# Suposição: se probabilidade estimada <= 0.5 então o cliente não subscreve depósito a prazo ("no"=1)
model_res1<-ifelse(model_res>0.5,2,1)
model_pred<-factor(model_res1,labels=c(2,3))
confusionMatrix(model_pred,test_log$number_of_played_sets)




###########################################################
#########   2 DATASET     ##############################
##########################################################



str(Train_2ndTry)
#
train_log_2<- Train_2ndTry[-c(1,2,3,4,7,9,10,12,13,14)]

train_log_2 <-select(train_log_2, 1,2,3,4,6,7,5)

str(train_log_2)

str(Test_2ndTry)
test_log_2<- Test_2ndTry[-c(1,2,3,4,7,9,10,12,13,14)]

test_log_2 <-select(test_log_2, 1,2,3,4,6,7,5)


str(test_log_2)


#retirar a variavel target para um dataset auxiliar
#atravez deste dataset vamos verificar se as variaveis são independentes ou multicolineares
train_2_log_correlation <-train_log_2[-c(7)]

#função que verifica se as variaveis são multicolineares
round(cor(train_2_log_correlation),3)

#As variaveis player_ranking_last_month e opponent_ranking_last_month eram multicolineares, com uma multicolinearidade de 0.639
#Tinhamos de retirar uma das variaveis,
#Visto que no modelo anterior retirámos a variavel opponent_ranking_last_month, desta vez vamos retirar a variavel player_ranking_last_month
train_log_2 <-train_log_2[-c(1)]
test_log_2 <-test_log_2[-c(1)]



train_log_2$number_of_played_sets <- as.factor(train_log_2$number_of_played_sets)
test_log_2$number_of_played_sets <- as.factor(test_log_2$number_of_played_sets)


# Balanceamento 2Dataset
set.seed(200)
train_down2_LOG <- downSample(train_log_2[,-6],train_log_2[,6]) 

names(train_down2_LOG)[6] <- "number_of_played_sets"



# Representação gráfica da variável alvo (y) com os dados do conjunto train após o balanceamento (train_down1_LOG)
plot(train_down2_LOG$number_of_played_sets,main = "Variável Alvo (number_of_played_sets) - Dados de Treino após Balanceamento (under-sampling)",
     xlab = "Valores da variável number_of_played_sets", ylab = "nº de observações",col = "orange")

summary(train_down2_LOG)




# Logistic Regression 1st Try Model
log_3_try<-glm(number_of_played_sets ~ ., data = train_down2_LOG,family="binomial")

# Logistic Regression 3rd Try Model Summary
summary(log_3_try)



# Teste do modelo criado (model) com os dados não vistos do conjunto teste (teste_1)
model_res<-predict(log_3_try,test_log_2,type="response")
model_table<-cbind(pred=round(model_res,3),Class=test_log_2$number_of_played_sets)
model_table<-data.frame(model_table)
head(model_table)



# Criação da Matriz de Confusão e Cálculo das Métricas Accuracy, Sensitivity, Specificity, ...
# Suposição: se probabilidade estimada > 0.5 então cliente subscreve depósito a prazo ("yes"=2); 
# Suposição: se probabilidade estimada <= 0.5 então o cliente não subscreve depósito a prazo ("no"=1)
model_res1<-ifelse(model_res>0.5,2,1)
model_pred<-factor(model_res1,labels=c(2,3))
confusionMatrix(model_pred,test_log_2$number_of_played_sets)




###########################################################
#########   3 DATASET     ##############################
##########################################################
str(Train_3rdTry)
#
train_log_3<- Train_3rdTry[-c(1,2,3,4,5)]

train_log_3 <-select(train_log_3,2,3,4,5,6,7,8,9,10,1)

str(train_log_3)

str(Test_3rdTry)
test_log_3<- Test_3rdTry[-c(1,2,3,4,5)]

test_log_3 <-select(test_log_3,2,3,4,5,6,7,8,9,10,1)




#retirar a variavel target para um dataset auxiliar
#atravez deste dataset vamos verificar se as variaveis são independentes ou multicolineares
train_3_correlation <-train_log_3[-c(10)]

#função que verifica se as variaveis são multicolineares
round(cor(train_3_correlation),3)


#As variaveis tourney_category_dummy e average_player_ranking eram multicolineares, com uma multicolinearidade de 0.6
#Tinhamos de retirar uma das variaveis, acabamos por escolher a variavel tourney_category_dummy
train_log_3 <-train_log_3[-c(8)]
test_log_3 <-test_log_3[-c(8)]


#sendo uma regressão logistica, a variavel target é colocada a factor
train_log_3$number_of_played_sets <- as.factor(train_log_3$number_of_played_sets)
test_log_3$number_of_played_sets <- as.factor(test_log_3$number_of_played_sets)


str(train_log_3)





#verificação da correlação das variaveis sem a variave target no dataset
#valor de referencia a ser o abs(0.3)




# Balanceamento 3Dataset
set.seed(200)
train_down3_LOG <- downSample(train_log_3[,-9],train_log_3[,9]) 

names(train_down3_LOG)[9] <- "number_of_played_sets"



str(train_down3_LOG)

# Representação gráfica da variável alvo (y) com os dados do conjunto train após o balanceamento (train_down1_LOG)
plot(train_down3_LOG$number_of_played_sets,main = "Variável Alvo (number_of_played_sets) - Dados de Treino após Balanceamento (under-sampling)",
     xlab = "Valores da variável number_of_played_sets", ylab = "nº de observações",col = "orange")

summary(train_down3_LOG)




# Logistic Regression 1st Try Model
log_4_try<-glm(number_of_played_sets ~ ., data = train_down3_LOG,family="binomial")

# Logistic Regression 1st Try Model Summary
summary(log_4_try)



# Teste do modelo criado (model) com os dados não vistos do conjunto teste (teste_1)
model_res<-predict(log_4_try,test_log_3,type="response")
model_table<-cbind(pred=round(model_res,3),Class=test_log_3$number_of_played_sets)
model_table<-data.frame(model_table)
head(model_table)



# Criação da Matriz de Confusão e Cálculo das Métricas Accuracy, Sensitivity, Specificity, ...
# Suposição: se probabilidade estimada > 0.5 então cliente subscreve depósito a prazo ("yes"=2); 
# Suposição: se probabilidade estimada <= 0.5 então o cliente não subscreve depósito a prazo ("no"=1)
model_res1<-ifelse(model_res>0.5,2,1)
model_pred<-factor(model_res1,labels=c(2,3))
confusionMatrix(model_pred,test_log_3$number_of_played_sets)



###########################################################
#########   4 DATASET     ##############################
##########################################################
str(Train_4thTry)
#
train_log_4<- Train_4thTry[-c(1,2,3,4,5)]

train_log_4 <-select(train_log_4,2,3,4,5,6,7,8,9,10,1)

str(train_log_4)

str(Test_4thTry)
test_log_4<- Test_4thTry[-c(1,2,3,4,5)]

test_log_4 <-select(test_log_4,2,3,4,5,6,7,8,9,10,1)

str(test_log_4)


#retirar a variavel target para um dataset auxiliar
#atravez deste dataset vamos verificar se as variaveis são independentes ou multicolineares
train_4_log_correlation <-train_log_4[-c(10)]

#função que verifica se as variaveis são multicolineares
round(cor(train_4_log_correlation),3)


#As variaveis tourney_category_dummy e average_player_ranking eram multicolineares, com uma multicolinearidade de 0.6
#Tinhamos de retirar uma das variaveis,
#uma vez que no modelo do 3 datasetacabamos por escolher a variavel tourney_category_dummy, desta vez vai ser retirada a variavel average_player_ranking
train_log_4 <-train_log_4[-c(6)]
test_log_4 <-test_log_4[-c(6)]


train_log_4$number_of_played_sets <- as.factor(train_log_4$number_of_played_sets)
test_log_4$number_of_played_sets <- as.factor(test_log_4$number_of_played_sets)


# Balanceamento 3Dataset
set.seed(200)
train_down4_LOG <- downSample(train_log_4[,-9],train_log_4[,9]) 

names(train_down4_LOG)[9] <- "number_of_played_sets"



# Representação gráfica da variável alvo (y) com os dados do conjunto train após o balanceamento (train_down1_LOG)
plot(train_down4_LOG$number_of_played_sets,main = "Variável Alvo (number_of_played_sets) - Dados de Treino após Balanceamento (under-sampling)",
     xlab = "Valores da variável number_of_played_sets", ylab = "nº de observações",col = "orange")

summary(train_down4_LOG)




# Logistic Regression 1st Try Model
log_5_try<-glm(number_of_played_sets ~ ., data = train_down4_LOG,family="binomial")

# Logistic Regression 1st Try Model Summary
summary(log_5_try)



# Teste do modelo criado (model) com os dados não vistos do conjunto teste (teste_1)
model_res<-predict(log_5_try,test_log_4,type="response")
model_table<-cbind(pred=round(model_res,3),Class=test_log_4$number_of_played_sets)
model_table<-data.frame(model_table)
head(model_table)



# Criação da Matriz de Confusão e Cálculo das Métricas Accuracy, Sensitivity, Specificity, ...
# Suposição: se probabilidade estimada > 0.5 então cliente subscreve depósito a prazo ("yes"=2); 
# Suposição: se probabilidade estimada <= 0.5 então o cliente não subscreve depósito a prazo ("no"=1)
model_res1<-ifelse(model_res>0.5,2,1)
model_pred<-factor(model_res1,labels=c(2,3))
confusionMatrix(model_pred,test_log_4$number_of_played_sets)






####################################################################
#########################  Bagging #################################
####################################################################




#START OF 1ST BAGGING########################################################################################################################


#Remove information about the target variable from the training data
train_target_removed_1 <- Train_1stTry %>% select(-number_of_played_sets)
test_target_removed_1 <- Test_1stTry %>% select(-number_of_played_sets)


# Now we're gonna rid of the columns with information on our target variable. But we still do need to know the labels for training and evaluating our model.
train_target_1 <- Train_1stTry %>% select(number_of_played_sets) %>% mutate(two_0_or_three_1 = ifelse(Train_1stTry$number_of_played_sets==2,0,1)) %>% select(two_0_or_three_1)
test_target_1 <- Test_1stTry %>% select(number_of_played_sets) %>% mutate(two_0_or_three_1 = ifelse(Test_1stTry$number_of_played_sets==2,0,1)) %>% select(two_0_or_three_1)


# Factorize the data
for (v in (train_target_removed_1 %>% select_if(is.character) %>% names())) {
  train_target_removed_1[[v]] <- as.factor(train_target_removed_1[[v]])
}
for (v in (test_target_removed_1 %>% select_if(is.character) %>% names())) {
  test_target_removed_1[[v]] <- as.factor(test_target_removed_1[[v]])
}


train_bagging_1<- cbind(train_target_removed_1,train_target_1)
test_bagging_1<- cbind(test_target_removed_1,test_target_1)


train_bagging_1$two_0_or_three_1 <- as.factor(train_bagging_1$two_0_or_three_1)
test_bagging_1$two_0_or_three_1 <- as.factor(test_bagging_1$two_0_or_three_1)

#str(train_bagging_1)
#str(test_bagging_1)


#Removal of the the variables, from the test and train datasets, which were a character type
#str(Train_1stTry)
train_bagging_1<- Train_1stTry[-c(1,2,3,4,7,9,10,12,13,14)]
train_bagging_1 <-select(train_bagging_1, 1,2,3,4,6,7,5)
#str(train_bagging_1)

#str(Test_1stTry)
test_bagging_1<- Test_1stTry[-c(1,2,3,4,7,9,10,12,13,14)]
test_bagging_1 <-select(test_bagging_1, 1,2,3,4,6,7,5)
#str(test_bagging_1)

#Factoring the Data
train_bagging_1$number_of_played_sets <- as.factor(train_bagging_1$number_of_played_sets)
test_bagging_1$number_of_played_sets <- as.factor(test_bagging_1$number_of_played_sets)


#Creation of the Bagging Model
model_bagging_1 <- bagging(number_of_played_sets ~., data=train_bagging_1, method="rpart", nbagg=100, metric="ROC", coob=TRUE, Control= rpart.control(minsplit=2, cp=0.01))
model_bagging_1


#Predictions of the Bagging Model
model_bagging_previsao_1 <- predict(model_bagging_1, test_bagging_1)
plot(test_bagging_1$number_of_played_sets, model_bagging_previsao_1, main="Árvore Obtida através de Bagging: Previstos vs Reais", xlab="Reais",ylab="Previstos")


#Creation of the error table in order to understand the erros of the predictions
tabela_bagging_1 <- data.frame(VReais=test_bagging_1$number_of_played_sets, VPrevistos=model_bagging_previsao_1)
#str(tabela_bagging_1)
tabela_bagging_1$VReais <- as.integer(tabela_bagging_1$VReais)
tabela_bagging_1$VPrevistos <- as.integer(tabela_bagging_1$VPrevistos)
#str(tabela_bagging_1)
tabela_bagging_1$error <- with(tabela_bagging_1,tabela_bagging_1$VReais-tabela_bagging_1$VPrevistos)
tabela_bagging_1


#Confusion Matrix
test_bagging_1$number_of_played_sets <- as.integer(test_bagging_1$number_of_played_sets)
model_bagging_previsao_1 <- as.integer(model_bagging_previsao_1)
#str(test_bagging_1$number_of_played_sets)
#str(model_bagging_previsao_1)

confusionMatrix(as.factor(model_bagging_previsao_1), as.factor(test_bagging_1$number_of_played_sets))

#END OF 1ST BAGGING#########################################################################################################################



#START OF 2ND BAGGING######################################################################################################################


# Remove information about the target variable from the training data
train_target_removed_2 <- Train_2ndTry %>% select(-number_of_played_sets)
test_target_removed_2 <- Test_2ndTry %>% select(-number_of_played_sets)

# Now we're gotten rid of the columns with information on our target variable. But we still do need to know the labels for training and evaluating our model.
train_target_2 <- Train_2ndTry %>% select(number_of_played_sets) %>% mutate(two_0_or_three_1 = ifelse(Train_2ndTry$number_of_played_sets==2,0,1)) %>% select(two_0_or_three_1)
test_target_2 <- Test_2ndTry %>% select(number_of_played_sets) %>% mutate(two_0_or_three_1 = ifelse(Test_2ndTry$number_of_played_sets==2,0,1)) %>% select(two_0_or_three_1)


# Factorize the data
for (v in (train_target_removed_2 %>% select_if(is.character) %>% names())) {
  train_target_removed_2[[v]] <- as.factor(train_target_removed_2[[v]])
}
for (v in (test_target_removed_2 %>% select_if(is.character) %>% names())) {
  test_target_removed_2[[v]] <- as.factor(test_target_removed_2[[v]])
}


train_bagging_2<- cbind(train_target_removed_2,train_target_2)
test_bagging_2<- cbind(test_target_removed_2,test_target_2)


train_bagging_2$two_0_or_three_1 <- as.factor(train_bagging_2$two_0_or_three_1)
test_bagging_2$two_0_or_three_1 <- as.factor(test_bagging_2$two_0_or_three_1)

#str(train_bagging_2)
#str(test_bagging_2)


#Removal of the the variables, from the test and train datasets, which were a character type
#str(Train_2ndTry)
train_bagging_2<- Train_2ndTry[-c(1,2,3,4,7,9,10,12,13,14)]
train_bagging_2 <-select(train_bagging_2, 1,2,3,4,6,7,5)
#str(train_bagging_2)

#str(Test_2ndTry)
test_bagging_2<- Test_2ndTry[-c(1,2,3,4,7,9,10,12,13,14)]
test_bagging_2 <-select(test_bagging_2, 1,2,3,4,6,7,5)
#str(test_bagging_2)

#Factoring the Data
train_bagging_2$number_of_played_sets <- as.factor(train_bagging_2$number_of_played_sets)
test_bagging_2$number_of_played_sets <- as.factor(test_bagging_2$number_of_played_sets)


#Creation of the Bagging Model
model_bagging_2 <- bagging(number_of_played_sets ~., data=train_bagging_2, method="rpart", nbagg=100, metric="ROC", coob=TRUE, Control= rpart.control(minsplit=2, cp=0.01))
model_bagging_2


#Predictions of the Bagging Model
model_bagging_previsao_2 <- predict(model_bagging_2, test_bagging_2)
plot(test_bagging_2$number_of_played_sets, model_bagging_previsao_2, main="Árvore Obtida através de Bagging: Previstos vs Reais", xlab="Reais",ylab="Previstos")

#Creation of the error table in order to understand the erros of the predictions
tabela_bagging_2 <- data.frame(VReais=test_bagging_2$number_of_played_sets, VPrevistos=model_bagging_previsao_2)
#str(tabela_bagging_2)
tabela_bagging_2$VReais <- as.integer(tabela_bagging_2$VReais)
tabela_bagging_2$VPrevistos <- as.integer(tabela_bagging_2$VPrevistos)
#str(tabela_bagging_2)
tabela_bagging_2$error <- with(tabela_bagging_2,tabela_bagging_2$VReais-tabela_bagging_2$VPrevistos)
tabela_bagging_2

#Confusion Matrix
test_bagging_2$number_of_played_sets <- as.integer(test_bagging_2$number_of_played_sets)
model_bagging_previsao_2 <- as.integer(model_bagging_previsao_2 )
#str(test_bagging_2$number_of_played_sets)
#str(model_bagging_previsao_2)

confusionMatrix(as.factor(model_bagging_previsao_2), as.factor(test_bagging_2$number_of_played_sets))

#END OF 2ND BAGGING##########################################################################################################



#START OF 3RD BAGGING##########################################################################################################


#Remove information about the target variable from the training data
train_target_removed_3 <- Train_3rdTry %>% select(-number_of_played_sets)
test_target_removed_3 <- Test_3rdTry %>% select(-number_of_played_sets)

# Now we're gotten rid of the columns with information on our target variable. But we still do need to know the labels for training and evaluating our model.
train_target_3 <- Train_3rdTry %>% select(number_of_played_sets) %>% mutate(two_0_or_three_1 = ifelse(Train_3rdTry$number_of_played_sets==2,0,1)) %>% select(two_0_or_three_1)
test_target_3 <- Test_3rdTry %>% select(number_of_played_sets) %>% mutate(two_0_or_three_1 = ifelse(Test_3rdTry$number_of_played_sets==2,0,1)) %>% select(two_0_or_three_1)

# Factorize the data
for (v in (train_target_removed_3 %>% select_if(is.character) %>% names())) {
  train_target_removed_3[[v]] <- as.factor(train_target_removed_3[[v]])
}
for (v in (test_target_removed_3 %>% select_if(is.character) %>% names())) {
  test_target_removed_3[[v]] <- as.factor(test_target_removed_3[[v]])
}


train_bagging_3<- cbind(train_target_removed_3,train_target_3)
test_bagging_3<- cbind(test_target_removed_3,test_target_3)


train_bagging_3$two_0_or_three_1 <- as.factor(train_bagging_3$two_0_or_three_1)
test_bagging_3$two_0_or_three_1 <- as.factor(test_bagging_3$two_0_or_three_1)

#str(train_bagging_3)
#str(test_bagging_3)


#Removal of the the variables, from the test and train datasets, which were a character type
#str(Train_3rdTry)
train_bagging_3<- Train_3rdTry[-c(1,2,3,4,5)]
train_bagging_3 <-select(train_bagging_3, 2,3,4,5,6,7,8,9,10,1)
#str(train_bagging_3)

#str(Test_3rdTry)
test_bagging_3<- Test_3rdTry[-c(1,2,3,4,5)]
test_bagging_3 <-select(test_bagging_3, 2,3,4,5,6,7,8,9,10,1)
#str(test_bagging_3)

#Factoring the Data
train_bagging_3$number_of_played_sets <- as.factor(train_bagging_3$number_of_played_sets)
test_bagging_3$number_of_played_sets <- as.factor(test_bagging_3$number_of_played_sets)


#Creation of the Bagging Model
model_bagging_3 <- bagging(number_of_played_sets ~., data=train_bagging_3, method="rpart", nbagg=100, metric="ROC", coob=TRUE, Control= rpart.control(minsplit=2, cp=0.01))
model_bagging_3

#Predictions of the Bagging Model
model_bagging_previsao_3 <- predict(model_bagging_3, test_bagging_3)
plot(test_bagging_3$number_of_played_sets, model_bagging_previsao_3, main="Árvore Obtida através de Bagging: Previstos vs Reais", xlab="Reais",ylab="Previstos")

#Creation of the error table in order to understand the erros of the predictions
tabela_bagging_3 <- data.frame(VReais=test_bagging_3$number_of_played_sets, VPrevistos=model_bagging_previsao_3)
#str(tabela_bagging_3)
tabela_bagging_3$VReais <- as.integer(tabela_bagging_3$VReais)
tabela_bagging_3$VPrevistos <- as.integer(tabela_bagging_3$VPrevistos)
#str(tabela_bagging_3)
tabela_bagging_3$error <- with(tabela_bagging_3,tabela_bagging_3$VReais-tabela_bagging_3$VPrevistos)
tabela_bagging_3

#Confusion Matrix
test_bagging_3$number_of_played_sets <- as.integer(test_bagging_3$number_of_played_sets)
model_bagging_previsao_3 <- as.integer(model_bagging_previsao_3)
#str(test_bagging_3$number_of_played_sets)
#str(model_bagging_previsao_3)

confusionMatrix(as.factor(model_bagging_previsao_3), as.factor(test_bagging_3$number_of_played_sets))

#END OF 3RD BAGGING##########################################################################################################





#START OF 4TH BAGGING##########################################################################################################


# Remove information about the target variable from the training data
train_target_removed_4 <- Train_4thTry %>% select(-number_of_played_sets)
test_target_removed_4 <- Test_4thTry %>% select(-number_of_played_sets)

# Now we're gotten rid of the columns with information on our target variable. But we still do need to know the labels for training and evaluating our model.
train_target_4 <- Train_4thTry %>% select(number_of_played_sets) %>% mutate(two_0_or_three_1 = ifelse(Train_4thTry$number_of_played_sets==2,0,1)) %>% select(two_0_or_three_1)
test_target_4 <- Test_4thTry %>% select(number_of_played_sets) %>% mutate(two_0_or_three_1 = ifelse(Test_4thTry$number_of_played_sets==2,0,1)) %>% select(two_0_or_three_1)


# Factorize the data
for (v in (train_target_removed_4 %>% select_if(is.character) %>% names())) {
  train_target_removed_4[[v]] <- as.factor(train_target_removed_4[[v]])
}
for (v in (test_target_removed_4 %>% select_if(is.character) %>% names())) {
  test_target_removed_4[[v]] <- as.factor(test_target_removed_4[[v]])
}


train_bagging_4<- cbind(train_target_removed_4,train_target_4)
test_bagging_4<- cbind(test_target_removed_4,test_target_4)

train_bagging_4$two_0_or_three_1 <- as.factor(train_bagging_4$two_0_or_three_1)
test_bagging_4$two_0_or_three_1 <- as.factor(test_bagging_4$two_0_or_three_1)

#str(train_bagging_4)
#str(test_bagging_4)



#Removal of the the variables, from the test and train datasets, which were a character type
#str(Train_4thTry)
train_bagging_4<- Train_4thTry[-c(1,2,3,4,5)]
train_bagging_4 <-select(train_bagging_4, 2,3,4,5,6,7,8,9,10,1)
#str(train_bagging_4)

#str(Test_4thTry)
test_bagging_4<- Test_4thTry[-c(1,2,3,4,5)]
test_bagging_4 <-select(test_bagging_4, 2,3,4,5,6,7,8,9,10,1)
#str(test_bagging_4)


#Factoring of the data
train_bagging_4$number_of_played_sets <- as.factor(train_bagging_4$number_of_played_sets)
test_bagging_4$number_of_played_sets <- as.factor(test_bagging_4$number_of_played_sets)

#Creation of the Bagging Model
model_bagging_4 <- bagging(number_of_played_sets ~., data=train_bagging_4, method="rpart", nbagg=100, metric="ROC", coob=TRUE, Control= rpart.control(minsplit=2, cp=0.01))
model_bagging_4


#Predictions of the Bagging Model
model_bagging_previsao_4 <- predict(model_bagging_4, test_bagging_4)
plot(test_bagging_4$number_of_played_sets, model_bagging_previsao_4, main="Árvore Obtida através de Bagging: Previstos vs Reais", xlab="Reais",ylab="Previstos")

#Creation of the error table in order to understand the erros of the predictions
tabela_bagging_4 <- data.frame(VReais=test_bagging_4$number_of_played_sets, VPrevistos=model_bagging_previsao_4)
#str(tabela_bagging_4)
tabela_bagging_4$VReais <- as.integer(tabela_bagging_4$VReais)
tabela_bagging_4$VPrevistos <- as.integer(tabela_bagging_4$VPrevistos)
#str(tabela_bagging_4)
tabela_bagging_4$error <- with(tabela_bagging_4,tabela_bagging_4$VReais-tabela_bagging_4$VPrevistos)
tabela_bagging_4

#Confusion Matrix
test_bagging_4$number_of_played_sets <- as.integer(test_bagging_4$number_of_played_sets)
model_bagging_previsao_4 <- as.integer(model_bagging_previsao_4)
#str(test_bagging_4$number_of_played_sets)
#str(model_bagging_previsao_4)

confusionMatrix(as.factor(model_bagging_previsao_4), as.factor(test_bagging_4$number_of_played_sets))

#END OF 4TH BAGGING##########################################################################################################





####################################################################
######################  Random Forests #############################
####################################################################

#Columns to be dropped
#These columns are not relevant for the model
#Some Country of Origin has so few records that they're not present on the Test and Train. So it's removed
drop <-c("tourney_name", "date", "sets_raw", "player_country_of_origin", "opponent_country_of_origin")

#Process 1st Dataset
#Removes the columns to drop
#And creates a new Dataset exclusively for Random Forests
Test_1stTry_RF <- Test_1stTry[,!(names(Test_1stTry) %in% drop)]
Train_1stTry_RF <- Train_1stTry[,!(names(Train_1stTry) %in% drop)]

#Converts the Target Variable to Factor
Test_1stTry_RF$number_of_played_sets <- as.factor(Test_1stTry_RF$number_of_played_sets)
Train_1stTry_RF$number_of_played_sets <- as.factor(Train_1stTry_RF$number_of_played_sets)

#Downsamples the data. For the Result: 3 we have half the records from Result: 2
#Corrects the Target Factors assymetry
Train_1stTry_RF_Down <- downSample(Train_1stTry_RF[,-as.numeric(grep("number_of_played_sets", colnames(Train_1stTry_RF)))],Train_1stTry_RF[,as.numeric(grep("number_of_played_sets", colnames(Train_1stTry_RF)))]) 
names(Train_1stTry_RF_Down)[as.numeric(grep("Class", colnames(Train_1stTry_RF_Down)))] <- "number_of_played_sets"
Train_1stTry_RF_Down$number_of_played_sets <- as.factor(Train_1stTry_RF_Down$number_of_played_sets)


#Process 2nd Dataset
#Removes the columns to drop
Test_2ndTry_RF <- Test_2ndTry[,!(names(Test_2ndTry) %in% drop)]
Train_2ndTry_RF <- Train_2ndTry[,!(names(Train_2ndTry) %in% drop)]

#Converts the Target Variable to Factor
Test_2ndTry_RF$number_of_played_sets <- as.factor(Test_2ndTry_RF$number_of_played_sets)
Train_2ndTry_RF$number_of_played_sets <- as.factor(Train_2ndTry_RF$number_of_played_sets)

#Downsamples the data. For the Result: 3 we have half the records from Result: 2
#Corrects the Target Factors assymetry
Train_2ndTry_RF_Down <- downSample(Train_2ndTry_RF[,-as.numeric(grep("number_of_played_sets", colnames(Train_2ndTry_RF)))],Train_2ndTry_RF[,as.numeric(grep("number_of_played_sets", colnames(Train_2ndTry_RF)))]) 
names(Train_2ndTry_RF_Down)[as.numeric(grep("Class", colnames(Train_2ndTry_RF_Down)))] <- "number_of_played_sets"
Train_2ndTry_RF_Down$number_of_played_sets <- as.factor(Train_2ndTry_RF_Down$number_of_played_sets)

#Process 3rd Dataset
#Removes the columns to drop
Test_3rdTry_RF <- Test_3rdTry[,!(names(Test_3rdTry) %in% drop)]
Train_3rdTry_RF <- Train_3rdTry[,!(names(Train_3rdTry) %in% drop)]

#Converts the Target Variable to Factor
Test_3rdTry_RF$number_of_played_sets <- as.factor(Test_3rdTry_RF$number_of_played_sets)
Train_3rdTry_RF$number_of_played_sets <- as.factor(Train_3rdTry_RF$number_of_played_sets)

#Downsamples the data. For the Result: 3 we have half the records from Result: 2
#Corrects the Target Factors assymetry
Train_3rdTry_RF_Down <- downSample(Train_3rdTry_RF[,-as.numeric(grep("number_of_played_sets", colnames(Train_3rdTry_RF)))],Train_3rdTry_RF[,as.numeric(grep("number_of_played_sets", colnames(Train_3rdTry_RF)))]) 
names(Train_3rdTry_RF_Down)[as.numeric(grep("Class", colnames(Train_3rdTry_RF_Down)))] <- "number_of_played_sets"
Train_3rdTry_RF_Down$number_of_played_sets <- as.factor(Train_3rdTry_RF_Down$number_of_played_sets)

#Process 4th Dataset
Test_4thTry_RF <- Test_4thTry[,!(names(Test_4thTry) %in% drop)]
Train_4thTry_RF <- Train_4thTry[,!(names(Train_4thTry) %in% drop)]

#Converts the Target Variable to Factor
Test_4thTry_RF$number_of_played_sets <- as.factor(Test_4thTry_RF$number_of_played_sets)
Train_4thTry_RF$number_of_played_sets <- as.factor(Train_4thTry_RF$number_of_played_sets)

#Downsamples the data. For the Result: 3 we have half the records from Result: 2
#Corrects the Target Factors assymetry
Train_4thTry_RF_Down <- downSample(Train_4thTry_RF[,-as.numeric(grep("number_of_played_sets", colnames(Train_4thTry_RF)))],Train_4thTry_RF[,as.numeric(grep("number_of_played_sets", colnames(Train_4thTry_RF)))]) 
names(Train_4thTry_RF_Down)[as.numeric(grep("Class", colnames(Train_4thTry_RF_Down)))] <- "number_of_played_sets"
Train_4thTry_RF_Down$number_of_played_sets <- as.factor(Train_4thTry_RF_Down$number_of_played_sets)

plot(Train_1stTry_RF$number_of_played_sets, main = "Variável Alvo - Dados de Treino pré-Balanceamento",
     xlab = "Valores da variável y",
     ylab = "nº de observações",
     col = "orange")

#Loops through all Train four Datasets
for (try in as.list(c(1:4))) {
  
  #sets the name of the variable to be worked on
  name <- paste0("Train_",ordinal(try),"Try_RF_Down")
  
  #Creates a tree model using the random forest method
  cv.control<-trainControl(method="cv", 
                           number=10, 
                           savePredictions="final")
  
  
  # Adjusting the hyperparameters
  tune_forest<-expand.grid(mtry = sqrt(length(colnames(get(name)))), 
                           splitrule="gini",
                           min.node.size=c(2, 5, 10, 20, 50))    
  
  #Train the model
  assign(paste0("model_forest_",name), 
         
         caret::train(number_of_played_sets ~.,
               data = get(name),
               method="ranger",
               metric="Accuracy",
               num.trees=50,
               tuneGrid=tune_forest,
               importance="impurity",
               tuneLength=10,
               trControl=cv.control)
         
  )
  
  print(paste("Predicting on",name))
  
  #Predicts using the corresponding test data
  assign(paste0("model_forest_",name,"_prediction"), 
         predict(get(paste0("model_forest_",name)), get(paste0("Test_",ordinal(try),"Try_RF")))
  )
  print(paste0(name,": Done"))
}



model_forest_Train_1stTry_RF_Down
model_forest_Train_2ndTry_RF_Down
model_forest_Train_3rdTry_RF_Down

#Best Accuracy on Train Data:
model_forest_Train_4thTry_RF_Down


for (try in as.list(c(1:4))) {
  # Criação da Matriz de Confusão e Cálculo das Métricas Accuracy, Sensitivity, Specificity, ...
  print(paste0("model_forest_Train_", ordinal(try),"Try_RF_Down_prediction"))
  
  #Generates the Confusion Matrix
  assign(paste0("model_forest_Train_", ordinal(try),"Try_RF_Down_confusion"),
         confusionMatrix(
           get(paste0("model_forest_Train_", ordinal(try),"Try_RF_Down_prediction")),
           get(paste0("Test_", ordinal(try),"Try_RF"))$number_of_played_sets)
  )
  
  #Plots the variable relevances
  assign(paste0("model_forest_Train_", ordinal(try),"Try_RF_Down_plot"),
         plot(varImp(get(paste0("model_forest_Train_", ordinal(try),"Try_RF_Down"))),
              main=paste0("Importância das Variáveis (", paste0("model_forest_Train_", ordinal(try),"Try"), ") - modelo obtido com método Florestas Aleatórias "))
  )
}

#Confusion Matrixes
model_forest_Train_1stTry_RF_Down_confusion
model_forest_Train_2ndTry_RF_Down_confusion
model_forest_Train_3rdTry_RF_Down_confusion
#Best Accuracy
model_forest_Train_4thTry_RF_Down_confusion

#Calculates the difference between the Test Data (Confusion) and Train Data (Model)
model_forest_Train_1stTry_RF_Down_confusion$overall[1] - model_forest_Train_1stTry_RF_Down$results[1,4]
model_forest_Train_2ndTry_RF_Down_confusion$overall[1] - model_forest_Train_2ndTry_RF_Down$results[1,4]
model_forest_Train_3rdTry_RF_Down_confusion$overall[1] - model_forest_Train_3rdTry_RF_Down$results[1,4]
model_forest_Train_4thTry_RF_Down_confusion$overall[1] - model_forest_Train_4thTry_RF_Down$results[1,4]

#Plots of relevant 
model_forest_Train_1stTry_RF_Down_plot
model_forest_Train_2ndTry_RF_Down_plot
model_forest_Train_3rdTry_RF_Down_plot
model_forest_Train_4thTry_RF_Down_plot

######################################################################################################################



####################################################################
############################  Boosting #############################
####################################################################

# Load datasets
train_boosting <- Train_2ndTry
test_boosting <- Test_2ndTry
############################################################################

# Replace Rankings by Scores
# Scrap Points and use them as an example.

top <- c('0-100', '101-200', '201-300', '301-400', '401-500', '501-600', '601-700', '701-800', '801-900', '901-1000', '1001-1100', '1101-1200', '1201-1300', '1301-1400', '1401-1500', '1501-5000')

poinlist <- c()

for (e in top) {
  link <- paste(paste('https://www.atptour.com/en/rankings/singles?rankRange=', e, sep=''), '&rankDate=2022-12-05', sep='')
  elements <- read_html(link) %>% html_nodes("[class='points-cell border-right-dash-1']") %>% html_text()
  points <- as.numeric(gsub(',', '', str_squish(elements)))
  poinlist <- append(poinlist, points)
}

ranking <- 1:length(poinlist)

points_df <- data.frame(ranking, poinlist) %>% rename(points=poinlist)

# Transform the ranking variable into the points variable

foo  <- function (x) {
  if (x %in% points_df$points) {
    points_df[x, ]$points
  } else {
    1
  }
}

test_boosting$opponent_ranking_last_month <- lapply(test_boosting$opponent_ranking_last_month, foo)
train_boosting$opponent_ranking_last_month <- lapply(train_boosting$opponent_ranking_last_month, foo)

test_boosting$player_ranking_last_month <- lapply(test_boosting$player_ranking_last_month, foo)
train_boosting$player_ranking_last_month <- lapply(train_boosting$player_ranking_last_month, foo)

train_boosting$player_ranking_last_month <- as.numeric(train_boosting$player_ranking_last_month)
test_boosting$player_ranking_last_month <- as.numeric(test_boosting$player_ranking_last_month)

train_boosting$opponent_ranking_last_month <- as.numeric(train_boosting$opponent_ranking_last_month)
test_boosting$opponent_ranking_last_month <- as.numeric(test_boosting$opponent_ranking_last_month)

# Check for outliers and remove
counts <- table(train_boosting$opponent_ranking_last_month)
barplot(counts)

length(test_boosting$opponent_ranking_last_month)
length(train_boosting$opponent_ranking_last_month)


train_boosting <- train_boosting[train_boosting$opponent_ranking_last_month != 1, ]
train_boosting <- train_boosting[train_boosting$player_ranking_last_month != 1, ]
test_boosting <- test_boosting[test_boosting$opponent_ranking_last_month != 1, ]
test_boosting <- test_boosting[test_boosting$player_ranking_last_month != 1, ]


counts <- table(train_boosting$player_ranking_last_month)
barplot(counts)

counts <- table(test_boosting$player_ranking_last_month)
barplot(counts)

counts <- table(train_boosting$opponent_ranking_last_month)
barplot(counts)

counts <- table(test_boosting$opponent_ranking_last_month)
barplot(counts)

# Some values in different columns are not found in train and test altogether. Remove them!
test_boosting$tourney_name [which(!(test_boosting$tourney_name  %in% unique(test_boosting$tourney_name)))] <- NA 
test_boosting$player_country_of_origin [which(!(test_boosting$player_country_of_origin  %in% unique(test_boosting$player_country_of_origin)))] <- NA 
test_boosting$opponent_country_of_origin [which(!(test_boosting$opponent_country_of_origin  %in% unique(test_boosting$opponent_country_of_origin)))] <- NA 
test_boosting <- na.omit(test_boosting)
train_boosting <- na.omit(train_boosting)

# Transform data for 3rd and 4th datasets
# names(train_boosting)
# 
# test_boosting$rank_dif <- (test_boosting$player_ranking_last_month + test_boosting$opponent_ranking_last_month) / 2 
# test_boosting$heaight_dif <- (test_boosting$player_height_in_cm + test_boosting$opponent_height_in_cm) / 2 
# test_boosting$age_dif <- (test_boosting$player_age + test_boosting$opponent_age) /2 
# 
# train_boosting$rank_dif <- (train_boosting$player_ranking_last_month + train_boosting$opponent_ranking_last_month) / 2 
# train_boosting$heaight_dif <- (train_boosting$player_height_in_cm + train_boosting$opponent_height_in_cm) / 2 
# train_boosting$age_dif <- (train_boosting$player_age + train_boosting$opponent_age) /2 



# these columns ALSO need to be removed for analyzis of the 3rf and 4th datasets
# 'player_ranking_last_month', 'opponent_ranking_last_month', 'player_height_in_cm', 'opponent_height_in_cm', 'player_age', 'opponent_age'
# Remove unused variables
drop <-c("date", "sets_raw")
train_boosting <- train_boosting[,!(names(train_boosting) %in% drop)]
test_boosting <- test_boosting[,!(names(test_boosting) %in% drop)]


# Remove information about the target variable from the training data
train_boosting_target_removed <- train_boosting[,!(names(train_boosting) %in% 'number_of_played_sets')] 
test_boosting_target_removed <- test_boosting[,!(names(test_boosting) %in% 'number_of_played_sets')]

# Create lables
# As we gonna be using binary operations to predict if the game had 2 or 3 sets.
# Encode 2 sets to 1 and 3 sets to 0
train_boosting_target <- train_boosting %>% select(number_of_played_sets) %>% mutate(number_of_played_sets = ifelse(train_boosting$number_of_played_sets==2, 1,0)) # %>% select(two_or_three)
test_boosting_target <- test_boosting %>% select(number_of_played_sets)  %>% mutate(number_of_played_sets = ifelse(test_boosting$number_of_played_sets==2, 1,0)) #%>% select(two_or_three)

# Factorize the data
for (v in (train_boosting_target_removed %>% select_if(is.character) %>% names())) {
  train_boosting_target_removed[[v]] <- as.factor(train_boosting_target_removed[[v]])
}
for (v in (test_boosting_target_removed %>% select_if(is.character) %>% names())) {
  test_boosting_target_removed[[v]] <- as.factor(test_boosting_target_removed[[v]])
}

# Convert the data into a dmatrix. XGBoost algorithm uses weather data.matrix type of variables or xdb.Dmatrix
dtrain_matrix <- data.matrix(train_boosting_target_removed)
dtest_matrix <- data.matrix(test_boosting_target_removed)

# For xgboost, we'll use xgb.DMatrix to convert data table into a matrix (most recommended):
dtrain_boosting <- xgb.DMatrix(data = dtrain_matrix, label= train_boosting_target$number_of_played_sets)
dtest_boosting <- xgb.DMatrix(data = dtest_matrix, label= test_boosting_target$number_of_played_sets)

# We'll first build our model using default parameters
xgb_params_2 <- list(booster = "gbtree",            # Which booster to use. 
                     colsample_bytree = 1,          # Is the subsample ratio of columns when constructing each tree. Subsampling occurs once for every tree constructed.
                     subsample = 1,                 # Ratio of the training instances. Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees. and this will prevent overfitting.
                     max_depth = 6,                 # Maximum depth of a tree. Increasing this value will make the model more complex and more likely to overfit. 
                     eta = 0.3,                     # [0, 1] Learning or shrinkage. Step size shrinkage used in update to prevents overfitting. After each boosting step, we can directly get the weights of new features, and eta shrinks the feature weights to make the boosting process more conservative.
                     eval_metric = "auc",           # Evaluation metrics for validation data. AreaUnderCurve in our case.
                     objective = "binary:logistic", # logistic regression for binary classification, output probability
                     min_child_weight = 5)          # Minimum loss reduction required to make a further partition on a leaf node of the tree. The larger gamma is, the more conservative the algorithm will be.


# # Note : nrounds (or n_estimators or weak_learners) is a number of decision trees the algorithm will add sequentially, each attempting to correct the mistake of the learners that came before.


#  Each iteration (round) of boosting fits a single tree to the negative gradient of some loss function.
#  The algorithm does not fit an ensemble of trees at each iteration. Each tree is then added (with optimal weighting) to all prior fitted trees + optimal weights in previous iterations to come up with final predictions.

# The XGBoost cross validation process proceeds like this
# The dataset X is split into nfold subsamples, X1, X2...Xnfold.
# The XGBoost algorithm fits a boosted tree to a training dataset comprising X1, X2,...,Xnfold-1, while the last subsample (fold) Xnfold is held back as a validation1 (out-of-sample) dataset. The chosen evaluation metrics (RMSE, AUC, etc.) are calculated for both the training and validation dataset and retained.
# One subsample (fold) in the training dataset is now swapped with the validation subsample (fold), so the training dataset now comprises X1, X2, ... , Xnfold-2, Xnfold and the validation (out-of-sample) dataset is Xnfold-1. Once again, the algorithm fits a boosted tree to the training data, calculates the evaluation scores (for each chosen metric) and so on.
# This process repeats nfold times until every subsample (fold) has served both as a part of the training set and as a validation set.
# Now, another boosted tree is added and the process outlined in steps 2-4 is repeated. This continues until the total number of boosted trees being fitted to the training data is equal to nrounds
# There are now nfold calculated evaluation scores (times the number of distinct metrics chosen) for each round in nrounds for both the training sets and the validation sets (scores on the validation sets naturally tend to be worse). The means and standard deviations of the nfold scores is calculated for both the training and validation sets (times the number of distinct metrics chosen) for each round in nrounds and returned in a dataframe with nrounds rows.

#Using the inbuilt xgb.cv to calculate the best nround for this model. In addition, this function also returns CV error, which is an estimate of test error.
xgbcv <- xgb.cv( params = xgb_params_2, data = dtrain_boosting, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print.every.n = 2, early.stop.round = 50)
xgbcv$best_iteration
# Our model fitted 7 trees with the lowest AUC error

# Create XGBoost model
model_2 <- xgboost(dtrain_boosting, nrounds = 4, params = xgb_params_2)

# Plot impoetance features
importance_matrix = xgb.importance(colnames(dtrain_boosting), model = model_2)
xgb.plot.importance(importance_matrix[1:13,])

# Make some predictions on out test sample.
test_preds <- predict(model_2,dtest_boosting)

names(dtest_boosting)

#If we think about the meaning of a regression applied to our data, the numbers we get are probabilities that a datum will be classified as 1. 
#Therefore, we will set the rule that if this probability for a specific datum is > 0.5 then the observation is classified as 1 (or 0 otherwise).
err_2 <- mean(as.numeric(test_preds > 0.5) != test_boosting_target$number_of_played_sets)
print(paste("test-error=", err_2)) # "test-error= 0.333013895543843"

# Confusion Matrix
confusionMatrix (as.factor(as.numeric(test_preds > 0.5)), as.factor(test_boosting_target$number_of_played_sets))

# Test error for dataset_1 # "test-error= 0.340286054827175"
# Test error for dataset_2 # "test-error= 0.333013895543843" Was chosen because dataset_4 could improve accuracy after tuning
# Test error for dataset_3 # "test-error= 0.358760429082241"
# Test error for dataset_4 # "test-error= 0.329180642069957" 


######################################################################################################################################3
# Can we still improve our model? We will proceed to the random / grid search procedure and attempt to find better accuracy. 
# From here on, we'll be using the MLR package for model building. 
# The MLR package creates its own frame of data, learner as shown below. 
# Also mlr doesn't accept character variables. Hence, we need to convert them to factors before creating task.
# But at this time we need the target variable inside the train/test datasets.

train_boosting_tuned <- train_boosting
test_boosting_tuned <- test_boosting

# Factorizing
for (v in (train_boosting_tuned %>% select_if(is.character) %>% names())) {
  train_boosting_tuned[[v]] <- as.factor(train_boosting_tuned[[v]])
}
for (v in (test_boosting_tuned %>% select_if(is.character) %>% names())) {
  test_boosting_tuned[[v]] <- as.factor(test_boosting_tuned[[v]])
}

str(train_boosting_tuned)

train_boosting_tuned$number_of_played_sets <- as.factor(train_boosting_tuned$number_of_played_sets)
test_boosting_tuned$number_of_played_sets <- as.factor(test_boosting_tuned$number_of_played_sets)

#create tasks
traintask <- makeClassifTask (data = train_boosting_tuned,target = "number_of_played_sets")
testtask <- makeClassifTask (data = test_boosting_tuned,target = "number_of_played_sets")

#do one hot encoding
traintask <- createDummyFeatures (obj = traintask) 
testtask <- createDummyFeatures (obj = testtask)

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response", eta=0.3)
lrn$par.vals <- list( objective="binary:logistic", eval_metric="auc", nrounds=100L)

#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree")), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L),
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)

# Result: booster=gbtree; max_depth=4; min_child_weight=2.23; subsample=0.849; colsample_bytree=0.767 : acc.test.mean=0.6243036

#################################################################################

xgb_params_2_tuned <- list(booster = "gbtree",            
                           colsample_bytree = 0.767,         
                           subsample = 0.849,                
                           max_depth = 4,                
                           eta = 0.3,                    
                           eval_metric = "auc",          
                           objective = "binary:logistic", 
                           min_child_weight = 2.23
)    


xgbcv <- xgb.cv( params = xgb_params_2_tuned, data = dtrain_boosting, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 50)
xgbcv$best_iteration
# 7 --- best nrounds

model_2_tuned <- xgboost(dtrain_boosting, nrounds = 7, params = xgb_params_2_tuned)

importance_matrix_tuned = xgb.importance(colnames(dtrain_boosting), model = model_2_tuned)
xgb.plot.importance(importance_matrix_tuned[1:13,])

names(train_boosting_tuned)

test_preds_tuned <- predict(model_2_tuned,dtest_boosting)
confusionMatrix (as.factor(as.numeric(test_preds_tuned > 0.5)), as.factor(test_boosting_target$number_of_played_sets))
err_2_tuned <- mean(as.numeric(test_preds_tuned > 0.5) != test_boosting_target$number_of_played_sets)
print(paste("test-error=", err_2_tuned)) # "test-error= 0.31919296430419"
# "test-error= 0.326305701964542" for the 4th dataset


#The Receiver Operator Characteristic (ROC) curve is an evaluation metric for binary classification problems. 
#It is a probability curve that plots the TPR against FPR at various threshold values and essentially separates the 'signal' from the 'noise'. 
#The Area Under the Curve (AUC) is the measure of the ability of a classifier to distinguish between classes and is used as a summary of the ROC curve.
#The higher the AUC, the better the performance of the model at distinguishing between the positive and negative classes.

myroc = roc(test_boosting_target$number_of_played_sets, test_preds_tuned)
auc <- round(auc(test_boosting_target$number_of_played_sets, test_preds_tuned),4)
ggroc(myroc, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed")

xgb.plot.multi.trees(feature_names=names(test_boosting_target_removed), model=model_2_tuned)

################################################################################################################################################



####################################################################
######################  Redes Neuronais ############################
####################################################################


# Depois de fazer modelos de regressão logistica, bagging, boosting e random forest,
#o data set que teve recorrententemente melhores resultados foi o Train_4thTry e respetivo conjunto de teste

#desta forma, vamos aplicar o modelo das redes neuronais a este conjunto




#############################################
#########   4 DATASET     ###################
#############################################



str(Train_4thTry)
#
train_RN<- Train_4thTry[-c(1,2,3,4,5)]

train_RN <-select(train_RN,2,3,4,5,6,7,8,9,10,1)

str(train_RN)

str(Test_4thTry)
test_RN<- Test_4thTry[-c(1,2,3,4,5)]

test_RN <-select(test_RN,2,3,4,5,6,7,8,9,10,1)

str(test_RN)


#retirar a variavel target para um dataset auxiliar
#atravez deste dataset vamos verificar se as variaveis são independentes ou multicolineares
train_RN_correlation <-test_RN[-c(10)]

#função que verifica se as variaveis são multicolineares
round(cor(train_RN_correlation),3)

#As variaveis tourney_category_dummy e average_player_ranking eram multicolineares, com uma multicolinearidade de 0.679
#Tinhamos de retirar uma das variaveis,
#escolheu-se retirar a variavel average_player_ranking
train_RN <-train_RN[-c(6)]
test_RN <-test_RN[-c(6)]


train_RN$number_of_played_sets <- as.factor(train_RN$number_of_played_sets)
test_RN$number_of_played_sets <- as.factor(test_RN$number_of_played_sets)

str(train_RN)


# scale the data
train_RN_scale <- scale(train_RN[-9])
test_RN_scale <- scale(test_RN[-9])




# Add back the outcome variable
train_RN_scale <- cbind(train_RN_scale, train_RN[9])
test_RN_scale <- cbind(test_RN_scale, test_RN[9])

str(train_RN_scale)

#######################################################3


RN_1_try <- neuralnet(number_of_played_sets ~ .,
                      data=train_RN_scale,hidden=4,
                      act.fct = 'logistic', linear.output = FALSE)
plot(RN_1_try)

# Make predictions on the test data.
my.pred<- predict(RN_1_try,test_RN_scale)

# Make the table needed to create the confusion matrix.
my.results <-data.frame(Predictions =my.pred)

# ifelse converts probabilites to factor values. 
# Use column 1 of my.results for the ifelse statement.
# Column 1 values are probilites for tested_negative



my.predList<- ifelse(my.results[1] > 0.5,1,2) 
my.predList
# Structure the confusion matrix
my.predList <- factor(my.predList,level=c(1,2),labels=c("2","3"))

str(test_RN_scale)

confusionMatrix(my.predList,test_RN_scale$number_of_played_sets)



#################################################################################################

RN_2_try <- neuralnet(number_of_played_sets ~ .,
                      data=train_RN_scale,hidden=6,
                      act.fct = 'logistic', linear.output = FALSE)
plot(RN_2_try)



# Make predictions on the test data.
my.pred<- predict(RN_2_try,test_RN_scale)

# Make the table needed to create the confusion matrix.
my.results <-data.frame(Predictions =my.pred)

# ifelse converts probabilites to factor values. 
# Use column 1 of my.results for the ifelse statement.
# Column 1 values are probilites for tested_negative



my.predList<- ifelse(my.results[1] > 0.5,1,2) 
my.predList
# Structure the confusion matrix
my.predList <- factor(my.predList,level=c(1,2),labels=c("2","3"))

str(test_RN_scale)

confusionMatrix(my.predList,test_RN_scale$number_of_played_sets)


#################################################################################################


# Balanceamento 3Dataset
set.seed(200)
train_down_RN <- downSample(train_RN_scale[,-9],train_RN_scale[,9]) 

names(train_down_RN)[9] <- "number_of_played_sets"


# Representação gráfica da variável alvo (y) com os dados do conjunto train após o balanceamento (train_down1_LOG)
plot(train_down_RN$number_of_played_sets,main = "Variável Alvo (number_of_played_sets) - Dados de Treino após Balanceamento (under-sampling)",
     xlab = "Valores da variável number_of_played_sets", ylab = "nº de observações",col = "orange")

summary(train_down_RN)


RN_3_try <- neuralnet(number_of_played_sets ~ .,
                      data=train_down_RN,hidden=4,
                      act.fct = 'logistic', linear.output = FALSE)
plot(RN_3_try)



# Make predictions on the test data.
my.pred<- predict(RN_3_try,test_RN_scale)

# Make the table needed to create the confusion matrix.
my.results <-data.frame(Predictions =my.pred)

# ifelse converts probabilites to factor values. 
# Use column 1 of my.results for the ifelse statement.
# Column 1 values are probilites for tested_negative



my.predList<- ifelse(my.results[1] > 0.5,1,2) 
my.predList
# Structure the confusion matrix
my.predList <- factor(my.predList,level=c(1,2),labels=c("2","3"))

str(test_RN_scale)

confusionMatrix(my.predList,test_RN_scale$number_of_played_sets)


######################################################################################



RN_4_try <- neuralnet(number_of_played_sets ~ .,
                      data=train_down_RN,hidden=6,
                      act.fct = 'logistic', linear.output = FALSE)
plot(RN_4_try)



# Make predictions on the test data.
my.pred<- predict(RN_4_try,test_RN_scale)

# Make the table needed to create the confusion matrix.
my.results <-data.frame(Predictions =my.pred)

# ifelse converts probabilites to factor values. 
# Use column 1 of my.results for the ifelse statement.
# Column 1 values are probilites for tested_negative



my.predList<- ifelse(my.results[1] > 0.5,1,2) 
my.predList
# Structure the confusion matrix
my.predList <- factor(my.predList,level=c(1,2),labels=c("2","3"))

str(test_RN_scale)

confusionMatrix(my.predList,test_RN_scale$number_of_played_sets)
