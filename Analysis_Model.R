#Instalar pacotes
install.packages(c("tidyverse","ggplot2","tree", "rpart", "rpart.plot","ggrepel","GGally","data.table","scales","plyr","dplyr"))

#Carregar pacotes
#Manipulação dos dados
library(tidyverse) 
library(data.table) 
library(dplyr)
library(plyr)
#Visualização dos dados
library(ggplot2) 
library(ggrepel)
library(GGally)
library(scales)
#Biblioteca para gerar a árvore de decisão
library(tree) 
library(rpart)
library(rpart.plot)

#Carregar o dataset
movie <- read_csv("movie_metadata.csv")

#--Exploração do dataset
#Visualizar o dataset
View(movie)
#A função str retorna a estrutura e os tipos do dataset 
str(movie)
#Visualizar o nome das colunas do dataset
colnames(movie)
#Verificar os valores ausentes
Valores_missing <- aggr(movie, sortVars = T, prop = T, sortCombs = T,
                        cex.lab = 1.5, cex.axis = .6, cex.numbers = 5,
                        combined = F, gap = -.2)

#--ANALISE IMDB SCORES
#Distribuição IMDB Scores
imdb_score = as.data.table(subset(movie, movie$imdb_score >= 0 & !is.na(movie$imdb_score)))
ggplot(imdb_score, aes(x=imdb_score)) + 
  geom_histogram(aes(fill=..count..), binwidth = 0.1) + 
  xlab("IMDB Score") + 
  ylab("Frequência") + 
  ggtitle("Distribuição IMDB Scores") + 
  geom_vline(aes(xintercept=mean(imdb_score, na.rm=T)), color="black", linetype="dashed", size=1) +
  scale_fill_gradient("Frequência", low = "blue", high = "red") +
  scale_x_continuous(breaks = seq(0, 10, 0.5)) 

#--ANALISE DE FILMES
#Filmes mais votados pelos os usuários
movie %>% top_n(30, wt=num_voted_users) %>%
  ggplot(aes(x=reorder(movie_title, num_voted_users), y=num_voted_users)) +
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0, 15000)) +
  labs(x=" ", y="Número de votos") +
  geom_text(aes(label=num_voted_users), hjust=-0.1, size=3) +
  geom_text(aes(label=num_voted_users), y=1000, size=3, col="white")

#Distribuição de filmes por país
country <- as.data.frame(table(movie$country))
country <- arrange(country, desc(Freq))
ggplot(head(country, 20), aes(x = reorder(factor(Var1), Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  labs(x = "Países", y = "Número de filmes") + 
  ggtitle(" Quantidade de filmes por país") + 
  coord_flip() + 
  theme_get()

#--ANALISE DO ELENCO
#Atores com as maiores avaliações
#Calcular a classificação média e SE para cada ator principal
df_med_imdb_score <- ddply(movie, c("actor_1_name"), summarise,
                           M = mean(imdb_score, na.rm=T),
                           SE = sd(imdb_score, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                           N = length(na.omit(imdb_score)))
med_imdb_score <-df_med_imdb_score[which(df_med_imdb_score$N>=20),]
#Transformar o ator em um fator ordenado
med_imdb_score$actor_1_name <- factor(med_imdb_score$actor_1_name)
#Ordenar pela classificação das médias do IMDB SCORES
med_imdb_score$actor_1_name <- reorder(med_imdb_score$actor_1_name, med_imdb_score$M)
#Plotar o gráfico
ggplot(med_imdb_score, aes(x = M, xmin = M-SE, xmax = M+SE, y = actor_1_name )) +
  geom_point() + 
  geom_segment( aes(x = M-SE, xend = M+SE,
                    y = actor_1_name, yend=actor_1_name)) +
  theme(axis.text=element_text(size=7)) +
  xlab("Média de Avaliações") + ylab("Atores") + ggtitle(' Atores mais bem avaliados')

#DIRETOR
#Diretores com as maiores avaliações
#Calcular a classificação média e SE para cada diretor
df_med_imdb_score <- ddply(movie, c("director_name"), summarise,
                           M = mean(imdb_score, na.rm=T),
                           SE = sd(imdb_score, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                           N = length(na.omit(imdb_score)))
med_imdb_score <- df_med_imdb_score[which(df_med_imdb_score$N>=10 & !(df_med_imdb_score$director_name=='')),]
#Transformar o ator em um fator ordenado
med_imdb_score$director_name <- factor(med_imdb_score$director_name)
#Ordenar pela classificação das médias do IMDB SCORES
med_imdb_score$director_name <- reorder(med_imdb_score$director_name, med_imdb_score$M)
#Plotar o gráfico
ggplot(med_imdb_score, aes(x = M, xmin = M-SE, xmax = M+SE, y = director_name )) +
  geom_point() + 
  geom_segment( aes(x = M-SE, xend = M+SE,
                    y = director_name, yend=director_name)) +
  theme(axis.text=element_text(size=8)) +
  xlab("Média de Avaliaçõe") + ylab("Diretores") + ggtitle(' Diretores mais bem avaliados')

#LUCRO
#Adicionar a coluna de lucro
movie <- movie %>% 
  mutate(lucro = gross - budget)
colnames(movie)

#Top 20 melhores filmes com base em seu lucro
movie %>% 
  filter(title_year %in% c(2000:2016)) %>%
  arrange(desc(lucro)) %>%
  top_n(20, lucro) %>%
  #Plotar o gráfico
  ggplot(aes(x=budget/1000000, y = lucro/1000000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Orçamento em milhões", y = "Lucro em milhões", title = "Top 10 filmes mais lucrativos") +
  theme(plot.title = element_text(hjust = 0.5))

#Matriz de correlação
ggcorr(movie, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

#ARVORE DE DECISÃO
df_filme_tree <- read.csv("movie_metadata.csv",header = T,stringsAsFactors = F)

#Remover valores ausentes
colSums(sapply(df_filme_tree, is.na))
df_filme_tree <- df_filme_tree[!is.na(df_filme_tree$gross), ]
df_filme_tree <- df_filme_tree[!is.na(df_filme_tree$budget), ]
dim(df_filme_tree)
sum(complete.cases(df_filme_tree))

#Extrair apenas as variáveis numéricas 
columns <- c()
for(i in 1:dim(df_filme_tree)[2])
{
  if(is.numeric(df_filme_tree[,i])|| is.integer(df_filme_tree[,i]))
  {
    columns[i]=T
  }
  else
  {
    columns[i]=F
  }
}
#Armazenar as váriaveis númericas na váriavel temp
temp <- na.omit(df_filme_tree[,columns])

#MODELO
#Dividir o dataset em dados de treinamento e dados para teste
set.seed(2)
train <- sample(dim(temp)[1],dim(temp)[1]*0.9)
temp_train <- temp[train,]
temp_test <- temp[-train,]
#Colunas para a construção do modelo
lmfit = lm(imdb_score~num_voted_users+duration+gross,data=temp_train)
summary(lmfit)

#A função rpart criará a árvore de decisão com uma expressão de entrada e a base de dados que utilizaremos para criar o modelo
set.seed(3)
m.rpart <- rpart(imdb_score~.,data=temp_train)
m.rpart
#A função rpart.plot gera a visualização da árvore de decisão 
rpart.plot(m.rpart,digits = 3)

#Testar o modelo 
#Aplicar o modelo gerado no conjunto de teste
p.rpart <- predict(m.rpart,temp_test)

#Como os valores reais e previstos diferem? Visualizamos isso pelo uso de histogramas
tree_dataframe <- data.frame(p.rpart,temp_test$imdb_score)
#Plotar a diferença dos valores reais e previstos
#Histograma do rpart
ggplot(tree_dataframe, aes(x=p.rpart)) + geom_histogram(fill="black", colour="black")
#Histograma do conjunto de teste
ggplot(tree_dataframe, aes(x=temp_test.imdb_score)) + geom_histogram(fill="black", colour="black")

#Correlação entre os valores previstos e o valor real
cor(p.rpart,temp_test$imdb_score)

#Calculo do erro quadrático médio
mean((p.rpart-temp_test$imdb_score)^2)
