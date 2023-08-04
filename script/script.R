# Bibliotecas necessárias
library(readr)
library(corrplot)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(Metrics)
library(caret)
library(class)
library(FNN)
library(nortest)
library(moments)

# Cores para os gráficos
PaleBlue<- rgb(171,196,255, maxColorValue = 255)
Blue2<- rgb(136,143,205, maxColorValue = 255)
Blue3<- rgb(98,109,167, maxColorValue = 255)
Blue4<- rgb(61,74,129, maxColorValue = 255)

# Função para calcular o RMSE
RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

# Função para calcular o MAE
MAE <- function(test, predicted) {
  mean(abs(test - predicted))
}


# Carregar o ficheiro com os dados (ciclismo.csv) - Rita Ariana (1201386)
setwd("/home/rita/Desktop/dev/anadi/anadi_pdn_3de3df_1201518_1201386_1202016/dataset")

# Carregar o ficheiro com os dados (ciclismo.csv) - Ana Albergaria (1201518)
setwd("C:/Users/Ana.Albergaria/OneDrive - konkconsulting/ANADI/anadi_pdn_3de3df_1201518_1201386_1202016/dataset")

# Carregar o ficheiro com os dados (ciclismo.csv) - Vasco Azevedo (1202016)
setwd("C:/Users/Vasco/Documents/anadi_pdn_3de3df_1201518_1201386_1202016/dataset")

ciclismo <- read.csv("ciclismo.csv", header = FALSE)

# Definir os nomes das colunas
colnames(ciclismo) <- c( 'ID', 'gender', 'Team', 'Background', 'ProLevel', 'WinterTrainingCamp', 'altitude_results',
'vo2_results', 'hr_results', 'dob', 'Continent')

# Remover a primeira linha (cabeçalho)
ciclismo <- ciclismo[-1,]

######################################### Análise dos Dados #########################################

# Quantidade de linhas e colunas do dataset
# dim(ciclismo)

nrow(ciclismo)
ncol(ciclismo)

# Obter um sumário dos dados - análise descritiva

summary(ciclismo)
str(ciclismo)

######################################### Pré-Processamento #########################################

# Derivar o atributo Age a partir do atributo dob (ex: 1988-01-09 - YYYY-MM-DD)
ciclismo$age <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(as.Date(ciclismo$dob), "%Y")) - 
  (as.numeric(format(Sys.Date(), "%m%d")) < as.numeric(format(as.Date(ciclismo$dob), "%m%d")))

# Transformar os atributos altitude_results, vo2_results e hr_results em numéricos
ciclismo$altitude_results <- as.numeric(ciclismo$altitude_results)
ciclismo$vo2_results <- as.numeric(ciclismo$vo2_results)
ciclismo$hr_results <- as.numeric(ciclismo$hr_results)

################################ Análise dos Dados Mais Significativos ##############################

head(ciclismo, 4)
summary(ciclismo)
str(ciclismo)


# analisar atributos categóricos

# gender - como é um atributo binário foi transformado em numérico
barplot(table(ciclismo$gender), col = c(Blue2, Blue3), main = "Género", xlab = "Género", ylab = "Frequência")
round(prop.table(table(ciclismo$gender)), 2)
ciclismo$gender= as.numeric(as.factor(ciclismo$gender)) - 1 # 0 - Female; 1 - Male

# Team - a informação não se encontra muito balanceada
# Pouca informação sobre os ciclistas da equipa A e E quando comparado com a equipa C
barplot(table(ciclismo$Team), col = c(Blue2, Blue3, Blue4), main = "Team", xlab = "Team", ylab = "Frequência")
round(prop.table(table(ciclismo$Team)), 2) # proporção de cada equipa

# Background
# Poucos dados relativos ao Mountain quando comparado com os outros backgrounds
barplot(table(ciclismo$Background), col = c(Blue2, Blue3, Blue4), main = "Background", xlab = "Background", ylab = "Frequência")
round(prop.table(table(ciclismo$Background)), 2) 

# ProLevel - como é um atributo binário foi transformado em numérico
barplot(table(ciclismo$ProLevel), col = c(Blue2, Blue3), main = "ProLevel", xlab = "ProLevel", ylab = "Frequência")
round(prop.table(table(ciclismo$ProLevel)), 2) 
ciclismo$ProLevel = as.numeric(as.factor(ciclismo$ProLevel)) - 1 # 0 - Continental; 1 - World Tour

# WinterTrainingCamp - como é um atributo binário foi transformado em numérico
barplot(table(ciclismo$WinterTrainingCamp), col = c(Blue2, Blue3), main = "WinterTrainingCamp", xlab = "WinterTrainingCamp", ylab = "Frequência")
round(prop.table(table(ciclismo$WinterTrainingCamp)), 2) 
ciclismo$WinterTrainingCamp[ciclismo$WinterTrainingCamp == "completed"] <- 1
ciclismo$WinterTrainingCamp[ciclismo$WinterTrainingCamp == "none"] <- 0
ciclismo$WinterTrainingCamp <- as.numeric(ciclismo$WinterTrainingCamp)

# Continent - divisão dos dados muito balanceada
barplot(table(ciclismo$Continent), col = c(Blue2, Blue3, Blue4), main = "Continent", xlab = "Continent", ylab = "Frequência")
round(prop.table(table(ciclismo$Continent)), 2) 

# Análise atributos numéricos

# altitude_results

plot(ciclismo$altitude_results)
plot(density(ciclismo$altitude_results),
     main = "altitude_results",
     xlab = "altitude_results")
hist(ciclismo$altitude_results, main = "ciclismo$altitude_results", xlab = "ciclismo$altitude_results")
t.test(ciclismo$altitude_results)

# vo2_results

plot(ciclismo$vo2_results)
plot(density(ciclismo$vo2_results),
     main = "vo2_results",
     xlab = "vo2_results")
hist(ciclismo$vo2_results, main = "ciclismo$vo2_results", xlab = "ciclismo$vo2_results")
t.test(ciclismo$vo2_results)

# hr_results

plot(ciclismo$hr_results)
plot(density(ciclismo$hr_results),
     main = "hr_results",
     xlab = "hr_results")
hist(ciclismo$hr_results, main = "ciclismo$hr_results", xlab = "ciclismo$hr_results")
t.test(ciclismo$hr_results)

# age
plot(ciclismo$age)
plot(density(ciclismo$age),
     main = "age",
     xlab = "age")
hist(ciclismo$age, main = "ciclismo$age", xlab = "ciclismo$age")

# relacionar altitude_results com vo2_results 
plot(ciclismo$altitude_results, ciclismo$vo2_results, main = "altitude_results vs vo2_results", xlab = "altitude_results", ylab = "vo2_results")

# relacionar altitude_results com hr_results
plot(ciclismo$altitude_results, ciclismo$hr_results, main = "altitude_results vs hr_results", xlab = "altitude_results", ylab = "hr_results")

# relacionar altitude_results com age
plot(ciclismo$altitude_results, ciclismo$age, main = "altitude_results vs age", xlab = "altitude_results", ylab = "age")

# relacionar vo2_results com hr_results
plot(ciclismo$vo2_results, ciclismo$hr_results, main = "vo2_results vs hr_results", xlab = "vo2_results", ylab = "hr_results")

# relacionar vo2_results com age
plot(ciclismo$vo2_results, ciclismo$age, main = "vo2_results vs age", xlab = "vo2_results", ylab = "age")

# relacionar hr_results com age
plot(ciclismo$hr_results, ciclismo$age, main = "hr_results vs age", xlab = "hr_results", ylab = "age")

######################################### Pré-Processamento #########################################

# Identificação e remoção de NA's
if(sum(is.na(ciclismo)) > 0) {
  ciclismo <- na.omit(ciclismo)
}

# Identificação de outliers e valores inconsistentes

## altitude_results, vo2_results, hr_results
boxplot(ciclismo[,c(7,8,9)], col = c(Blue2, Blue3, Blue4))

## Visualização dos outliers
boxplot.stats(ciclismo$altitude_results)$out
boxplot.stats(ciclismo$vo2_results)$out
boxplot.stats(ciclismo$hr_results)$out

# Mostrar as linhas com outliers
ciclismo[ciclismo$altitude_results %in% boxplot.stats(ciclismo$altitude_results)$out,]
ciclismo[ciclismo$vo2_results %in% boxplot.stats(ciclismo$vo2_results)$out,]
ciclismo[ciclismo$hr_results %in% boxplot.stats(ciclismo$hr_results)$out,]

# Seleção de atributos
ciclismo <- subset(ciclismo, select = -c(ID, dob))

# One hot encoding 
colunas_categoricas <- c("Team", "Background", "Continent")

dummy_obj <- dummyVars(~., data = ciclismo[, colunas_categoricas])

dados_encoded <- predict(dummy_obj, newdata = ciclismo)

# Transformar os dados encoded em dataframe
ciclismo_encoded <- as.data.frame(dados_encoded)

# Adicionar os atributos numéricos do dataframe ciclismo ao dataframe ciclismo_encoded
ciclismo_encoded$altitude_results <- ciclismo$altitude_results
ciclismo_encoded$vo2_results <- ciclismo$vo2_results
ciclismo_encoded$hr_results <- ciclismo$hr_results
ciclismo_encoded$age <- ciclismo$age
ciclismo_encoded$gender= ciclismo$gender
ciclismo_encoded$ProLevel = ciclismo$ProLevel
ciclismo_encoded$WinterTrainingCamp = ciclismo$WinterTrainingCamp

summary(ciclismo_encoded)

# Label encoding 
ciclismo.encoded <- ciclismo

ciclismo.encoded$Team= as.numeric(as.factor(ciclismo.encoded$Team)) - 1
ciclismo.encoded$Background = as.numeric(as.factor(ciclismo.encoded$Background)) - 1
ciclismo.encoded$Continent = as.numeric(as.factor(ciclismo.encoded$Continent)) - 1

summary(ciclismo.encoded)


# Normalizar os dados
normalise <- function(y) {
  (y - min(y)) / (max(y) - min(y))
}

# normalizar os dados
ciclismo.norm <- as.data.frame(lapply(ciclismo_encoded, normalise))

# Desnormalizar os dados
minmaxdesnorm <- function(x, goal.attrib) {
  return (x * (max(goal.attrib) - min(goal.attrib)) + min(goal.attrib))
}

############################################# Regressão #############################################

################################################# 5 #################################################

# Matriz de correlação

ciclismo.norm <- as.data.frame(lapply(ciclismo.encoded, normalise))

corrplot(cor(ciclismo.norm), method = "color",
         type = "lower", order = "hclust",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8,
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200))

################################################# 6 #################################################

# garantir a reprodutibilidade dos resultados
set.seed(123)

# dataset exercício 6
datasetEx6 <- ciclismo.encoded 

# Dividir o dataset em 70% para treino e 30% para teste - Critério Holdout

sample <- sample(c(TRUE, FALSE), nrow(datasetEx6), replace=TRUE, prob=c(0.7,0.3))
datasetEx6.train <- datasetEx6[sample,]
datasetEx6.test <- datasetEx6[!sample,]

#Verificar se a divisão não enviesou os dados
summary(datasetEx6.train$altitude_results)
summary(datasetEx6.test$altitude_results)


# Modelo de regressão linear simples

slr.model <- lm(altitude_results ~ hr_results, data = datasetEx6.train)
summary(slr.model)  # altitude_results = 14,4220 + ( 0.7636 * hr_results )

# Visualizar a reta correspondente ao modelo de regressão linear simples e o respetivo diagrama de dispersão

plot(datasetEx6.train$hr_results, datasetEx6.train$altitude_results, col = Blue2, main = "altitude_results ~ hr_results", xlab = "hr_results", ylab = "altitude_results")
abline(slr.model$coefficients[1],slr.model$coefficients[2], col='red')

# Calcular o erro médio absoluto (MAE) e a raiz quadrada do erro médio (RMSE) do modelo

slr.pred <- predict(slr.model, datasetEx6.test)

slr.mae <- MAE(slr.pred, datasetEx6.test$altitude_results)
slr.rmse <- RMSE(slr.pred, datasetEx6.test$altitude_results)

# determinar a amplitude interquartil (IQR) do atributo altitude_results no datasetEx6
iqr <- IQR(datasetEx6$altitude_results) # 20
q3 <- quantile(datasetEx6$altitude_results, 0.75) # 77
q1 <- quantile(datasetEx6$altitude_results, 0.25) # 57
iqr <- q3 - q1 # 20


# Modelo de regressão linear múltipla - com todos os atributos 

mlr.model <- lm(altitude_results ~ ., 
            data = datasetEx6.train)

summary(mlr.model)

mlr.pred <- predict(mlr.model,datasetEx6.test)

mlr.mae <- MAE(mlr.pred, datasetEx6.test$altitude_results)
mlr.rmse <- RMSE(mlr.pred, datasetEx6.test$altitude_results)

# Modelo de regressão linear múltipla - sem os atributos que não apresentam significância estatística (age, background)

mlr.model2 <- lm(altitude_results ~ gender + vo2_results + hr_results + ProLevel + WinterTrainingCamp + Team + Continent, 
            data = datasetEx6.train)

summary(mlr.model2)

mlr.pred <- predict(mlr.model2,datasetEx6.test)

mlr.mae <- MAE(mlr.pred, datasetEx6.test$altitude_results)
mlr.rmse <- RMSE(mlr.pred, datasetEx6.test$altitude_results)


# Árvore de regressão 

tree.model <- rpart(altitude_results ~ ., method="anova", data=datasetEx6.train)

rpart.plot(tree.model)
rpart.plot(tree.model, digits = 3, fallen.leaves = TRUE, type = 3)
rpart.plot(tree.model, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

rpart.plot(tree.model, digits=3, cex=0.85, main="Árvore de Regressão")


tree.pred <- predict(tree.model, datasetEx6.test)

tree.mae <- MAE(tree.pred, datasetEx6.test$altitude_results)
tree.rmse <- RMSE(tree.pred, datasetEx6.test$altitude_results)


# Rede neuronal

datasetEx6.norm <- as.data.frame(lapply(datasetEx6, normalise))

sample <- sample(c(TRUE, FALSE), nrow(datasetEx6.norm), replace=TRUE, prob=c(0.7,0.3))
datasetEx6.norm.train <- datasetEx6.norm[sample,]
datasetEx6.norm.test <- datasetEx6.norm[!sample,]


#-------------------------------
# 3 internal nodes
numnodes <- 3

#-------------------------------
# 2 internal levels: 3,2 nodes
# numnodes <- c(3, 2)

#-------------------------------
# 2 internal levels: 6,2 nodes
# numnodes <- c(6, 2)

nn.model <-
  neuralnet(
    altitude_results ~ .,
    data = datasetEx6.norm.train,
    hidden = numnodes
  )
plot(nn.model)

nn.pred <- compute(nn.model, datasetEx6.norm.test)

# Desnormalizar os dados para avaliar o modelo 

nn.pred.altitude <- minmaxdesnorm(nn.pred$net.result,datasetEx6$altitude_results)
test.altitude <- minmaxdesnorm(datasetEx6.norm.test$altitude_results,datasetEx6$altitude_results)

nn.mae <- MAE(nn.pred.altitude, test.altitude)
nn.rmse <- RMSE(nn.pred.altitude, test.altitude)

############################################## 7 e 8 ##############################################

# garantir a reprodutibilidade dos resultados
set.seed(123)

# dataset exercício 7
datasetEx7 <- ciclismo.encoded 

# Dividir o dataset em 70% para treino e 30% para teste - Critério Holdout

sample <- sample(c(TRUE, FALSE), nrow(datasetEx7), replace=TRUE, prob=c(0.7,0.3))
datasetEx7.train <- datasetEx7[sample,]
datasetEx7.test <- datasetEx7[!sample,]

# Regressão linear múltipla - com todos os atributos

mlr.model <- lm(vo2_results ~ ., 
            data = datasetEx7.train)

summary(mlr.model)

mlr.pred <- predict(mlr.model,datasetEx7.test)

mlr.mae <- MAE(mlr.pred, datasetEx7.test$vo2_results)
mlr.rmse <- RMSE(mlr.pred, datasetEx7.test$vo2_results)

# Regressão linear múltipla - sem os atributos que não apresentam significância estatística (age, background)

mlr.model2 <- lm(vo2_results ~ hr_results + altitude_results + WinterTrainingCamp + ProLevel + Continent + Team + gender, 
            data = datasetEx7.train)

summary(mlr.model2)

mlr.pred <- predict(mlr.model2,datasetEx7.test)

mlr.mae <- MAE(mlr.pred, datasetEx7.test$vo2_results)
mlr.rmse <- RMSE(mlr.pred, datasetEx7.test$vo2_results)


# Árvore de regressão

tree.model <- rpart(vo2_results ~ ., method="anova", data=datasetEx7.train)

rpart.plot(tree.model)
rpart.plot(tree.model, digits = 3, fallen.leaves = TRUE, type = 3)
rpart.plot(tree.model, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

rpart.plot(tree.model, digits=3, cex=0.85, main="Árvore de Regressão")

tree.pred <- predict(tree.model, datasetEx7.test)

tree.mae <- MAE(tree.pred, datasetEx7.test$vo2_results)
tree.rmse <- RMSE(tree.pred, datasetEx7.test$vo2_results)


# Rede neuronal

datasetEx7.norm <- as.data.frame(lapply(datasetEx7, normalise))

datasetEx7.norm.train <- datasetEx7.norm[sample,]
datasetEx7.norm.test <- datasetEx7.norm[!sample,]

#-------------------------------
# 1 internal node
numnodes <- 1

#-------------------------------
# 3 internal nodes
# numnodes <- 3

#-------------------------------
# 2 internal levels: 3,2 nodes
# numnodes <- c(3, 2)

#-------------------------------
# 2 internal levels: 6,2 nodes
# numnodes <- c(6, 2)


nn.model <-
  neuralnet(
    vo2_results ~ .,
    data = datasetEx7.norm.train,
    hidden = numnodes
  )
plot(nn.model)

nn.pred <- compute(nn.model, datasetEx7.norm.test)

# Desnormalizar os dados para avaliar o modelo 

nn.pred.vo2 <- minmaxdesnorm(nn.pred$net.result,datasetEx7$vo2_results)
test.vo2 <- minmaxdesnorm(datasetEx7.norm.test$vo2_results,datasetEx7$vo2_results)

nn.mae <- MAE(nn.pred.vo2, test.vo2)
nn.rmse <- RMSE(nn.pred.vo2, test.vo2)

################################################ 9 ################################################

# Verificar se os resultados obtidos são estatisticamente significativos (nivel de significância de 5%)
# Modelo de regressão linear múltipla e modelo de rede neuronal

# H0: resultados obtidos não são estatisticamente significativos
# H1: resultados obtidos são estatisticamente significativos

nn <- c(nn.mae,nn.rmse)
mlr <- c(mlr.mae,mlr.rmse)

test <- t.test(nn,mlr); test
# p-value =0.9266 > 0.05 , não rejeitamos H0 - resultados obtidos não são estatisticamente significativos

# Modelo que apresenta melhor desempenho

# O modelo com melhor desempenho é a Regressão Linear Múltipla, pois é o que tem MAE e RMSE menores.


########################################### Classificação #########################################

################################################ 1 ################################################

# capacidade preditiva relativamente ao atributo “Pro_level”

# garantir a reprodutibilidade dos resultados
set.seed(123)

# dataset exercício 1
datasetEx1 <- ciclismo_encoded
datasetEx1.norm <- as.data.frame(lapply(datasetEx1, normalise))


sample <- sample(c(TRUE, FALSE), nrow(datasetEx1.norm), replace=TRUE, prob=c(0.7,0.3))
datasetEx1.train <- datasetEx1.norm[sample,]
datasetEx1.test <- datasetEx1.norm[!sample,]

# Árvore de Decisão  - com todos os atributos

tree.model = rpart(ProLevel ~ . , data = datasetEx1.train, method = "class")

rpart.plot(
  tree.model,
  digits = 3,
  fallen.leaves = TRUE,
  type = 3
)

rpart.plot(tree.model, digits=3, cex=0.85, main="Árvore de Decisão")


tree.pred = predict(tree.model, datasetEx1.test, type = 'class')

#confusionMatrix
m.conf <- table(datasetEx1.test$ProLevel, tree.pred)
parse_results(m.conf)

plot(tree.pred)

# Rede Neuronal

datasetEx1.norm <- as.data.frame(lapply(datasetEx1, normalise))

colnames(datasetEx1.norm) <- c( 'GroupA', 'GroupB', 'GroupC', 'GroupD', 'GroupE', 'Cobblestones', 'Hill',
                           'Mountain','NoneBackground','Sprinter','TimeTrial','Africa','Asia','Australia',
                           'Europe','NorthAmerica','SouthAmerica','altitude_results','vo2_results', 'hr_results', 'age', 'gender','ProLevel',
                           'WinterTrainingCamp')

datasetEx1.norm.train <- datasetEx1.norm[sample,]
datasetEx1.norm.test <- datasetEx1.norm[!sample,]

train_labels <- datasetEx1.norm[sample, "ProLevel"]
test_labels <- datasetEx1.norm[!sample, "ProLevel"]


#-------------------------------
# 1 internal node
# numnodes <- 1

#-------------------------------
# 2 internal nodes
# numnodes <- 2

#-------------------------------
# 3 internal nodes
# numnodes <- 3

#-------------------------------
# 2 internal levels: 3,2 nodes
numnodes <- c(3, 2)


nn.model <-
  neuralnet(
    ProLevel ~ gender + vo2_results + hr_results + age + altitude_results + WinterTrainingCamp + GroupA + GroupB + GroupC + GroupD + GroupE + Cobblestones + Hill + Mountain + NoneBackground + Sprinter + TimeTrial + Africa + Asia + Australia + Europe + NorthAmerica + SouthAmerica,
    data = datasetEx1.norm.train,
    hidden = numnodes,
    stepmax=1e7
  )
plot(nn.model)

nn.pred <- predict(nn.model, datasetEx1.norm.test)
nn.pred <- ifelse(nn.pred > 0.5, "1", "0")

accuracy_nn <- sum(nn.pred == datasetEx1.norm.test$ProLevel) / length(datasetEx1.norm.test$ProLevel) * 100
m.conf <- table(datasetEx1.norm.test$ProLevel, nn.pred)
parse_results(m.conf)


# KNN

k <- c()
accuracy <- c()
for (i in seq(1, 50, 2)){
  
  knn.pred <- knn(train = datasetEx1.norm.train,
                  test = datasetEx1.norm.test,
                  cl = train_labels, k=i) 
  
  cfmatrix <- table(test_labels, knn.pred)
  
  accuracy <- c(accuracy, sum(diag(cfmatrix))/sum(cfmatrix))
  
  k <- c(k,i)
}

resNeigh <- data.frame(k, accuracy)
resNeigh[resNeigh$accuracy == max(resNeigh$accuracy), ]
plot(resNeigh$k, resNeigh$accuracy)

k[which.max(accuracy)]
plot(
  resNeigh$k,
  resNeigh$accuracy,
  col = ifelse(
    resNeigh$accuracy == max(resNeigh$accuracy),
    'orangered1',
    'steelblue4'
  )
)

# modelo knn

model_knn <- knn(train = datasetEx1.norm.train,
                 test = datasetEx1.norm.test,
                 cl = train_labels,
                 k = k[which.max(accuracy)]
                 ) 
# Confusion Matrix
m.conf <- table(datasetEx1.norm.test$ProLevel, model_knn)
parse_results(m.conf)


# 2 melhores modelos - k-fold cross validation (knn e árvore de decisão)

cvf <- 10
folds <- sample(1:cvf, nrow(datasetEx1), replace = TRUE)

#Fold size
table(folds)

k=k[which.max(accuracy)]

accuracy <- matrix(nrow = cvf, ncol = 2)

ncols <- dim(datasetEx1)[2]

for (i in 1:cvf){
  
  train.cv <- datasetEx1.norm[folds != i, ]
  test.cv <- datasetEx1.norm[folds == i, ]
  
  train_labels <- datasetEx1[folds != i, "ProLevel"]
  tst_labels <- datasetEx1[folds == i, "ProLevel"]
  
  knn.pred <- knn(train=train.cv[,-ncols], test=test.cv[,-ncols], cl=train_labels, k) 
  cfmatknn <- table(tst_labels,knn.pred)
  
  rpart.model <- rpart(ProLevel ~ ., method="class", data=train.cv)
  rpart.pred <- predict(rpart.model, test.cv, type = "class")
  cfmatrpart <- table(tst_labels,rpart.pred)
  
  accuracy[i, ] <- c( sum(diag(cfmatknn))/sum(cfmatknn),
                      sum(diag(cfmatrpart))/sum(cfmatrpart)) 
}

accuracy

apply(accuracy,2,mean)
apply(accuracy,2,sd)

# Existe diferença significativa no desempenho dos dois melhores modelos  obtidos  anteriormente?

shapiro.test(accuracy[,1]-accuracy[,2]) # p-value =  0.02661, existe normalidade

t.test<-t.test(accuracy[,1], accuracy[,2], paired = TRUE)

p_value <- t.test$p.value # p-value < alpha

# como p-value < alpha, rejeita-se a hipótese nula (H0) e conclui-se que existe diferença significativa no desempenho dos dois melhores modelos obtidos anteriormente
# como a média da acurácia do modelo de KNN é superior à média da acurácia do modelo de árvore de decisão, conclui-se que o modelo de KNN é o melhor modelo

# Comparação dos modelos
# Accuracy; Sensitivity; Specificity e F1. 
parse_results <- function(m.conf) {
  accuracy <- 100 * round((m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf), 4)
  recall = m.conf[1, 1] / (m.conf[1, 1] + m.conf[1, 2])
  precision = m.conf[1, 1] / (m.conf[1, 1] + m.conf[2, 1])
  f1 = (2 * precision * recall) / (precision + recall)
  specifity <- m.conf[2,2] / sum(m.conf[2,])

  
  message("accuracy: ", accuracy, "%")
  message("Recall: ", recall)
  message("precision: ", precision)
  message("F1: ", f1)
  message("specifity: ", specifity)
  
  my_list <-
    list(
      "F1" = f1,
      "precision" = precision,
      "recall" = recall,
      "accuracy" = accuracy,
      "specifity" = specifity

    )
  return(my_list)
}


################################################ 2 ################################################

# capacidade preditiva relativamente ao atributo “WinterTrainingCamp”

# garantir a reprodutibilidade dos resultados
set.seed(123)

# dataset exercício 2
datasetEx2 <- ciclismo_encoded
datasetEx2.norm <- as.data.frame(lapply(datasetEx2, normalise))

colnames(datasetEx2.norm) <- c( 'GroupA', 'GroupB', 'GroupC', 'GroupD', 'GroupE', 'Cobblestones', 'Hill',
                                'Mountain','NoneBackground','Sprinter','TimeTrial','Africa','Asia','Australia',
                                'Europe','NorthAmerica','SouthAmerica','altitude_results','vo2_results', 'hr_results', 'age', 'gender','ProLevel',
                                'WinterTrainingCamp')

sample <- sample(c(TRUE, FALSE), nrow(datasetEx2.norm), replace=TRUE, prob=c(0.7,0.3))
datasetEx2.train <- datasetEx2.norm[sample,]
datasetEx2.test <- datasetEx2.norm[!sample,]

# Árvore de Decisão  - com todos os atributos

tree.model = rpart(WinterTrainingCamp ~ . , data = datasetEx2.train, method = "class")

rpart.plot(
  tree.model,
  digits = 3,
  fallen.leaves = TRUE,
  type = 3
)

rpart.plot(tree.model, digits=3, cex=0.85, main="Árvore de Decisão")


tree.pred = predict(tree.model, datasetEx2.test, type = 'class')

#confusionMatrix
m.conf <- table(datasetEx2.test$WinterTrainingCamp, tree.pred)
parse_results(m.conf)

# Rede Neuronal

#-------------------------------
# 1 internal node
numnodes <- 1

#-------------------------------
# 2 internal nodes
# numnodes <- 2

#-------------------------------
# 3 internal nodes
# numnodes <- 3

#-------------------------------
# 2 internal levels: 3,2 nodes
# numnodes <- c(3, 2)


nn.model <-
  neuralnet(
    WinterTrainingCamp ~ gender + vo2_results + hr_results + age + altitude_results + ProLevel + GroupA + GroupB + GroupC + GroupD + GroupE + Cobblestones + Hill + Mountain + NoneBackground + Sprinter + TimeTrial + Africa + Asia + Australia + Europe + NorthAmerica + SouthAmerica,
    data = datasetEx2.train,
    hidden = numnodes,
    stepmax=1e7
  )
plot(nn.model)

nn.pred <- predict(nn.model, datasetEx2.test)
nn.pred <- ifelse(nn.pred > 0.5, "1", "0")

m.conf <- table(datasetEx2.test$WinterTrainingCamp, nn.pred)
parse_results(m.conf)

# k-fold cross validation

cvf <- 10
folds <- sample(1:cvf, nrow(datasetEx2), replace = TRUE)

#Fold size
table(folds)

accuracy <- matrix(nrow = cvf, ncol = 2)

ncols <- dim(datasetEx2)[2]

for (i in 1:cvf){
  
  train.cv <- datasetEx2.norm[folds != i, ]
  test.cv <- datasetEx2.norm[folds == i, ]
  
  train_labels <- datasetEx2[folds != i, "WinterTrainingCamp"]
  tst_labels <- datasetEx2[folds == i, "WinterTrainingCamp"]
  
  # Rede Neuronal
  nn.model <-
    neuralnet(
      WinterTrainingCamp ~ gender + vo2_results + hr_results + age + altitude_results + ProLevel + GroupA + GroupB + GroupC + GroupD + GroupE + Cobblestones + Hill + Mountain + NoneBackground + Sprinter + TimeTrial + Africa + Asia + Australia + Europe + NorthAmerica + SouthAmerica,
      data = train.cv,
      hidden = numnodes,
      stepmax=1e7
    )
  
  # Fazer as previsões através do modelo de rede neuronal
  predictions_nn <- predict(nn.model,test.cv)
  predictions_nn <- ifelse(predictions_nn > 0.5, "1", "0")

  # Matriz de confusão
  cfmatknn <- table(tst_labels,predictions_nn)
  

  # Árvore de Decisão
  rpart.model <- rpart(WinterTrainingCamp ~ ., method="class", data=train.cv)
  rpart.pred <- predict(rpart.model, test.cv, type = "class")
  cfmatrpart <- table(tst_labels,rpart.pred)
  
  accuracy[i, ] <- c( sum(diag(cfmatknn))/sum(cfmatknn),
                      sum(diag(cfmatrpart))/sum(cfmatrpart)) 
}

accuracy

apply(accuracy,2,mean)
apply(accuracy,2,sd)

# Existe diferença significativa no desempenho dos dois melhores modelos  obtidos  anteriormente?


shapiro.test(accuracy[,1]-accuracy[,2]) # p-value =  0.1861, existe normalidade

t.test<-t.test(accuracy[,1], accuracy[,2], paired = TRUE)

p_value <- t.test$p.value # p-value = 0.01169446 < 0.05 , rejeita-se a hipótese nula

# como p-value < alpha, rejeita-se a hipótese nula (H0) e conclui-se que existe diferença significativa no desempenho dos dois melhores modelos obtidos anteriormente


# Comparação dos modelos

################################################ 3 ################################################

# capacidade preditiva relativamente ao atributo “Gender”

# garantir a reprodutibilidade dos resultados
set.seed(123)

# dataset exercício 3
datasetEx3 <- ciclismo_encoded
colnames(datasetEx3) <- c( 'TeamgroupA', 'TeamgroupB', 'TeamgroupC', 'TeamgroupD', 'TeamgroupE', 'BackgroundCobblestones', 'BackgroundHill',
                           'BackgroundMountain','BackgroundNone','BackgroundSprinter','BackgroundTimeTrial','ContinentAfrica','ContinentAsia','ContinentAustralia',
                           'ContinentEurope','ContinentNorthAmerica','ContinentSouthAmerica','altitude_results','vo2_results', 'hr_results', 'age', 'gender','ProLevel',
                           'WinterTrainingCamp')

datasetEx3.norm <- as.data.frame(lapply(datasetEx3, normalise))

sample <- sample(c(TRUE, FALSE), nrow(datasetEx3.norm), replace=TRUE, prob=c(0.7,0.3))
datasetEx3.norm.train <- datasetEx3.norm[sample,]
datasetEx3.norm.test <- datasetEx3.norm[!sample,]

train_labels <- datasetEx3.norm[sample, "gender"]
test_labels <- datasetEx3.norm[!sample, "gender"]

## Rede neuronal


#-------------------------------
# 1 internal node
numnodes <- 1

#-------------------------------
# 2 internal nodes
#numnodes <- 2

#-------------------------------
# 3 internal nodes
#numnodes <- 3

#-------------------------------
# 2 internal levels: 3,2 nodes
#numnodes <- c(3, 2)


nn_formula <- as.formula("gender ~ TeamgroupA + TeamgroupB + TeamgroupC + TeamgroupD + TeamgroupE + BackgroundCobblestones + BackgroundHill + BackgroundMountain + 
      BackgroundNone + BackgroundSprinter + BackgroundTimeTrial +
      ContinentAfrica + ContinentAsia + ContinentAustralia + ContinentEurope + ContinentNorthAmerica + ContinentSouthAmerica + altitude_results + vo2_results +
      hr_results + age + ProLevel + WinterTrainingCamp")

nn.model <-
  neuralnet(
    nn_formula,
    data = datasetEx3.norm.train,
    hidden = numnodes,
    stepmax=1e7
  )
plot(nn.model)

nn.pred <- predict(nn.model, datasetEx3.norm.test)
nn.pred <- ifelse(nn.pred > 0.5, "1", "0")

accuracy_nn <- sum(nn.pred == datasetEx3.norm.test$gender) / length(datasetEx3.norm.test$gender) * 100   # 1 node: 87.80 | 2 nodes: 87.80 | 3 nodes: 85.76 | 2 internal levels: 3,2: 84.07

m.conf <- table(datasetEx3.norm.test$gender, nn.pred)
parse_results(m.conf)


# K-vizinhos-mais-próximos

# garantir a reprodutibilidade dos resultados
set.seed(123)

# verificação da normalização
summary(datasetEx3.norm)

k <- c()
accuracy <- c()
for (i in seq(1, 50, 2)){
  
  knn.pred <- knn(train = datasetEx3.norm.train,
                  test = datasetEx3.norm.test,
                  cl = train_labels, k=i) 
  
  cfmatrix <- table(test_labels, knn.pred)
  
  accuracy <- c(accuracy, sum(diag(cfmatrix))/sum(cfmatrix))
  
  k <- c(k,i)
}

resNeigh <- data.frame(k, accuracy)
resNeigh[resNeigh$accuracy == max(resNeigh$accuracy), ]
plot(resNeigh$k, resNeigh$accuracy)

k[which.max(accuracy)]
plot(
  resNeigh$k,
  resNeigh$accuracy,
  xlab="K",
  ylab="Accuracy",
  main="K-vizinhos-mais-próximos: Gender",
  col = ifelse(
    resNeigh$accuracy == max(resNeigh$accuracy),
    'orangered1',
    'steelblue4'
  )
)

# modelo knn

model_knn <- knn(train = datasetEx3.norm.train,
                 test = datasetEx3.norm.test,
                 cl = train_labels,
                 k = 13
) 
# Confusion Matrix
m.conf <- table(datasetEx3.norm.test$gender, model_knn)
parse_results(m.conf)

### Usar o método k-fold cross validation para obter a média e o desvio padrão da taxa de acerto da previsão 
### do atributo “Gender” com os dois melhores modelos obtidos na alínea anterior.

# garantir a reprodutibilidade dos resultados
set.seed(123)

cvf <- 10
folds <- sample(1:cvf, nrow(datasetEx3), replace = TRUE)

#Fold size
table(folds)

accuracy <- matrix(nrow = cvf, ncol = 2)

ncols <- dim(datasetEx3)[2]

# usar o k melhor obtido anteriormente.
k=13

# números de nós na rede neuronal
numnodes <- 1

for (i in 1:cvf){
  
  train.cv <- datasetEx3.norm[folds != i, ]
  test.cv <- datasetEx3.norm[folds == i, ]
  
  train_labels <- datasetEx3[folds != i, "gender"]
  tst_labels <- datasetEx3[folds == i, "gender"]
  
  knn.pred <- knn(train=train.cv[,-ncols], test=test.cv[,-ncols], cl=train_labels, k) 
  cfmatknn <- table(tst_labels,knn.pred)
  
  nn.model <-
    neuralnet(
      nn_formula,
      data = train.cv,
      hidden = numnodes,
      stepmax=1e7
    )
  
  dataset_without_gender <- subset(test.cv, select = -c(gender) )
  nn.pred <- neuralnet::compute(nn.model, dataset_without_gender)
  results <- data.frame(actual = test.cv$gender, prediction = nn.pred$net.result)
  roundedresults<-sapply(results,round,digits=0)
  roundedresultsdf=data.frame(roundedresults)
  cfmatnn <- table(roundedresultsdf$actual,roundedresultsdf$prediction)
  
  accuracy[i, ] <- c( sum(diag(cfmatknn))/sum(cfmatknn),
                      sum(diag(cfmatnn))/sum(cfmatnn)) 
}

colnames(accuracy) <- c('KNN', 'Neural Network')
accuracy
# média da taxa de acerto
apply(accuracy,2,mean)
# desvio padrão da taxa de acerto
apply(accuracy,2,sd)


# Verificar se existe diferença significativa no desempenho dos dois melhores modelos obtidos anteriormente 
# (nível de significância de 5%)

# H0: Não existe diferença significative no desempenho dos dois modelos
# H1: Existe diferença significativa no desempenho dos dois modelos

# Existe diferença significativa no desempenho dos dois melhores modelos  obtidos  anteriormente?

shapiro.test(accuracy[,1]-accuracy[,2]) # p value > alfa, logo existe normalidade

t.test<-t.test(accuracy[,1], accuracy[,2], paired = TRUE)

p_value <- t.test$p.value # p-value < alpha

alpha <- 0.05
# como p-value < alfa=0.05, rejeita-se H0, logo existe diferença significativa no desempenho dos dois modelos

# Comparação dos modelos

