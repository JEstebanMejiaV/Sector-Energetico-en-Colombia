library("pROC")
library(e1071)
library(C50)

Supervisado <- DatosModelamiento2[,c(4, 7, 8, 12, 13, 15:23,25:29,32,33)]


Supervisado$ColocoPrecion <- as.factor(Supervisado$ColocoPrecion)

set.seed(2)
ind <- sample(2, nrow(Supervisado), replace = TRUE, prob=c(0.7, 0.3))
trainset = Supervisado[ind == 1,]
testset = Supervisado[ind == 2,]
ind = cut(1:nrow(Supervisado), breaks=10, labels=F)

accuracies = c()
for (i in 1:10) {
  fit = svm(ColocoPrecio ~., Supervisado[ind != i,])
  predictions = predict(fit, Supervisado[ind == i, ! names(Supervisado) %in% c("ColocoPrecio")])
  correct_count = sum(predictions == Supervisado[ind == i,c("ColocoPrecio")])
  accuracies = append(correct_count / nrow(Supervisado[ind == i,]), accuracies)
}
accuracies
mean(accuracies)
for (i in 1:10) {
  fit = naiveBayes(ColocoPrecio ~., Supervisado[ind != i,])
  predictions = predict(fit, Supervisado[ind == i, ! names(Supervisado) %in% c("ColocoPrecio")])
  correct_count = sum(predictions == Supervisado[ind == i,c("ColocoPrecio")])
  accuracies = append(correct_count / nrow(Supervisado[ind == i,]), accuracies)
}


library(caret)
control = trainControl(method="repeatedcv", number=10, repeats=3)
model = train(ColocoPrecio~., data=na.omit(trainset), method="rpart", preProcess="scale", trControl=control)
model


svm.model= train(ColocoPrecio ~ .,
  data = na.omit(trainset),
  method = "svmRadial")
svm.pred = predict(svm.model, testset[,! names(testset) %in% c("ColocoPrecio")])
table(svm.pred, testset[,c("ColocoPrecio")])
confusionMatrix(svm.pred, testset[,c("ColocoPrecio")])

library(randomForest)
datos <- na.omit(Supervisado)
datos$ColocoPrecio <- as.factor(datos$ColocoPrecio)

library(dplyr)
datos=datos %>% mutate_if(is.character, as.factor)

BosquesA = randomForest(ColocoPrecio  ~ ., data = na.omit(datos[,c(2:5,7:17,19:21)]), 
  importance =T,# Evalución de la importancia de los predictores
  ntree = 500 , 
  proximity=F # Número de arboles en el bosque
)

BosquesA


plot(BosquesA) #Graficacion de Error para cada árbol 

importance(BosquesA) # Importancia de los predictores medante diferentes criterios
# Recuesde: que sucederia con la exactitud o el coeficionte de 
# GINI promedio al quirar dicho predictor

varImpPlot(BosquesA) #Graficación de los resultados anteriores