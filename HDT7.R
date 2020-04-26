library(caret)
library(dummy)
library(plyr)
library(dplyr)
library(e1071)
library(lattice)
library(rpart)
library(nnet)
library(RWeka)
library(neural)
library(neuralnet)

#Redes Neuronales 

setwd("C:/Users/Gustavo/Desktop/SEPTIMO SEMESTRE/MINERIA/HDT7/HDT7-MINERIA")
#setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Hoja-Trabajo-6/Hoja-de-trabajo-06")

porcentaje<-0.7
datos <- read.csv("train.csv", stringsAsFactors = FALSE)
set.seed(123)

trainImportantes <- datos[c("MSSubClass","LotFrontage","LotArea","OverallCond","YearBuilt","YearRemodAdd","X2ndFlrSF","FullBath","TotRmsAbvGrd","GarageCars","SalePrice")]
trainImportantes[is.na(trainImportantes)]<-0

km<-kmeans(trainImportantes,3)
trainImportantes$grupo<-km$cluster

trainImportantes$grupo <- mapvalues(trainImportantes$grupo, c(1,2,3), c("Intermedio","Barato","Caro"))

#trainImportantes<-cbind(trainImportantes,dummy(trainImportantes,verbose = T))

porcentaje<-0.7
corte <- sample(nrow(trainImportantes),nrow(trainImportantes)*porcentaje)
train<-trainImportantes[corte,]
test<-trainImportantes[-corte,]

#-------------------------------------------------
# Red Neuronal con nnet
#-------------------------------------------------
modelo.nn2 <- nnet(as.factor(grupo)~.,data=train, size=5, rang=0.0000001,decay=5e-4, maxit=500) 
prediccion2 <- as.data.frame(predict(modelo.nn2, newdata = test[,1:11]))
columnaMasAlta<-apply(prediccion2, 1, function(x) colnames(prediccion2)[which.max(x)])
test$prediccion2<-columnaMasAlta
test$prediccion2 <- as.factor(test$prediccion2)
cfm<-confusionMatrix(test$prediccion2,as.factor(test$grupo))
cfm
#-------------------------------------------------

#-------------------------------------------------
# Red Neuronal con RWeka
#-------------------------------------------------
NB <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
NB 
WOW(NB)
nnodos='4'

modelo.bp<-NB(as.factor(grupo)~., data=trainImportantes,subset = corte,
              control=Weka_control(H=nnodos, N=1000, G=TRUE), options=NULL)
test$prediccionWeka<-predict(modelo.bp, newdata = test[,1:11])
cfmWeka<-confusionMatrix(test$prediccionWeka,as.factor(test$grupo))
cfmWeka

#-------------------------------------------------
# Red Neuronal con caret
#-------------------------------------------------

modeloCaret <- train(as.factor(grupo)~.,data=train, method="nnet", trace=F)
test$prediccionCaret<-predict(modeloCaret, newdata = test[,1:11])
cfmCaret<-confusionMatrix(test$prediccionCaret,as.factor(test$grupo))
cfmCaret

















