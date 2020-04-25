library(caret)
library(dummies)
library(plyr)
library(dplyr)
library(e1071)
library(lattice)
library(rpart)
library(nnet)
library(RWeka)
library(neural)
library(neuralnet)

#Modelo de Regresi?n log?stica

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

#trainImportantes<-cbind(trainImportantes,dummy(trainImportantes$grupo,verbose = T))
#colnames(trainImportantes)[13] <- "EsBarata"
#colnames(trainImportantes)[14] <- "EsCara"
#colnames(trainImportantes)[15] <- "EsIntermedia"

porcentaje<-0.7
corte <- sample(nrow(trainImportantes),nrow(trainImportantes)*porcentaje)
train<-trainImportantes[corte,]
test<-trainImportantes[-corte,]

#-------------------------------------------------
# Red Neuronal con nnet
#-------------------------------------------------

trainImportantes$grupo <- as.factor(trainImportantes$grupo)
modelo.nn2 <- nnet(grupo~.,data = trainImportantes,subset = corte, size=2, rang=0.1,
                   decay=5e-4, maxit=200) 

prediccion2 <- as.data.frame(predict(modelo.nn2, newdata = test[,1:11]))

columnaMasAlta<-apply(prediccion2, 1, function(x) colnames(prediccion2)[which.max(x)])

test$prediccion2<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción
test$grupo <- as.factor(test$grupo)
test$prediccion2 <- as.factor(test$prediccion2)
cfm<-confusionMatrix(test$prediccion2,test$grupo)
cfm

#-------------------------------------------------


