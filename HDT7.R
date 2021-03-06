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







#------------------------------------------------------------------------------------------------
#  INICIA SVM
# ----------------------------------------------------------------------------------------------

setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Hoja-Trabajo-7/HDT7-MINERIA")

porcentaje<-0.7
set.seed(123)
datos <- read.csv("train.csv", stringsAsFactors = FALSE)

# MAQUINAS VECTORIALES DE SOPORTE

trainImportantes <- datos[c("MSSubClass","LotFrontage","LotArea","OverallCond","YearBuilt","YearRemodAdd","X2ndFlrSF","FullBath","TotRmsAbvGrd","GarageCars","SalePrice")]
trainImportantes[is.na(trainImportantes)]<-0

km<-kmeans(trainImportantes,3)
trainImportantes$grupo<-km$cluster

g1<- trainImportantes[trainImportantes$grupo==1,]
g2<- trainImportantes[trainImportantes$grupo==2,]
g3<- trainImportantes[trainImportantes$grupo==3,]
#trainImportantes$grupo <- mapvalues(trainImportantes$grupo, c(1,2,3), c("Intermedio","Barato","Caro"))

corte <- sample(nrow(trainImportantes),nrow(trainImportantes)*porcentaje)
train<-trainImportantes[corte,]
test<-trainImportantes[-corte,]

# ==== MODELO LINEAR ======================================================
modelo_10 = svm(grupo~., data=trainImportantes, cost=2^5, gamma=2^1, kernel="linear")
modelo_11 = svm(grupo~., data=trainImportantes, cost=100, gamma=1,kernel="linear")
modelo_12 = svm(grupo~., data=trainImportantes, cost=10, gamma=2, kernel="linear")
modelo_13 = svm(grupo~., data=trainImportantes, cost=100, gamma=10,kernel="linear")

prediccion_10<-predict(modelo_10,newdata=test[,1:11])
prediccion_11<-predict(modelo_11,newdata=test[,1:11])
prediccion_12<-predict(modelo_12,newdata=test[,1:11])
prediccion_13<-predict(modelo_10,newdata=test[,1:11])

str(as.factor(test$grupo))
str(as.factor(testCompleto$predRF))

testCompleto<-test
testCompleto$pred_10<-ceiling(prediccion_10)
testCompleto$pred_11<-ceiling(prediccion_11)
testCompleto$pred_12<-ceiling(prediccion_12)
testCompleto$pred_13<-ceiling(prediccion_13)

confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_10))
confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_11))
confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_12))
confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_13))

#======================================================================================

#====== MODELO RADIAL =================================================================
modelo_20 = svm(grupo~., data=trainImportantes, cost=2^5, gamma=2^1, kernel="radial")
modelo_21 = svm(grupo~., data=trainImportantes, cost=100, gamma=1,kernel="radial")
modelo_22 = svm(grupo~., data=trainImportantes, cost=10, gamma=1, kernel="radial")
modelo_23 = svm(grupo~., data=trainImportantes, cost=100, gamma=10,kernel="radial")

prediccion_20<-predict(modelo_20,newdata=test[,1:11])
prediccion_21<-predict(modelo_21,newdata=test[,1:11])
prediccion_22<-predict(modelo_22,newdata=test[,1:11])
prediccion_23<-predict(modelo_20,newdata=test[,1:11])

str(as.factor(test$grupo))
str(as.factor(testCompleto$predRF))

testCompleto<-test
testCompleto$pred_20<-ceiling(prediccion_20)
testCompleto$pred_21<-ceiling(prediccion_21)
testCompleto$pred_22<-ceiling(prediccion_22)
testCompleto$pred_23<-ceiling(prediccion_23)

confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_20))
confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_21))
confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_22))
confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_23))

#===== MODELO POLINOMIAL ============================================================
modelo_30 = svm(grupo~., data=trainImportantes, cost=10, gamma=2, kernel="polynomial", degree = 4)
modelo_31 = svm(grupo~., data=trainImportantes, cost=10, gamma=2,kernel="polynomial", degree=3)
modelo_32 = svm(grupo~., data=trainImportantes, cost=10, gamma=2, kernel="polynomial", degree=6)
modelo_33 = svm(grupo~., data=trainImportantes, cost=10, gamma=2,kernel="polynomial", degree=5)

prediccion_30<-predict(modelo_30,newdata=test[,1:11])
prediccion_31<-predict(modelo_31,newdata=test[,1:11])
prediccion_32<-predict(modelo_32,newdata=test[,1:11])
prediccion_33<-predict(modelo_33,newdata=test[,1:11])

str(as.factor(test$grupo))
str(as.factor(testCompleto$predRF))

testCompleto<-test
testCompleto$pred_30<-ceiling(prediccion_30)
testCompleto$pred_31<-ceiling(prediccion_31)
testCompleto$pred_32<-ceiling(prediccion_32)
testCompleto$pred_33<-ceiling(prediccion_33)

confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_30))
confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_31))
confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_32))
confusionMatrix(as.factor(test$grupo),as.factor(testCompleto$pred_33))











