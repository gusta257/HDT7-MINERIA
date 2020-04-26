setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Hoja-Trabajo-7/HDT7-MINERIA")
library(e1071)
library(caret)

porcentaje<-0.7
set.seed(123)
datos <- read.csv("train.csv", stringsAsFactors = FALSE)

# MAQUINAS VECTORIALES DE SOPORTE