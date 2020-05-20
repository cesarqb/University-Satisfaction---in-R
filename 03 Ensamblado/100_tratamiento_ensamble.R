rm(list=ls())
gc()
dev.off()
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01/01_primera_fase')
id <- 'COD_ENCUESTADO'
target <- 'NPS'

data_1 <- read.csv('glmnet_2_fase_1_20170802.csv')
data_2 <- read.csv('glmnet_fase_1_20170727.csv')
data_3 <- read.csv('svm_1_fase_1_20170802.csv')
data_4 <- read.csv('svm_fase_1_20170727.csv')
data_5 <- read.csv('xgb_1_fase_1_20170728.csv')
data_6 <- read.csv('xgb_2_fase_1_20170727.csv')
data_7 <- read.csv('xgb_l_fase_1_20170727.csv')

data_5$NPS <- data_5$NPS + 1
data_6$NPS <- data_6$NPS + 1
data_7$NPS <- data_7$NPS + 1

datos <- merge(data_1,data_2, by=c(id,target))
datos <- merge(datos,data_3, by=c(id,target))
datos <- merge(datos,data_4, by=c(id,target))
datos <- merge(datos,data_5, by=c(id,target))
datos <- merge(datos,data_6, by=c(id,target))
datos <- merge(datos,data_7, by=c(id,target))


setwd('C:\Users\Cesar Quezada\Dropbox\KAGGLE\01. Concurso 01/02_segunda_fase')
data_1_1 <- read.csv('glmnet_2_fase_2_20170802.csv')
data_2_1 <- read.csv('glmnet_fase_2_20170727.csv')
data_3_1 <- read.csv('svm_1_fase_2_20170802.csv')
data_4_1 <- read.csv('svm_fase_2_20170727.csv')
data_5_1 <- read.csv('xgb_1_fase_2_20170728.csv')
data_6_1 <- read.csv('xgb_2_fase_2_20170727.csv')
data_7_1 <- read.csv('xgb_l_fase_2_20170727.csv')

datos_1 <- merge(data_1_1,data_2_1, by=c(id))
datos_1 <- merge(datos_1,data_3_1, by=c(id))
datos_1 <- merge(datos_1,data_4_1, by=c(id))
datos_1 <- merge(datos_1,data_5_1, by=c(id))
datos_1 <- merge(datos_1,data_6_1, by=c(id))
datos_1 <- merge(datos_1,data_7_1, by=c(id))
datos_1[,target] <- NA

datos_modeliza_0 <- rbind(datos,datos_1)


#==================================================================
# Lectura de datos
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01/')
lista_datos <- readRDS('lista_subir_final.rds')

datos_modeliza_1 <- lista_datos$datos_modeliza

rm(lista_datos)

datos_modeliza <- merge(datos_modeliza_0,datos_modeliza_1,
                        by = c(id,target))

#===============================================================================
# Generando cluster
library(rBayesianOptimization)
cv_folds_ensable <- KFold(datos_modeliza[!is.na(!datos_modeliza[,target]),target],
                          nfolds = 5, stratified = TRUE, seed = 090909)

cv_folds <- KFold(datos_modeliza[!is.na(!datos_modeliza[,target]),target],
                  nfolds = 5, stratified = TRUE, seed = 112233)

#===============================================================================
# Guardando resultados
setwd('C:\Users\Cesar Quezada\Dropbox\KAGGLE\01. Concurso 01')
lista_subir <- list(datos_modeliza = datos_modeliza,
                    cv_folds_ensable = cv_folds_ensable,
                    cv_folds = cv_folds,
                    id = id,
                    target = target)
saveRDS(lista_subir, "lista_subir_final_ensamble.rds")
#===============================================================================
