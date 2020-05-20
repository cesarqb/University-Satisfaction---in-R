#==================================================================
# Lectura de datos
rm(list = ls())
gc()
dev.off()
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01')
lista_datos <- readRDS('lista_subir_final.rds')

datos_modeliza <- lista_datos$datos_modeliza
cv_folds_ensable <- lista_datos$cv_folds_ensable
cv_folds <- lista_datos$cv_folds
id <- lista_datos$id
target <- lista_datos$target

num_simula <- 5
nombres_col <- c('svm_1_1', 'svm_2_1', 'svm_3_1', 'svm_4_1')

rm(lista_datos)

library(Matrix)
library(SparseM)
library(LiblineaR)

id_modelo <- datos_modeliza[,id]
target_modelo <- as.factor(datos_modeliza[,target])
datos <- as.matrix.csr(as.matrix(datos_modeliza[,!names(datos_modeliza)%in%c(id,target)]))


datos_train <- datos[!is.na(target_modelo),]
target_train <- target_modelo[!is.na(target_modelo)]

#=============================================================================================
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores = 4)

model_list_svm_lineal <- function(costo){
  models_predict <- foreach(i = 1:length(cv_folds), .verbose = T,
                            .export = c('cv_folds','datos_train',
                                        'target_train')) %dopar%{
    folds_cv <- cv_folds[[i]]
    eps <- 0.01
    library(LiblineaR)
    set.seed(12345)
    model_svm <- LiblineaR(
      data = datos_train[-folds_cv,],
      target = target_train[-folds_cv],
      type = 0,
      epsilon = eps,
      verbose = T,
      cost = costo
    )
    predict_svm <- predict(model_svm,
                           datos_train[folds_cv,],
                           proba = T)$probabilities
    predict_svm <- predict_svm[,c('1','2','3','4')]
    library(MLmetrics)
    mlogloss_cv <- MultiLogLoss(predict_svm,
                                target_train[folds_cv])
    return(list(pred_cv = predict_svm, score = mlogloss_cv))
  }
  mlogloss <- lapply(models_predict,function(x)return(x$score))
  mlogloss <- Reduce('+',mlogloss)/length(cv_folds)
  pred_models <- lapply(models_predict,function(x)return(x$pred_cv))
  pred_models <- Reduce(rbind,pred_models)
  list(Score = (-1)*mlogloss, Pred = pred_models)
}

aaa <- model_list_svm_lineal(3)
aaa$Score

library(rBayesianOptimization)
opt_result_svm <- BayesianOptimization(model_list_svm_lineal,
                                       bounds = list(costo = c(1e-4,10)),
                                       init_points = 10, n_iter = 200,
                                       acq = "ucb", kappa = 2.576,
                                       eps = 0, verbose = TRUE)

costo_opt <- 9.2856
eps <- 0.01
#==================================================================
# Generando modelos
# Fase 1:
datos_cv <- datos[!is.na(datos_modeliza[,target]),]
target_cv <- target_modelo[!is.na(datos_modeliza[,target])]
id_modelo_cv <- id_modelo[!is.na(datos_modeliza[,target])]

predict_fase_1 <- foreach(i = 1:length(cv_folds_ensable),
                          .verbose = T)%dopar%{
                            library(LiblineaR)
                            # Genero modelo
                            set.seed(12345)
                            x_cv <- datos_cv[-cv_folds_ensable[[i]],]
                            y_cv <- target_cv[-cv_folds_ensable[[i]]]
                            x_val <- datos_cv[cv_folds_ensable[[i]],]
                            y_val <- target_cv[cv_folds_ensable[[i]]]
                            model_cv <- LiblineaR(
                              data = x_cv,
                              target = y_cv,
                              type = 0,
                              epsilon = eps,
                              verbose = T,
                              cost = costo_opt)
                            # predigo
                            prob_svm_cv <- predict(model_cv,
                                                   x_val,
                                                   proba = T)$probabilities
                            prob_svm_cv <- prob_svm_cv[,c('1','2','3','4')]
                            prob_svm_cv <- data.frame(prob_svm_cv)
                            id_val_cv <- id_modelo_cv[cv_folds_ensable[[i]]]
                            prob_svm_cv <- data.frame(id_val_cv,
                                                      prob_svm_cv,
                                                      target_cv[cv_folds_ensable[[i]]])
                            names(prob_svm_cv) <- c(id,nombres_col,target)
                            library(MLmetrics)
                            print(MultiLogLoss(prob_svm_cv[,2:5], prob_svm_cv[,target]))
                            return(prob_svm_cv)
                          }
library(MLmetrics)
lapply(predict_fase_1,function(x){
  print(MultiLogLoss(x[,2:5], x[,target]))
})


predict_fase_1 <- Reduce(rbind,predict_fase_1)
MultiLogLoss(predict_fase_1[,2:5], predict_fase_1[,target])

#=====================================================================
# Fase 2:
set.seed(12345)
model_svm <- LiblineaR(
  data = datos[!is.na(target_modelo),],
  target = target_modelo[!is.na(target_modelo)],
  type = 0,
  epsilon = eps,
  verbose = T,
  cost = costo_opt)

predict_fase_2 <- predict(model_svm,
                          datos[is.na(target_modelo),],
                          proba = T)$probabilities
predict_fase_2 <- predict_fase_2[,c('1','2','3','4')]

data_subir <- data.frame(datos_modeliza[is.na(datos_modeliza$NPS),
                                        id],
                         predict_fase_2)
names(data_subir) <- c(id,nombres_col)

#======================================================================
# Guardando resultados
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01/01_primera_fase')
write.csv(predict_fase_1,'svm_1_fase_1_20170802.csv',row.names = F)

setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01/02_segunda_fase')
write.csv(data_subir,'svm_1_fase_2_20170802.csv',row.names = F)

