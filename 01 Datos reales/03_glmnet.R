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
nombres_col <- c('glmnet_1', 'glmnet_2', 'glmnet_3', 'glmnet_4')

rm(lista_datos)

library(Matrix)
id_modelo <- datos_modeliza[,id]
target_modelo <- as.factor(datos_modeliza[,target])
datos <- Matrix(as.matrix(datos_modeliza[,!names(datos_modeliza)%in%c(id,target)]),
                sparse = T)

#==================================================================
library(glmnet)
library(parallel)
library(doParallel)
registerDoParallel(cores = 4)

# simula_glmnet <- data.frame(alpha = rep(NA,11),
#                             lambda = rep(NA,11),
#                             deviance = rep(NA,11))
# for (i in 1:11){
#   alpha_glmnet <- (i-1)/10
#   set.seed(12345)
#   fit <- cv.glmnet(x = datos[!is.na(target_modelo),],
#                    y = target_modelo[!is.na(target_modelo)],
#                    family = 'multinomial',
#                    type.measure = 'deviance',
#                    nfolds = 5,
#                    nlambda = 100,
#                    alpha = alpha_glmnet,
#                    parallel = T,
#                    maxit = 1e3,
#                    thresh = 1e-3,
#                    type.logistic = 'modified.Newton'
#                    )
#   simula_glmnet[i,] <- c(alpha_glmnet,
#                          fit$lambda.1se,
#                          fit$cvm[fit$lambda==fit$lambda.1se])
#   plot(fit)
#   print(i)
#   print(alpha_glmnet)
#   print(simula_glmnet[i,])
# }

#==================================================================

# alpha      lambda deviance
# 1    0.0 0.336784210 1.630251
# 2    0.1 0.037831780 1.592493
# 3    0.2 0.020760166 1.589941
# 4    0.3 0.015189504 1.592790
# 5    0.4 0.011392128 1.591356
# 6    0.5 0.009113703 1.590572
# 7    0.6 0.007594752 1.590123
# 8    0.7 0.007144484 1.596339
# 9    0.8 0.006251423 1.596071
# 10   0.9 0.005556821 1.595862
# 11   1.0 0.005001139 1.595713

alpha_opt <- 1
lambda_opt <- 0.005001139

#==================================================================
# Generando modelos
# Fase 1:
datos_cv <- datos[!is.na(datos_modeliza[,target]),]
target_cv <- target_modelo[!is.na(datos_modeliza[,target])]
id_modelo_cv <- id_modelo[!is.na(datos_modeliza[,target])]

predict_fase_1 <- foreach(i = 1:length(cv_folds_ensable),
                          .verbose = T)%dopar%{
  library(glmnet)
  # Genero modelo
  set.seed(12345)
  x_cv <- datos_cv[-cv_folds_ensable[[i]],]
  y_cv <- target_cv[-cv_folds_ensable[[i]]]
  x_val <- datos_cv[cv_folds_ensable[[i]],]
  y_val <- target_cv[cv_folds_ensable[[i]]]
  model_cv <- glmnet(x = x_cv,
                     y = y_cv,
                     family = 'multinomial',
                     alpha = alpha_opt,
                     lambda = lambda_opt,
                     maxit = 1e3,
                     thresh = 1e-3,
                     type.logistic = 'modified.Newton')
  # predigo
  prob_glmnet_cv <- predict(model_cv,
                            x_val,
                            type = 'response',
                            exact = T)[,,1]
  prob_glmnet_cv <- data.frame(prob_glmnet_cv)
  id_val_cv <- id_modelo_cv[cv_folds_ensable[[i]]]
  prob_glmnet_cv <- data.frame(id_val_cv,
                               prob_glmnet_cv,
                             target_cv[cv_folds_ensable[[i]]])
  names(prob_glmnet_cv) <- c(id,nombres_col,target)
  library(MLmetrics)
  print(MultiLogLoss(prob_glmnet_cv[,2:5], prob_glmnet_cv[,target]))
  return(prob_glmnet_cv)
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
model_glmnet <- glmnet(x = datos[!is.na(target_modelo),],
                       y = target_modelo[!is.na(target_modelo)],
                       family = 'multinomial',
                       alpha = alpha_opt,
                       lambda = lambda_opt,
                       maxit = 1e3,
                       thresh = 1e-3,
                       type.logistic = 'modified.Newton')
predict_fase_2 <- predict(model_glmnet,
                          datos[is.na(target_modelo),],
                          type = 'response')[,,1]

data_subir <- data.frame(datos_modeliza[is.na(datos_modeliza$NPS),
                                        id],
                         predict_fase_2)
names(data_subir) <- c(id,nombres_col)

#======================================================================
# Guardando resultados
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01/01_primera_fase')
write.csv(predict_fase_1,'glmnet_fase_1_20170727.csv',row.names = F)

setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01/02_segunda_fase')
write.csv(data_subir,'glmnet_fase_2_20170727.csv',row.names = F)
