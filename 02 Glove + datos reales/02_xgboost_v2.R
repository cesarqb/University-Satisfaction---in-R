#===============================================================================
rm(list=ls())
gc()
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01/')
lista_datos <- readRDS('lista_subir_v_final_20170802.rds')

library(MLmetrics)
library(Matrix)
datos_modeliza <- lista_datos$datos_modeliza
cv_folds_ensable <- lista_datos$cv_folds_ensable
cv_folds <- lista_datos$cv_folds
id <- lista_datos$id
target <- lista_datos$target
num_simula <- 5

nombres_col <- c('xgb_2_1', 'xgb_2_2', 'xgb_2_3', 'xgb_2_4')

rm(lista_datos)
gc()

datos <- Matrix(as.matrix(datos_modeliza[,!names(datos_modeliza)%in%c(id,target)]),
                sparse = T)
id_modelo <- datos_modeliza[,id]
target_modelo <- as.numeric(datos_modeliza[,target])-1
#==================================================================
library(xgboost)
# Generando los datos
dtrain <- xgb.DMatrix(data = datos[!is.na(datos_modeliza[,target]),],
                      label = target_modelo[!is.na(datos_modeliza[,target])],
                      missing = NA)

dtest <- xgb.DMatrix(data = datos[is.na(datos_modeliza[,target]),],
                     missing = NA)

gc()

#================================================================================
# Funcion para calcular los valores optimos
# library(rBayesianOptimization)
# 
# xgb_cv_bayes <- function(max_depth, eta, gamma, subsample, colsample_bytree) {
#   param <- list(objective = "multi:softprob",
#                 eval_metric = 'mlogloss',
#                 "num_class" = 4,
#                 booster = "gbtree",
#                 max_depth = max_depth,
#                 eta = eta,
#                 gamma = gamma,
#                 subsample = subsample,
#                 colsample_bytree = colsample_bytree)
#   cv <- xgb.cv(params = param,
#                data = dtrain, nround = 10000,
#                folds = cv_folds, prediction = TRUE, showsd = TRUE,
#                early_stopping_rounds = 200,
#                # maximize = TRUE,
#                verbose = TRUE,
#                print_every_n = 10)
#   
#   errores <- cv$evaluation_log$test_mlogloss_mean+cv$evaluation_log$test_mlogloss_std
#   
#   # minimizar el log_loss (-1 para maximizar)
#   list(Score = (-1)*min(errores), Pred = cv$pred, model = cv)
# }
# 
# opt_result <- BayesianOptimization(xgb_cv_bayes,
#                                    bounds = list(max_depth = c(1L, 10L),
#                                                  eta = c(0.01, 0.05),
#                                                  gamma = c(0.0, 0.5),
#                                                  subsample = c(0.4, 1),
#                                                  colsample_bytree = c(0.4, 1)),
#                                    init_points = 10, n_iter = 30,
#                                    acq = "ucb", kappa = 2.576,
#                                    eps = 0, verbose = TRUE)

#================================================================================

# parametros optimos
max_depth <- 5
eta <- 0.01
gamma <- 0.1
subsample <- 0.5
colsample_bytree <- 0.5

param_opt <- list(objective = "multi:softprob",
                  eval_metric = 'mlogloss',
                  "num_class" = 4,
                  booster = "gbtree",
                  max_depth = max_depth,
                  eta = eta,
                  gamma = gamma,
                  subsample = subsample,
                  colsample_bytree = colsample_bytree)

# cv <- xgb.cv(params = param_opt,
#              data = dtrain, nround = 10000,
#              folds = cv_folds, prediction = TRUE, showsd = TRUE,
#              early_stopping_rounds = 200,
#              # maximize = TRUE,
#              verbose = TRUE,
#              print_every_n = 10)
# best_iter <- which.min(cv$evaluation_log$test_mlogloss_mean+
#                         cv$evaluation_log$test_mlogloss_std)

nround <- 2563

#================================================================================
# Generando los modelos
# Fase 1:
datos_cv <- datos[!is.na(datos_modeliza[,target]),]
target_cv <- target_modelo[!is.na(datos_modeliza[,target])]
id_modelo_cv <- id_modelo[!is.na(datos_modeliza[,target])]

predict_fase_1 <- list()
for (i in 1:length(cv_folds_ensable)){
  # Dividiendo los datos de la muestra CV
  dtrain_cv <- xgb.DMatrix(data = datos_cv[-cv_folds_ensable[[i]],],
                           label = target_cv[-cv_folds_ensable[[i]]],
                           missing = NA)
  dval_cv <- xgb.DMatrix(data = datos_cv[cv_folds_ensable[[i]],],
                         label = target_cv[cv_folds_ensable[[i]]],
                         missing = NA)
  id_val_cv <- id_modelo_cv[cv_folds_ensable[[i]]]
  
  # Generando los modelos para cada muestra CV
  xgb_tree_cv <- lapply(seq(1:num_simula),function(x){
    set.seed(10*x)
    model <- xgb.train(params = param_opt, data = dtrain_cv,
                       nround = nround)
    print(model)
    return(model)
  })
  
  # Prediciendo cada muestra CV
  prob_tree_cv <- lapply(xgb_tree_cv,function(x){
    pred <- t(matrix(predict(x, dval_cv),
                     nrow = 4,
                     ncol = getinfo(dval_cv,
                                    name = 'nrow')))
    print('aaa')
    return(pred)
  })
  prob_tree_cv <- Reduce('+',prob_tree_cv)
  prob_tree_cv <- prob_tree_cv/num_simula
  prob_tree_cv <- data.frame(prob_tree_cv)
  prob_tree_cv <- data.frame(id_val_cv,
                             prob_tree_cv,
                             target_cv[cv_folds_ensable[[i]]])
  names(prob_tree_cv) <- c(id,nombres_col,target)
  # Guardando
  predict_fase_1[[i]] <- prob_tree_cv
  print(MultiLogLoss(prob_tree_cv[,2:5], prob_tree_cv[,target]))
  print(i)
}

predict_fase_1 <- Reduce(rbind,predict_fase_1)

# library(ModelMetrics)
# mlogLoss(target_cv, predicted = as.matrix(predict_fase_1[,!names(predict_fase_1)%in%c(id,target)]))


#================================================================================
# Generando los modelos
# Fase 2:

xgb_tree <- lapply(seq(1:num_simula),function(x){
  set.seed(10*x)
  model <- xgb.train(params = param_opt, data = dtrain,
                     nround = nround)
  print(model)
  return(model)
})

# xgb.save(xgb_tree[[1]],'xgb_1_20170727')
# xgb.save(xgb_tree[[2]],'xgb_2_20170727')
# xgb.save(xgb_tree[[3]],'xgb_3_20170727')
# xgb.save(xgb_tree[[4]],'xgb_4_20170727')
# xgb.save(xgb_tree[[5]],'xgb_5_20170727')

importancia1 <- xgb.importance(model=xgb_tree[[1]],
                               feature_names = names(datos_modeliza)[!names(datos_modeliza)%in%c(id,target)])
View(importancia1)

# Fase 2
prob_tree <- lapply(xgb_tree,function(x){
  pred <- t(matrix(predict(x, dtest),nrow=4,ncol=getinfo(dtest,
                                                         name = 'nrow')))
  print('aaa')
  return(pred)
})

prob_tree <- Reduce('+',prob_tree)
prob_tree <- prob_tree/num_simula
prob_tree <- data.frame(prob_tree)
names(prob_tree) <- nombres_col

data_subir <- data.frame(datos_modeliza[is.na(datos_modeliza$NPS),
                                        id],
                         prob_tree)
names(data_subir) <- c(id,nombres_col)


#======================================================================
# Guardando resultados
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01/01_primera_fase')
write.csv(predict_fase_1,'xgb_2_fase_1_20170802.csv',row.names = F)

setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01/02_segunda_fase')
write.csv(data_subir,'xgb_2_fase_2_20170802.csv',row.names = F)

# library(ModelMetrics)
# mlogLoss(target_cv, predicted = as.matrix(predict_fase_1[,!names(predict_fase_1)%in%c(id,target)]))

library(MLmetrics)
MultiLogLoss(predict_fase_1[,2:5], predict_fase_1$NPS)

