#==================================================================
rm(list = ls())
gc()

library(ggplot2)
library(tm)
library(stringr)
library(topicmodels)
library(hunspell)

# LECTURA DE DATOS
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/00. Concurso 00 - Titanic')
train_txt <- read.csv('train_universidad.csv')
train_txt$type_data <- 'train' 
test_txt <- read.csv('test_universidad.csv')
test_txt$NPS <- NA
test_txt$type_data <- 'test'

datos_txt <- rbind(train_txt, test_txt)
rm(train_txt, test_txt)

id <- 'COD_ENCUESTADO'
target <- 'NPS'

#==================================================================
# Tratando algunos datos
# DEPORTISTA
datos_txt$ind_deportista <- ifelse(datos_txt$UOD_depostista_ind_deportista=='Deportista',1,0)
datos_txt$UOD_depostista_ind_deportista <- NULL
mosaicplot(datos_txt$ind_deportista~datos_txt[,target], col=T)
prop_table <- prop.table(table(datos_txt$ind_deportista,
                               datos_txt$type_data), 
                         margin = 2)
barplot(prop_table)
chisq.test(prop_table)

# DELEGADO
datos_txt$ind_delegado <- ifelse(datos_txt$IND_DELEGADO=='Delegado',1,0)
datos_txt$IND_DELEGADO <- NULL
mosaicplot(datos_txt$ind_delegado~datos_txt[,target], col=T)
prop_table <- prop.table(table(datos_txt$ind_delegado,
                               datos_txt$type_data), 
                         margin = 2)
barplot(prop_table)
chisq.test(prop_table)

# Nombre.Campus
datos_txt$nombre_campus <- as.factor(datos_txt$Nombre.Campus)
datos_txt$Nombre.Campus <- NULL
mosaicplot(datos_txt$nombre_campus~datos_txt[,target], col=T)
prop_table <- prop.table(table(datos_txt$nombre_campus,
                               datos_txt$type_data), 
                         margin = 2)
barplot(prop_table)
chisq.test(prop_table)

# IND_GEA
datos_txt$ind_gea <- ifelse(datos_txt$IND_GEA=='GEA',1,0)
datos_txt$IND_GEA <- NULL
mosaicplot(datos_txt$ind_gea~datos_txt[,target], col=T)
prop_table <- prop.table(table(datos_txt$ind_gea,
                               datos_txt$type_data), 
                         margin = 2)
barplot(prop_table)
chisq.test(prop_table)

# Ciclo (la dejaremos como numero)
datos_txt$ciclo <- datos_txt$Ciclo
datos_txt$Ciclo <- NULL
table(datos_txt$ciclo,datos_txt$NPS)
boxplot(datos_txt$ciclo~datos_txt$NPS)
plot(density(datos_txt$ciclo[datos_txt$type_data=='train']),
     ylim = c(0,0.6), lwd = 2)
lines(density(datos_txt$ciclo[datos_txt$type_data=='test']),col='red',
      lwd = 3)
ks.test(datos_txt$ciclo[datos_txt$type_data=='train'],
        datos_txt$ciclo[datos_txt$type_data=='test'])


# Clave.de.carrera (la dejaremos como numero)
datos_txt$clave_carrera <- datos_txt$Clave.de.carrera
datos_txt$Clave.de.carrera <- NULL
boxplot(datos_txt$clave_carrera~datos_txt$NPS)
plot(density(datos_txt$clave_carrera[datos_txt$type_data=='train']),
     ylim = c(0,0.03), lwd = 2)
lines(density(datos_txt$clave_carrera[datos_txt$type_data=='test']),col='red',
      lwd = 3)
ks.test(datos_txt$clave_carrera[datos_txt$type_data=='train'],
        datos_txt$clave_carrera[datos_txt$type_data=='test'])


# CANT_CURSOS_MATRICU_SIN_INGLES (la dejaremos como numero)
datos_txt$s_ingles <- datos_txt$CANT_CURSOS_MATRICU_SIN_INGLES
datos_txt$CANT_CURSOS_MATRICU_SIN_INGLES <- NULL
table(datos_txt$s_ingles,datos_txt$NPS)
boxplot(datos_txt$s_ingles~datos_txt$NPS)
plot(density(datos_txt$s_ingles[!is.na(datos_txt$s_ingles)&
                                  datos_txt$type_data=='train']),
     ylim = c(0,0.7), lwd = 2)
lines(density(datos_txt$s_ingles[!is.na(datos_txt$s_ingles)&
                                   datos_txt$type_data=='test']),
      ylim = c(0,0.7), lwd = 3, col='red')
summary(datos_txt$s_ingles)
ks.test(datos_txt$s_ingles[datos_txt$type_data=='train'],
        datos_txt$s_ingles[datos_txt$type_data=='test'])


datos_txt$na_s_ingles <- ifelse(is.na(datos_txt$s_ingles),1,0)
table(datos_txt$na_s_ingles,datos_txt[,target])
mosaicplot(datos_txt$na_s_ingles~datos_txt[,target], col=T)
prop_table <- prop.table(table(datos_txt$na_s_ingles,
                               datos_txt$type_data), 
                         margin = 2)
barplot(prop_table)
chisq.test(prop_table)

# NIVEL.ACTUAL
datos_txt$nivel <- datos_txt$NIVEL.ACTUAL
datos_txt$NIVEL.ACTUAL <- NULL
table(datos_txt$nivel,datos_txt[,target])
mosaicplot(datos_txt$nivel~datos_txt[,target], col=T)
prop_table <- prop.table(table(datos_txt$nivel,
                               datos_txt$type_data),
                         margin = 2)
barplot(prop_table)
chisq.test(prop_table)


names(datos_txt)

#==================================================================

# Tratamiento de texto
library(stringr)
datos_txt$COMENTARIO <- as.character(datos_txt$COMENTARIO)

# Cantidad de textos
datos_txt$longitud <- sapply(gregexpr("\\W+", datos_txt$COMENTARIO), length) + 1
plot(density(datos_txt$longitud))
boxplot(datos_txt$longitud~datos_txt$NPS)
plot(density(datos_txt$longitud[datos_txt$type_data=='train']),
     ylim = c(0,0.1), lwd = 2)
lines(density(datos_txt$longitud[datos_txt$type_data=='test']),
      ylim = c(0,0.1), lwd = 3, col='red')
ks.test(datos_txt$longitud[datos_txt$type_data=='train'],
        datos_txt$longitud[datos_txt$type_data=='test'])


# Cantidad de numeros
cant_numer <- strsplit(datos_txt$COMENTARIO, " ")
cant_numer <- lapply(cant_numer,function(x){
  xx <- as.numeric(x)
  long <- length(xx[!is.na(xx)])
  return(long)
})
cant_numer <- unlist(cant_numer)
datos_txt$ind_number_text <- ifelse(cant_numer>0,1,0)
table(datos_txt$ind_number_text,datos_txt[,target])
mosaicplot(datos_txt$ind_number_text~datos_txt[,target], col=T)
prop_table <- prop.table(table(datos_txt$ind_number_text,
                               datos_txt$type_data),
                         margin = 2)
barplot(prop_table)
chisq.test(prop_table)

# datos_txt$prop_number <- cant_numer / datos_txt$longitud
# plot(density(datos_txt$prop_number[!is.na(datos_txt$NPS)&datos_txt$NPS==1]),col=1)
# lines(density(datos_txt$prop_number[!is.na(datos_txt$NPS)&datos_txt$NPS==2]),col=2)
# lines(density(datos_txt$prop_number[!is.na(datos_txt$NPS)&datos_txt$NPS==3]),col=3)
# lines(density(datos_txt$prop_number[!is.na(datos_txt$NPS)&datos_txt$NPS==4]),col=4)
# boxplot(log(datos_txt$prop_number+1)~datos_txt$NPS)
# plot(density(datos_txt$prop_number[datos_txt$type_data=='train']),
#      ylim = c(0,250), lwd = 2)
# lines(density(datos_txt$prop_number[datos_txt$type_data=='test']),
#       col="red")
# ks.test(datos_txt$prop_number[datos_txt$type_data=='train'],
#         datos_txt$prop_number[datos_txt$type_data=='test'])

# Ind si hay mayúsculas


#=============================================================================
library(text2vec)
library(tokenizers)
library(SnowballC)

text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  #x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  #x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  #x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  # x = 
  return(x)
}


prep_fun <- text.clean
tok_fun <- tokenize_word_stems


it_train <- itoken(datos_txt$COMENTARIO,
                   preprocessor = prep_fun,
                   tokenizer = tok_fun,
                   ids = datos_txt[,id],
                   progressbar = T,
                   language = "spanish")
vocab <- create_vocabulary(it_train, ngram = c(1L, 10L))

vocab1 <- prune_vocabulary(vocab,
                           term_count_min = 10,
                           doc_proportion_min = 0.001)

vectorizer <- vocab_vectorizer(vocab1)
dtm_train <- create_dtm(it_train, vectorizer)
dim(dtm_train)

# define tfidf model
tfidf <- TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
class(dtm_train_tfidf)
dim(dtm_train_tfidf)

#==================================================================
# LSA
set.seed(12345)
lsa <- LatentSemanticAnalysis$new(n_topics = 10)
lsa_data <- lsa$fit_transform(dtm_train_tfidf)
for(i in 1:10){
  boxplot(lsa_data[,i]~datos_txt$NPS)
}

# LDA
vocab2 <- prune_vocabulary(vocab,
                           term_count_min = 10,
                           doc_proportion_max = 0.3)
vectorizer2 <- vocab_vectorizer(vocab2)
lda_model <- LatentDirichletAllocation$new(n_topics = 10, vocabulary = vocab2)

dtm <- create_dtm(it_train, vectorizer2, type = "lda_c")
doc_topic_distr <- 
  lda_model$fit_transform(dtm, n_iter = 1000, convergence_tol = 0.01, 
                          check_convergence_every_n = 10)
for(i in 1:10){
  boxplot(doc_topic_distr[,i]~datos_txt$NPS)
}
# lda_model$plot()

#==================================================================
# TSNE
coment_matrix <- as.matrix(dtm_train_tfidf)

library(Rtsne)
data_1 <- data.frame(coment_matrix,row.names = NULL)
set.seed(12345)
data_1 <- sapply(data_1,
                 function(x)return(x + runif(length(x),-1e-10,1e-10)))
set.seed(12345)
tsne <- Rtsne(data_1, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)

tsne_data <- data.frame(tsne$Y)
names(tsne_data) <- c('tsn2_1','tsn2_2')

#==================================================================
# CLUSTER

set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
wss <- sapply(1:k.max,
              function(k){kmeans(tsne_data, k, nstart = 10)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

library(flexclust)
set.seed(12345)
cluster <- kcca(tsne_data, k = 4, kccaFamily("kmeans"))
image(cluster)

cluster_class <- predict(cluster, tsne_data)

cluster_1 <- ifelse(cluster_class==1,1,0)
cluster_2 <- ifelse(cluster_class==2,1,0)
cluster_3 <- ifelse(cluster_class==3,1,0)
cluster_4 <- ifelse(cluster_class==4,1,0)

medias_cluster <- cluster@centers

dist_1 <- apply(tsne_data,
                1,
                function(x){
                  sqrt(sum((x-as.numeric(medias_cluster[1,]))^2))
                })
dist_2 <- apply(tsne_data,
                1,
                function(x){
                  sqrt(sum((x-as.numeric(medias_cluster[2,]))^2))
                })
dist_3 <- apply(tsne_data,
                1,
                function(x){
                  sqrt(sum((x-as.numeric(medias_cluster[3,]))^2))
                })
dist_4 <- apply(tsne_data,
                1,
                function(x){
                  sqrt(sum((x-as.numeric(medias_cluster[4,]))^2))
                })

distance_cluster <- dist_1
distance_cluster[cluster_class==1] <- dist_1[cluster_class==1]
distance_cluster[cluster_class==2] <- dist_1[cluster_class==2]
distance_cluster[cluster_class==3] <- dist_1[cluster_class==3]
distance_cluster[cluster_class==4] <- dist_1[cluster_class==4]

#===============================================================================
# Objetos creados
# dtm_train_tfidf
# target
# lsa_data
# doc_topic_distr
# tsne_data
# distance_cluster
# cluster_1
# cluster_2
# cluster_3
# cluster_4
# id
campus_1 <- ifelse(datos_txt$nombre_campus==1,1,0)
campus_2 <- ifelse(datos_txt$nombre_campus==2,1,0)
campus_3 <- ifelse(datos_txt$nombre_campus==3,1,0)
campus_4 <- ifelse(datos_txt$nombre_campus==4,1,0)
nivel_ac <- ifelse(datos_txt$nivel == 'AC', 1, 0)
nivel_fc <- ifelse(datos_txt$nivel == 'FC', 1, 0)
nivel_online <- ifelse(datos_txt$nivel == 'ON LINE', 1, 0)
nivel_presencial <- ifelse(datos_txt$nivel == 'PRESENCIAL', 1, 0)

datos_modeliza <- data.frame(
  COD_ENCUESTADO = datos_txt[,id],
  NPS = datos_txt[,target],
  as.matrix(dtm_train_tfidf),
  lsa_data,
  doc_topic_distr,
  tsne_data,
  cluster_1,
  cluster_2,
  cluster_3,
  cluster_4,
  distance_cluster,
  ind_deportista = datos_txt[,'ind_deportista'],
  ind_delegado = datos_txt[,'ind_delegado'],
  campus_1,
  campus_2,
  campus_3,
  campus_4,
  ind_gea = datos_txt[,'ind_gea'],
  ciclo_estudio = datos_txt[,'ciclo'],
  clave_carrera = datos_txt[,'clave_carrera'],
  s_ingles = datos_txt[,'s_ingles'],
  na_s_ingles = datos_txt[,'na_s_ingles'],
  nivel_ac,
  nivel_fc,
  nivel_online,
  nivel_presencial,
  longitud = datos_txt[,'longitud'],
  ind_number_text = datos_txt[,'ind_number_text']
  )


# Imputando s_ingles
library(ranger)
set.seed(12345)
ranger_imputa <- ranger(formula = s_ingles ~ .-COD_ENCUESTADO-NPS,
                        data = datos_modeliza[!is.na(datos_modeliza$s_ingles),],
                        # mtry = mtry_opt, 
                        num.trees = 500,
                        num.threads = 4,
                        write.forest = T,
                        importance = 'impurity')
barplot(importance(ranger_imputa))

s_ingles_imputa <- predict(ranger_imputa,datos_modeliza)$predictions
datos_modeliza$s_ingles[is.na(datos_modeliza$s_ingles)] <- 
  s_ingles_imputa[is.na(datos_modeliza$s_ingles)]

dev.off()
pdf("histogramas.pdf",width=7,height=5)
for(i in 3:3075){
  plot(density(datos_modeliza[!is.na(datos_modeliza[,target]),i]),col='blue')
  lines(density(datos_modeliza[is.na(datos_modeliza[,target]),i]),col='red')
  print(i)
}
dev.off()
#===============================================================================
# Generando cluster
library(rBayesianOptimization)
cv_folds_ensable <- KFold(datos_modeliza[!is.na(!datos_modeliza[,target]),target],
                          nfolds = 5, stratified = TRUE, seed = 12345)

cv_folds <- KFold(datos_modeliza[!is.na(!datos_modeliza[,target]),target],
                  nfolds = 5, stratified = TRUE, seed = 0)

#===============================================================================
# Guardando resultados
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01')
lista_subir <- list(datos_modeliza = datos_modeliza,
                    cv_folds_ensable = cv_folds_ensable,
                    cv_folds = cv_folds,
                    id = id,
                    target = target)
saveRDS(lista_subir, "lista_subir_final.rds")
#===============================================================================




#===============================================================================


#===============================================================================


#===============================================================================

rm(list=ls())
gc()
setwd('C:/Users/Cesar Quezada/Dropbox/KAGGLE/01. Concurso 01')
lista_datos <- readRDS('lista_subir_final.rds')

datos_modeliza <- lista_datos$datos_modeliza
cv_folds_ensable <- lista_datos$cv_folds_ensable
cv_folds <- lista_datos$cv_folds
id <- lista_datos$id
target <- lista_datos$target

rm(lista_datos)
gc()
#==================================================================
library(xgboost)
# Generando los datos
dtrain <- xgb.DMatrix(data = as.matrix(datos_modeliza[!is.na(datos_modeliza[,target]),
                                                      !names(datos_modeliza)%in%c(id,target)]),
                      label = as.numeric(datos_modeliza[!is.na(datos_modeliza$NPS),target])-1,
                      missing = NA)

dtest <- xgb.DMatrix(data = as.matrix(datos_modeliza[is.na(datos_modeliza[,target]),
                                                     !names(datos_modeliza)%in%c(id,target)]),
                     missing = NA)

gc()

#================================================================================
# Funcion para calcular los valores optimos
library(rBayesianOptimization)

xgb_cv_bayes <- function(max_depth, eta, gamma, subsample, colsample_bytree) {
  param <- list(objective = "multi:softprob",
                eval_metric = 'mlogloss',
                "num_class" = 4,
                booster = "gbtree",
                max_depth = max_depth,
                eta = eta,
                gamma = gamma,
                subsample = subsample,
                colsample_bytree = colsample_bytree)
  cv <- xgb.cv(params = param,
               data = dtrain, nround = 10000,
               folds = cv_folds, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 200,
               # maximize = TRUE,
               verbose = TRUE,
               print_every_n = 10)
  
  errores <- cv$evaluation_log$test_mlogloss_mean+cv$evaluation_log$test_mlogloss_std
  
  # minimizar el log_loss (-1 para maximizar)
  list(Score = (-1)*min(errores), Pred = cv$pred, model = cv)
}

aa <- xgb_cv_bayes(5,0.01,0.1,0.5,0.5)

#================================================================================
# Generando los modelos

# parametros optimos
max_depth <- 5
eta <- 0.01
gamma <- 0.1
subsample <- 0.5
colsample_bytree <- 0.5
nround <- 3706
param_opt <- list(objective = "multi:softprob",
                  eval_metric = 'mlogloss',
                  "num_class" = 4,
                  booster = "gbtree",
                  max_depth = max_depth,
                  eta = eta,
                  gamma = gamma,
                  subsample = subsample,
                  colsample_bytree = colsample_bytree)

# Fase 1:
xgb_tree <- lapply(seq(1:5),function(x){
  set.seed(10*x)
  model <- xgb.train(params = param_opt, data = dtrain,
                     nround = nround)
  print(model)
  return(model)
})

xgb.save(xgb_tree[[1]],'xgb_1_20170724')
xgb.save(xgb_tree[[2]],'xgb_2_20170724')
xgb.save(xgb_tree[[3]],'xgb_3_20170724')
xgb.save(xgb_tree[[4]],'xgb_4_20170724')
xgb.save(xgb_tree[[5]],'xgb_5_20170724')

importancia1 <- xgb.importance(model=xgb_tree[[1]],
                               feature_names = names(datos_modeliza)[!names(datos_modeliza)%in%c(id,target)])
View(importancia1)

# Fase 1
prob_tree <- lapply(xgb_tree,function(x){
  pred <- t(matrix(predict(x, dtest),nrow=4,ncol=8427))
  print('aaa')
  return(pred)
})

prob_tree_1 <- Reduce('+',prob_tree)
prob_tree_1 <- prob_tree_1/5
prob_tree_1 <- data.frame(prob_tree_1)
colnames(prob_tree_1) <- c(1,2,3,4)

data_subir <- data.frame(COD_ENCUESTADO = datos_modeliza[is.na(datos_modeliza$NPS),
                                                         id],
                         prob_tree_1)
names(data_subir) <- c('COD_ENCUESTADO','1','2','3','4')

write.csv(data_subir,'subida_xgb_20170724.csv',row.names = F)
