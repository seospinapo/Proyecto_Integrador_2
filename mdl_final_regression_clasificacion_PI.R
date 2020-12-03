library(caret)
library(sqldf)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(purrr)


## Lectura de datos
data <- read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
puntos_criticos <- read.table(file.choose(), header=TRUE, sep = ';')


t<-proc.time()
distancias<-data.frame()

for(i in 1:20) {
  x=c(puntos_criticos[i,3],puntos_criticos[i,2])
  for(j in 1:nrow(data)){
    y=c(data[j,1],data[j,2])
    d<-dist(rbind(x,y))/0.000009009020
    #d<-(sqrt((puntos_criticos[i,2]-data[j,2])^2+(puntos_criticos[i,3]-data[j,1])^2))/ 0.000009009020 
    distancias[j,i]<-d
  }
}



proc.time()-t

grupo<-replicate(nrow(data), 0)
grupo<-data.frame(grupo)


for(j in 1:nrow(data)) {
  for(i in 1:20){
    if(distancias[j,i]<=500 && distancias[j,i]== min(distancias[j,1:20])) {
      grupo[j,1]=i
    } else {
      a=0
    }
  }
}
data["Grupo"]<-grupo
names(data)[1] <- "XF"
names(data)[14] <- "MES2"
names(puntos_criticos)[1] <- "Grupo"

datos_agrupados<-sqldf("SELECT a.Grupo,a.HORA2,a.PERIODO,DIA_NOMBRE, b.VelocidadMaxima, AVG(a.Poblacion) AS Poblacion, AVG(a.Precipitacion) AS Precipitacion, COUNT(a.OBJECTID) AS Eventos 
                        FROM data a 
                       inner join puntos_criticos as b on (a.Grupo = b.Grupo) GROUP BY 1,2,3,4")

datos_agrupados<-datos_agrupados[datos_agrupados$Grupo>0,]
datos_agrupados$HORA2[datos_agrupados$HORA2 %in% "#N/D"] <- "-1"
datos_agrupados$HORA2<-as.numeric(datos_agrupados$HORA2)
datos_agrupados<-datos_agrupados[datos_agrupados$HORA2>-1,]
datos_agrupados<-datos_agrupados[datos_agrupados$Poblacion>-1,]
datos_agrupados$Precipitacion <- gsub('-','0',datos_agrupados$Precipitacion)
datos_agrupados$Precipitacion<-as.numeric(datos_agrupados$Precipitacion)
datos_agrupados$Grupo<-as.character(datos_agrupados$Grupo)

datos <- datos_agrupados


############################################################
############ SCript Base Modelacion en Caret
library(caret)

datos$PERIODO <- NULL

#datos2 <- subset(datos, datos$Eventos <= 11)

### Validacion Cruzada 
## hiperparamtros

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  search = "random")  # hyper-parameters random search 


model1 <- train(Eventos ~ .,
                  data = datos,
                  method = "lm",
                  trControl = fitControl,
                  preProcess = c('scale', 'center'),
                  na.action = na.omit)

model1


model2 <- train(Eventos ~ .,
                    data = datos,
                    method = "ridge",
                    trControl = fitControl,
                    preProcess = c('scale', 'center'),
                    na.action = na.omit)

model2
plot(model2)

k <- expand.grid(k = seq(2, 10, length=5))
model3 <- train(Eventos ~ .,
                data = datos,
                method = "knn",
                tuneGrid = k,
                trControl = fitControl,
                preProcess = c('scale', 'center'),
                na.action = na.omit)

model3
plot(model3, main = 'K óptimo bajo RMSE')

## Ver importancia de las variables
ggplot(varImp (model3))

## Resumen de todos los modelos
allResamples <- resamples(list("Regresion KNN" = model3, 
                               "Regresion Ridge" = model2, 
                               "Regresion Lineal" = model1))

## Comparativo RMSE, Rsquared, MAE
par(mfrow = c(1,3))
parallelplot(allResamples, metric = 'RMSE', main ='Comparativo de Regresiones - RMSE (Cross Validation)')
parallelplot(allResamples, metric = 'Rsquared', main ='Comparativo de Regresiones - R cuadrado (Cross Validation)')
parallelplot(allResamples, metric = 'MAE', main ='Comparativo de Regresiones - MAE (Cross Validation)')


## Predicciones
predicciones_knn <- predict(model3, datos) 
predicciones_ridge <- predict(model2, datos) 
predicciones_lm <- predict(model1, datos) 


## Reales vs predichos
windows()
par(mfrow = c(3,1))
plot(datos$Eventos,predicciones_knn, xlab = 'Accidentes Reales', ylab = 'Predicciones KNN', ylim = c(-2,25), col = 'darkcyan', main="Reales vs Predicciones KNN")
abline(0, 1, col = "darkgrey", lty = 2)
plot(datos$Eventos,predicciones_ridge, xlab = 'Accidentes Reales', ylab = 'Predicciones Ridge', ylim = c(-2,25), col = 'darkcyan', main="Reales vs Predicciones Ridge")
abline(0, 1, col = "darkgrey", lty = 2)
plot(datos$Eventos,predicciones_lm, xlab = 'Accidentes Reales', ylab = 'Predicciones Regresión Lineal', ylim = c(-2,25), col = 'darkcyan', main="Reales vs Pred. Regresión Lineal")
abline(0, 1, col = "darkgrey", lty = 2)

## Residuales
residuales_knn <- residuals(model3)
residuales_ridge <- residuals(model2)
residuales_ln <- residuals(model1)


## Grafico Residuales
par(mfrow = c(1,3))
boxplot(residuals(model3), ylim = c(-6,16), main="Boxplot Residuales KNN")
boxplot(residuals(model2), ylim = c(-6,16), main="Boxplot Residuales Ridge")
boxplot(residuals(model1), ylim = c(-6,16), main="Boxplot Residuales Regresión Lineal")

## Distribucion de los residuales
par(mfrow = c(3,1))
plot(residuals(model3), ylim = c(-6,16), ylab='Residuales KNN', main="Residuales KNN")
plot(residuals(model2), ylim = c(-6,16), ylab='Residuales Ridge', main="Residuales Ridge")
plot(residuals(model1), ylim = c(-6,16), ylab='Residuales Regresión Lineal', main="Residuales Regresión Lineal")

 
## Predicciones vs residuales
par(mfrow = c(3,1))
plot(predicciones_knn, residuales_knn,xlab='Predicciones', ylab = "residuales KNN", col = 'black', main="Residuales KNN vs. Valores Ajustados")
abline(h = 0, col = "darkgrey", lty = 2)
plot(predicciones_knn, residuales_ridge,xlab='Predicciones', ylab = "residuales Ridge", col = 'black', main="Residuales Ridge vs. Valores Ajustados")
abline(h = 0, col = "darkgrey", lty = 2)
plot(predicciones_knn, residuales_ln,xlab='Predicciones', ylab = "residuales Regresión Lineal", col = 'black', main="Residuales Regresion Lineal vs. Valores Ajustados")
abline(h = 0, col = "darkgrey", lty = 2)


########Clasificación

data$GRAVEDAD <- gsub("CON HERIDOS", "HERIDO", data$GRAVEDAD)
data$GRAVEDAD <- gsub("CON MUERTOS", "MUERTO", data$GRAVEDAD)
names(data)[1] <- "XF"
data<-data[data$PERIODO<2020,]
data1<-data[c("XF","YF","HORA2","GRAVEDAD","DIA_NOMBRE","NUMERO_COM","Poblacion","Precipitacion","Grupo")]
data1$HORA2[data1$HORA2 %in% "#N/D"] <- "-1"
data1$HORA2<-as.numeric(data1$HORA2)
data1<-data1[data1$HORA2>-1,]
data1<-data1[data1$Poblacion>-1,]
data1$Precipitacion <- gsub('-','0',data1$Precipitacion)
data1$Precipitacion<-as.numeric(data1$Precipitacion)
data1$Grupo<-as.character(data1$Grupo)


##Separación de muestras
IndicesEntrenamiento <- createDataPartition(y = data1$GRAVEDAD,
                                            p = 0.7,
                                            list = FALSE)
Entrenamiento <- data1[IndicesEntrenamiento,]
Test <- data1[-IndicesEntrenamiento,]



#Clasificadores
t<-proc.time()

modelo1 <- train(GRAVEDAD~.,
                 data = Entrenamiento,
                 method = "xgbLinear")
proc.time()-t


t<-proc.time()

modelo12<- train(GRAVEDAD~.,
                 data = Entrenamiento,
                 method = "xgbTree")
proc.time()-t

xgb.plot.multi.trees(modelo12$finalModel, features_keep = 3, use.names=FALSE)

t<-proc.time()

modelo13<- train(GRAVEDAD~.,
                 data = Entrenamiento,
                 method = "kknn")
proc.time()-t


t<-proc.time()

modelo14<- train(GRAVEDAD~.,
                 data = Entrenamiento,
                 method = "rf") 
proc.time()-t


varImp(modelo1, scale = FALSE)

varImp(modelo12, scale = FALSE)

varImp(modelo14, scale = FALSE)

t<-proc.time()

modelo15<- train(Grupo~.,
                 data = Entrenamiento,
                 method = "kknn") 
proc.time()-t


t<-proc.time()

modelo16<- train(GRAVEDAD~.,
                 data = Entrenamiento,
                 method = "naive_bayes") 
proc.time()-t


t<-proc.time()

modelo17<- train(GRAVEDAD~.,
                 data = Entrenamiento,
                 method = "mlp") 
proc.time()-t


t<-proc.time()




#Se agrupan los modelos
modelos <- list(NB = modelo16,
                MLP = modelo17, KNN = modelo13, xgbTree = modelo12, rf = modelo14,
                xgboost = modelo1)

resultados_resamples <- resamples(modelos)
resultados_resamples$values %>% head(10)


metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))

metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.52, linetype = "dashed") +
  annotate(geom = "text", y = 0.52, x = 6.5, label = "Accuracy basal") +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() +
  theme_bw()

matriz_metricas <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  spread(key = modelo, value = valor) %>%
  select(-Resample, -metrica) %>% as.matrix()
friedman.test(y = matriz_metricas)


# Comparaciones múltiples de los modelos

metricas_accuracy <- metricas_resamples %>% filter(metrica == "Accuracy")
comparaciones  <- pairwise.wilcox.test(x = metricas_accuracy$valor, 
                                       g = metricas_accuracy$modelo,
                                       paired = TRUE,
                                       p.adjust.method = "holm")

# Se almacenan los p_values en forma de dataframe
comparaciones <- comparaciones$p.value %>%
  as.data.frame() %>%
  rownames_to_column(var = "modeloA") %>%
  gather(key = "modeloB", value = "p_value", -modeloA) %>%
  na.omit() %>%
  arrange(modeloA) 

comparaciones

unouno<-as.factor(Test$GRAVEDAD)
predictions1 <- predict(modelo16, Test, type = "raw")
predictions2 <- predict(modelo17, Test, type = "raw")
predictions3 <- predict(modelo13, Test, type = "raw")
predictions4 <- predict(modelo12, Test, type = "raw")
predictions5 <- predict(modelo14, Test, type = "raw")
predictions6 <- predict(modelo1, Test, type = "raw")



porcen_entre<-metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor))
porcen_entre["dataType"]="Training"

compute.A.conditional <- function(pred.matrix, i, j, ref.outcome) {
  i.idx <- which(ref.outcome == i)
  j.idx <- which(ref.outcome == j)
  pred.i <- pred.matrix[i.idx, i] # p(G = i) assigned to class i observations
  pred.j <- pred.matrix[j.idx, i] # p(G = i) assigned to class j observations
  all.preds <- c(pred.i, pred.j)
  classes <- c(rep(i, length(pred.i)), rep(j, length(pred.j)))
  o <- order(all.preds)
  classes.o <- classes[o]
  Si <- sum(which(classes.o == i))
  ni <- length(i.idx)
  nj <- length(j.idx)
  A <- (Si - ((ni * (ni + 1))/2)) / (ni * nj)
  return(A)
}

multiclass.auc <- function(pred.matrix, ref.outcome) {
  labels <- colnames(pred.matrix)
  A.ij.cond <- utils::combn(labels, 2, function(x, pred.matrix, ref.outcome) {x
    i <- x[1]
    j <- x[2]
    A.ij <- compute.A.conditional(pred.matrix, i, j, ref.outcome)
    A.ji <- compute.A.conditional(pred.matrix, j, i, ref.outcome)
    pair <- paste0(i, "/", j)
    return(c(A.ij, A.ji))
  }, simplify = FALSE, pred.matrix = pred.matrix, ref.outcome = ref.outcome)
  c <- length(labels)
  pairs <- unlist(lapply(combn(labels, 2, simplify = FALSE), function(x) paste(x, collapse = "/")))
  A.mean <- unlist(lapply(A.ij.cond, mean))
  names(A.mean) <- pairs
  A.ij.joint <- sum(unlist(A.mean))
  M <- 2 / (c * (c-1)) * A.ij.joint 
  attr(M, "pair_AUCs") <- A.mean
  return(M)
}


predictions11 <- predict(modelo16, Test, type = "prob")
predictions22 <- predict(modelo17, Test, type = "prob")
predictions33 <- predict(modelo13, Test, type = "prob")
predictions44 <- predict(modelo12, Test, type = "prob")
predictions55 <- predict(modelo14, Test, type = "prob")
predictions66 <- predict(modelo1, Test, type = "prob")

M <- multiclass.auc(predictions11, unouno)
print(paste0("Generalized AUC is: ", round(as.numeric(M), 3)))
M <- multiclass.auc(predictions22, unouno)
print(paste0("Generalized AUC is: ", round(as.numeric(M), 3)))
M <- multiclass.auc(predictions33, unouno)
print(paste0("Generalized AUC is: ", round(as.numeric(M), 3)))
M <- multiclass.auc(predictions44, unouno)
print(paste0("Generalized AUC is: ", round(as.numeric(M), 3)))
M <- multiclass.auc(predictions55, unouno)
print(paste0("Generalized AUC is: ", round(as.numeric(M), 3)))
M <- multiclass.auc(predictions66, unouno)
print(paste0("Generalized AUC is: ", round(as.numeric(M), 3)))

porcen_prediccion<-list(NB = confusionMatrix(predictions1, unouno)$overall[1], #logistic = modelo18,
                        MLP = confusionMatrix(predictions2, unouno)$overall[1], KNN = confusionMatrix(predictions3, unouno)$overall[1],
                        xgbTree = confusionMatrix(predictions4, unouno)$overall[1], rf = confusionMatrix(predictions5, unouno)$overall[1],
                        xgboost = confusionMatrix(predictions6, unouno)$overall[1])
porcen_prediccion<-data.frame(t(porcen_prediccion))

porcen_prediccion["modelo"]<-row.names(porcen_prediccion) 
row.names(porcen_prediccion) <- NULL
porcen_prediccion["dataType"]="Test"
names(porcen_entre)[2] <- "Accuracy"
porcen_prediccion1 = porcen_prediccion [ , c(2,1,3)]
metricas_predicciones <- rbind(porcen_prediccion1, porcen_entre)



datos_testeo<-metricas_predicciones %>%
  spread(key = dataType, value = Accuracy) %>%
  arrange(desc(Test))


ggplot(data = metricas_predicciones,
       aes(x = reorder(modelo, Accuracy), y = Accuracy,
           color = dataType, label = round(Accuracy, 2))) +
  geom_point(size = 7) +
  scale_color_manual(values = c("orangered2", "gray50")) +
  geom_text(color = "white", size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.52, linetype = "dashed") +
  annotate(geom = "text", y = 0.52, x = 6.5, label = "Accuracy basal") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", 
       x = "modelo") +
  theme_bw() + 
  theme(legend.position = "bottom")

ggplot(data = datos_testeo,
       aes(x = reorder(modelo, Test), y = Test,
           color = "orangered2", label = round(Test, 2))) +
  geom_point(size = 8) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.52, linetype = "dashed") +
  annotate(geom = "text", y = 0.52, x = 6.5, label = "Accuracy basal") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", 
       x = "modelo") +
  theme_bw() + 
  theme(legend.position = "bottom")


