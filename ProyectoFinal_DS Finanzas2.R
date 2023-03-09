library(quantmod)
library(TTR)
library(zoo)
library(randomForest)
library(dplyr)
library(tidyverse)
library(ggplot2)




# MAGENES Y RESETEAR ENTORNO GRAFICO
par(mar=c(0,0,0,0))
dev.off()

# CARGANDO CSV
df2 <- read.table(file="EUROZONA.csv",  header=T, sep = ",")
View(df2)



str(df2)
?read.csv


# CAMBIO DE NOMBRES DE COLUMNAS 
nombres_de_col <- names(df2)
nombres_de_col[1] = "Fecha"
nombres_de_col 
names(df2) = nombres_de_col
View(df2)



#PLOTEO
plot(df2[,2] , main = "Evolucion Index Eurozona")
df2 = na.omit(df2)
View(df2)


#ROWNAMES
rownames(df2) = df2$Fecha
df2 = df2[-1]



# PARAMETROS 
h_pred = 5 #HORIZONTE DE PREDICCION
cutoff  = 0.5# PARA DEFINIR ERROR DE PREDICCION
mty = 7 #MTRY DEL RANDOM FOREST
ntr = 200 #NUMERO DE ARBOLES
nsize = 13 #NUMERO DE NODOS MINIMOS QUE LE EXIJO A CADA ARBOL
corte_cla = 0.0046


# ACA NECESITAMOS DEFINIR POR LO MENOS 2 
# VER CON CUALES QUEDARNOS Y PORQUE
#Defino nuevas variables (Independientes)
df2$MA5  = df2[,2] / SMA (df2[,2],5)
df2$MA10 = df2[,2] / SMA (df2[,2],10)
df2$MA20 = df2[,2] / SMA (df2[,2],20)
df2$MA50 = df2[,2] / SMA (df2[,2],50)
df2$MA100 = df2[,2] /SMA (df2[,2],100)
df2$MA150 = df2[,2] /SMA (df2[,2],150)
df2$MA200 = df2[,2] /SMA (df2[,2],200)



df2$MA5MA100 = df2$MA5 / df2$MA100
df2$MA20MA50 = df2$MA20 / df2$MA50



df2$varMA100 = ROC ( df2$MA100, 50)
df2$varMA150 = ROC ( df2$MA150, 50)



df2$RSI10 = RSI ( df2[,2], 10)
df2$RSI20 = RSI ( df2[,2], 20)
df2$RSI50 = RSI ( df2[,2], 50)
df2$RSI100 = RSI ( df2[,2], 100)
df2$ROC5 = ROC ( df2[,2], 5)
df2$ROC10 = ROC ( df2[,2], 10)
df2$ROC20 = ROC ( df2[,2], 20)
df2$ROC40 = ROC ( df2[,2], 40)
df2$ROC60 = ROC ( df2[,2], 60)

df2$VOLAT20= volatility (df2[,2], 20)
df2$VOLAT40= volatility (df2[,2], 40)
df2$VOLAT60= volatility (df2[,2], 60)



df2$VOLAT20AC= df2$VOLAT20/ SMA (df2$VOLAT20, 20)

#VARIABLE INVENTADA
#df2$MA365 = df2[,2] /SMA (df2[,2],365)


View(df2)

# MACD
df2$MACD = MACD(df2$Index,nFast = 12, nSlow = 26, nSig = 9)[,1] 
View(df2)

     



ggp <- ggplot(df2)  + 
  geom_histogram(aes(MACD),bins = 10, fill="cyan",colour="#006000")
  
  
ggp


#DEFINO VARIABLES DEPENDIENTE

df2$y = 1; df2$y = Next((df2[,2]/Lag(df2[,2],h_pred)-1),h_pred)
df2$yc =1; df2$yc = ifelse ( df2$y > corte_cla , 1, 0)
View(df2)


#Defino train y test
corte = round((nrow(df2)-h_pred)*0.5,0)
train = seq(200, corte)
test = seq(corte+1, (nrow(df2)*0.75))
valid = seq(tail(test,1)+1, (nrow(df2)-h_pred))


#Corro modelo Random Forest con los valores por defecto
set.seed(11)
modelo_rf = randomForest(as.factor(yc) ~  MA5 + MA10+ MA20+ MA50+ MA100+ MA150+ MA200+MA5MA100+ MA20MA50 +
                           RSI10 +RSI20+RSI50+RSI100+ varMA100+varMA150+
                           ROC5+ROC10+ROC20+ROC40+ROC60 + MACD  +
                           VOLAT20 + VOLAT40 + VOLAT60 + VOLAT20AC,
                         data = df2[train,], nodesize = nsize, mtry = mty, ntree = ntr)




importance(modelo_rf)
varImpPlot(modelo_rf)



#Agrego la prediccion a la base
y_p_train = as.numeric(as.character(predict(modelo_rf,df2[train,], type = "prob")[,'1']))
y_p_test = as.numeric(as.character(predict(modelo_rf, df2[test,],  type = "prob")[,'1']))
y_p_valid = as.numeric(as.character(predict(modelo_rf, df2[valid,],  type = "prob")[,'1']))



df2$y_pred = 0; 
df2$y_pred[train] <- ifelse (y_p_train >cutoff,1,0)



df2$y_pred[test] <- ifelse (y_p_test>cutoff,1,0)



df2$y_pred[valid] <- ifelse (y_p_valid>cutoff,1,0)



#Accuracy del modelo en Train
acctrain = sum(diag (table(df2$yc[train] ,df2$y_pred[train]))) / sum(table(df2$yc[train] ,df2$y_pred[train]))
acctrain



#Accuracy del modelo en Test
acctest = sum(diag (table(df2$yc[test] ,df2$y_pred[test]))) / sum(table(df2$yc[test] ,df2$y_pred[test]))
acctest



#Accuracy del modelo en Valid
accval = sum(diag (table(df2$yc[valid] ,df2$y_pred[valid]))) / sum(table(df2$yc[valid] ,df2$y_pred[valid]))
accval



#Backtesting


#Rendimiento de la estrategia en Train
df2$r5d =  df2$y * df2$y_pred
df2$r5d =  Lag(df2$r5d, h_pred) 
vec1 = seq(200,tail(train,1), by=h_pred)
plot(cumprod (1+df2$r5d[vec1]))


##Rendimiento de la estrategia en Train
#IND$r5d =  IND$y * IND$y_pred
#IND$r5d =  Lag(IND$r5d, h_pred) #corrijo el retorno para que quede pasado
#vec1 = seq(200,tail(train,1), by=h_pred)
#plot(cumprod (1 + IND$r5d[vec1]))


#Rendimiento de la estrategia en Test
vec2 = seq(tail(train,1)+h_pred,tail(test,1), by=h_pred)
plot(cumprod (1 + df2$r5d[vec2]))


#Rendimiento de la estrategia en Valid
vec3 = seq(tail(test,1)+h_pred,tail(valid,1), by=h_pred)
plot(cumprod (1 + df2$r5d[vec3]))


#Comparo con mi df2ice benckmark en train
estrategia = cumprod (1 + df2$r5d[vec1])
result = cbind(df2[index(estrategia), "Index"] /as.numeric(df2[index(estrategia), "Index"][1] ),
               estrategia)
plot(result, col = c("blue", "red"),main = "Estrategia vs Benckmark")


#Comparo con mi df2ice benckmark en test
estrategia = cumprod (1 + df2$r5d[vec2])
result = cbind(df2[index(estrategia), "Index"] /as.numeric(df2[index(estrategia), "Index"][1] ),
               estrategia)
plot(result, col = c("blue", "red"),main = "Estrategia vs Benckmark")



estrategia = cumprod (1 + df2$r5d[,"Lag.5"][vec3])
result = cbind(df2[index(estrategia), "Index"] /as.numeric(df2[index(estrategia), "Index"][1] ),
               estrategia)
plot(result, col = c("blue", "red"),main = "Estrategia vs Benckmark")




#Comparo con mi df2ice benckmark en valid
estrategia = cumprod (1 + df2$r5d[vec3])
result = cbind(df2[index(estrategia), "Index"] /as.numeric(df2[index(estrategia), "Index"][1] ),
               estrategia)
plot(result, col = c("blue", "red"),main = "Estrategia vs Benckmark")







install.packages("rmarkdown")
library(rmarkdown)




