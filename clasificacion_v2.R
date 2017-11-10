
# Versión 2.0
# David López

library(magrittr)
library(ggplot2)

#Fuente: http://archive.ics.uci.edu/ml/datasets/Bank+Marketing
# Obtención de los datos
system('cd ~/datos; wget http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip; unzip bank.zip')


# Carga

# Input variables:
# # bank client data:
# 1 - age (numeric)
# 2 - job : type of job (categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')
# 3 - marital : marital status (categorical: 'divorced','married','single','unknown'; note: 'divorced' means divorced or widowed)
# 4 - education (categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown')
# 5 - default: has credit in default? (categorical: 'no','yes','unknown')
# 6 - housing: has housing loan? (categorical: 'no','yes','unknown')
# 7 - loan: has personal loan? (categorical: 'no','yes','unknown')
# # related with the last contact of the current campaign:
# 8 - contact: contact communication type (categorical: 'cellular','telephone') 
# 9 - month: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
# 10 - day_of_week: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')
# 11 - duration: last contact duration, in seconds (numeric). Important note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.
# # other attributes:
# 12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
# 13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
# 14 - previous: number of contacts performed before this campaign and for this client (numeric)
# 15 - poutcome: outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')
# # social and economic context attributes
# 16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)
# 17 - cons.price.idx: consumer price index - monthly indicator (numeric) 
# 18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
# 19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
# 20 - nr.employed: number of employees - quarterly indicator (numeric)
# 
# Output variable (desired target):
#   21 - y - has the client subscribed a term deposit? (binary: 'yes','no')

nombres <- c('cli.edad',
             'cli.empleo',
             'cli.estado.civil',
             'cli.educacion',
             'cli.credito.mora',
             'cli.hipoteca',
             'cli.prestamo',
             'cam.tipo.contacto',
             'cam.ult.dia.semana',
             'cam.ult.mes',
             'cam.duracion.seg',
             'otr.actual.num.contactos',
             'otr.cam.previa.num.contactos',
             'otr.cam.previa.dias.contacto',
             'otr.cam.previa.',
             'otr.cam.previa.resultado',
             #'soc.tasa.var.empleo.q',
             #'soc.ipc.m',
             #'soc.icc.m',
             #'soc.euribor3m.d', #tipo europeo de oferta interbancaria
             #'soc.num.empleados.q',
             'res.contrato')

#setwd('C:/Users/Documents/blah/....')


datos.train <- read.csv('~/datos/bank-full.csv',header = T,sep = ';')
head(datos.train)
# names(datos.train) <- nombres
datos.test <- read.csv('~/datos/bank.csv',header = T,sep = ';')
head(datos.test)
dim(datos.train)
dim(datos.test)

summary(datos.train)
summary(datos.test)

# Conversión de variables categóricas

# Hay modelos que no usan variables categóricas, o mejor dicho, no realizan una conversión de esas variables
# Por tanto, hay que convertirlas nosotros
library(dummies)
head(dummy.data.frame(datos.test,sep='.'))
cor(dummy.data.frame(datos.test,sep='.'))
cor(dummy.data.frame(datos.test,sep='.')[,c('y.no','y.yes')])
# Hay que quitar las redundantes (cor = 1, ej. genero.m=1 => genero.f=1)
# default.no, housing.no, loan.no, y.no
excluir <- which(names(dummy.data.frame(datos.test,sep='.')) %in% c('default.no', 'housing.no', 'loan.no', 'y.no'))
datos.train.dum <- dummy.data.frame(datos.train,sep='.')[,-excluir]
datos.test.dum <- dummy.data.frame(datos.test,sep='.')[,-excluir]
head(datos.train.dum)
nom <- names(datos.test.dum)

# Visualizando un poco... cuidado porque es grande... vamos a recortar unas variables...
library(GGally)
#ggpairs(datos.test.dum[,c('age','marital.divorced','marital.married','marital.single','education.secondary','month.jan','month.may','pdays','previous','duration','campaign','poutcome.success','y.yes')])

ggpairs(datos.test.dum[,c('age','balance','duration','y.yes')])

# Revisemos los datos, buscando outliers
ggplot(datos.train.dum) + geom_histogram(aes(x=balance))
boxplot(datos.train.dum$balance)
quantile(datos.train.dum$balance)

# Hay que escalar los datos entre 0 y 1
datos.train.dum <- as.data.frame(apply(as.matrix(datos.train.dum), MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
datos.test.dum <- as.data.frame(apply(as.matrix(datos.test.dum), MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
summary(datos.train.dum)

# Vamos a hacer una clasificación rápida, con todas estas variables.

# Modelo lineal
dat.lm <- datos.train.dum %>% lm(formula= y.yes ~ .)
dat.lm
plot(dat.lm)
datos.train.dum %>% lm(formula= y.yes ~ balance)
datos.train.dum %>% lm(formula= y.yes ~ balance + age)
datos.train.dum %>% lm(formula= y.yes ~ balance * age)

pred.lm <- predict(dat.lm,datos.test.dum[,-49])
resultados <- data.frame(real=datos.test.dum[,49],predicho.lm=round(pred.lm,3),type='response')
resultados <- resultados[with(resultados,order(real)),]
ggplot(resultados) + geom_point(aes(x=1:nrow(resultados),y=predicho.lm),color='red',alpha=.3) + geom_point(aes(x=1:nrow(resultados),y=real),color='blue',alpha=.3)

# Definitivamente podría ser mejor...

# Cuál es la razón de que no sea tan buen modelo?
# 1. Podría ser porque no es tan exacto (lo vimos la clase pasada)
# 2. No es el modelo correcto para lo que queremos hacer (ESTA ES LA RAZÓN PRINCIPAL!)
# Entonces, ¿Qué modelos se pueden usar para clasificar?
# Probemos con: a) logístico, b) SVM, c) Redes Neuronales

# Antes: Ejemplo de un buen uso de modelo lineal
head(faithful)
ggplot(faithful) + geom_point(aes(x=waiting,y=eruptions,color=eruptions,size=eruptions,alpha=.7))
faithful.lm <- faithful %>% lm(formula= eruptions ~ waiting)
summary(faithful)
modelo <- predict(faithful.lm,data.frame(waiting=seq(40,100,5)))
linea.faithful <- data.frame(eruptions=modelo,waiting=seq(40,100,5))
ggplot(faithful,aes(x=waiting,y=eruptions,color=eruptions,size=eruptions,alpha=.7)) + geom_point() + geom_smooth(method = "lm", se = FALSE,color='red')


# Ahora bien...
datos.train.dum <- dummy.data.frame(datos.train,sep='.')[,-excluir]
datos.test.dum <- dummy.data.frame(datos.test,sep='.')[,-excluir]

# Podríamos quitar los valores atípicos, outliers
# Inferior: Q1 – 1.5×IQR
# Superior: Q3 + 1.5×IQR
hist(datos.train.dum$balance)
cuartiles.tr <- quantile(datos.train.dum$balance)
cuartiles.tr
iqr <- cuartiles.tr[4] - cuartiles.tr[1]
out.inf <- cuartiles.tr[1] - 1.5*iqr
out.sup <- cuartiles.tr[4] + 1.5*iqr
dtrain <- datos.train.dum[-c(which(datos.train.dum$balance <= out.inf),which(datos.train.dum$balance >= out.sup)),]
nrow(datos.train.dum)
nrow(dtrain)

# Comprobando...
par(mfrow=c(1,2))
hist(datos.train.dum$balance)
hist(dtrain$balance)
par(mfrow=c(1,1))

# OK... Podríamos analizar e incluir todas las columnas, o quitar las categóricas.
# La primera no es opción porque puede ser costosa computacionalmente (sí, aún si es big data, no es lo mejor darle todas las variables nada más porque sí)
# La segunda tampoco porque se pierde información
#Por tanto...

# Reducción de dimensionalidad
dtrain.2 <- dtrain[,names(dtrain)[1:(dim(dtrain)[2]-1)]]
datos.pca <- prcomp(dtrain.2,
                    center = TRUE,
                    scale. = TRUE) 
print(datos.pca)
plot(datos.pca,type='l')
summary(datos.pca)

# Usando el paquete Caret
library(caret)
library(e1071)
# PCA, pero antes BoxCox, Centrar y Escalar, para corregir sesgos (skewness)
dtrain.pca <- preProcess(dtrain.2, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
print(dtrain.pca)
dtrain.pca
dtrain.pca <- preProcess(dtrain.2, 
                        method=c("BoxCox", "center", 
                                 "scale", "pca"),pcaComp = 30)
dtrain.transf <- cbind(predict(dtrain.pca,dtrain.2),dtrain[,'y.yes'])
head(dtrain.transf)
names(dtrain.transf)[dim(dtrain.transf)[2]] <- 'y.yes'
head(dtrain.transf)

# De una vez lo aplicamos al de prueba (Test)
# Primero quitamos los outliers
hist(datos.test$balance)
cuartiles.tst <- quantile(datos.test.dum$balance)
cuartiles.tst
iqr <- cuartiles.tst[4] - cuartiles.tst[1]
out.inf <- cuartiles.tst[1] - 1.5*iqr
out.sup <- cuartiles.tst[4] + 1.5*iqr
dtest <- datos.test.dum[-c(which(datos.test.dum$balance <= out.inf),which(datos.test.dum$balance >= out.sup)),]
nrow(datos.test.dum)
nrow(dtest)

dtest.2 <- dtest[,names(dtest)[1:(dim(dtest)[2]-1)]]
dtest.transf <- cbind(predict(dtrain.pca,dtest.2),dtest[,'y.yes'])
names(dtest.transf)[dim(dtest.transf)[2]] <- 'y.yes'
head(dtest.transf)


# Ahora sí, apliquemos los modelos

# Regresión logística
modelo.glm <- dtrain.transf %>%
  glm(formula= y.yes ~ .,family=binomial(link='logit'))
modelo.glm
plot(modelo.glm)

glm.pred <- predict(modelo.glm,dtest.transf[,names(dtest.transf)[1:(dim(dtest.transf)[2]-1)]],type='response')
resultado <- data.frame(real=dtest.transf[,'y.yes'],glm.pred)
resultado <- resultado[with(resultado,order(real,glm.pred)),]
ggplot(resultado) + geom_point(aes(x=1:nrow(dtest.transf),y=real),color='blue',alpha=.3) + geom_point(aes(x=1:nrow(dtest.transf),y=glm.pred),color='red',alpha=.3)

# Si decimos que todo lo >= .5 es y.yes = 1...

resultado <- data.frame(real=dtest.transf[,'y.yes'],glm.pred=ifelse(glm.pred>=.5,1,0))
resultado <- resultado[with(resultado,order(real,glm.pred)),]
ggplot(resultado) + geom_point(aes(x=1:nrow(dtest.transf),y=real),color='blue',alpha=.3) + geom_point(aes(x=1:nrow(dtest.transf),y=glm.pred),color='red',alpha=.3)
tmp.res <- table(resultado)
tmp.res
error.glm <- 1-sum(diag(tmp.res))/nrow(resultado)
error.glm

# Ojo: El número de gente que acepta es predicho con una exactitud de...
tmp.res[2,2]/sum(tmp.res[2,])

# Ahora, con Redes Neuronales
library(neuralnet)
nn <- dtrain.transf %>%
  neuralnet(formula = y.yes ~ PC1 + PC2 + PC3 + PC4 + PC5,hidden = c(5),rep = 10)
# También se puede usar as.formula
plot(nn)
nn.pred <- compute(nn,dtest.transf[,names(dtest.transf)[1:(dim(dtest.transf)[2]-1)]])
resultado <- data.frame(real=dtest.transf[,'y.yes'],nn.pred)
resultado <- resultado[with(resultado,order(real,nn.pred)),]
ggplot(resultado) + geom_point(aes(x=1:nrow(dtest.transf),y=real),color='blue',alpha=.3) + geom_point(aes(x=1:nrow(dtest.transf),y=nn.pred),color='red',alpha=.3)





#pr.nn_ <- nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
#test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
#MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

#OJO: NO estamos haciendo validaciones ahorita. Estas son SÚPER necesarias. Vean el tema de validación cruzada.



