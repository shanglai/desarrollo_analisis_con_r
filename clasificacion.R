
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
ggpairs(datos.test.dum[,c('age','marital.divorced','marital.married','marital.single','education.secondary','month.jan','month.may','pdays','previous','duration','campaign','poutcome.success','y.yes')])

# Revisemos los datos, buscando outliers
ggplot(datos.train.dum) + geom_histogram(aes(x=balance))
boxplot(datos.train.dum$balance)
quantile(datos.train.dum$balance)

# Podríamos quitar los valores atípicos, outliers, porque nos ocasionarán problemas después (esto queda de tarea)

# Hay que escalar los datos entre 0 y 1
datos.train.dum <- as.data.frame(apply(as.matrix(datos.train.dum), MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
datos.test.dum <- as.data.frame(apply(as.matrix(datos.test.dum), MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
summary(datos.train.dum)

# Vamos a hacer una clasificación rápida, con todas estas variables.

# Modelo lineal
dat.lm <- datos.train.dum %>% lm(formula= y.yes ~ .)
dat.lm
plot(dat.lm)
pred.lm <- predict(dat.lm,datos.test.dum[,-49])
resultados <- data.frame(real=datos.test.dum[,49],predicho.lm=round(pred.lm,3))
resultados <- resultados[with(resultados,order(predicho.lm)),]
ggplot(resultados) + geom_point(aes(x=1:nrow(resultados),y=real,color='blue',alpha=.75)) + geom_point(aes(x=1:nrow(resultados),y=predicho.lm,color='red',alpha=.75))

# Definitivamente podría ser mejor...

#OJO: NO estamos haciendo validaciones ahorita. Estas son SÚPER necesarias. Vean el tema de validación cruzada.

# Hagamos un modelo diferente...
library(randomForest)
dat.rf <- datos.train.dum %>% randomForest(formula= y.yes ~ .,ntree=50)
dat.


dat.glm <- datos.train.dum %>% glm(formula= y.yes ~ .,family=binomial(link='logit'))
dat.glm
pred.glm <- predict(dat.glm,datos.test.dum[,-49])
resultados <- cbind(resultados,predicho.glm=round(pred.glm,3))
resultados <- resultados[with(resultados,order(predicho.glm)),]
ggplot(resultados) + geom_point(aes(x=1:nrow(resultados),y=real,color='blue',alpha=.75)) + geom_point(aes(x=1:nrow(resultados),y=predicho.lm,color='red',alpha=.75)) + geom_point(aes(x=1:nrow(resultados),y=predicho.glm,color='green',alpha=.75))



# Tarea (moral):
# Prueben los modelos quitando los outliers (valores atípicos)
# 

