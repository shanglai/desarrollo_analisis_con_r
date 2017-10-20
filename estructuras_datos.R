
#Estructuras de datos

#Datos básicos
'A'
'Big Data'
123
123.435
as.Date('2017-10-10')

#Variables
var1 <- 'Big Data'
var1

var2 <- 123
var2
var2 * 5


#Vectores

c(1,2,3)
c('a','b')
c('1','2','3')

v1 <- c(1,2,3)
v2 <- c(4,5,6)
c(v1,v2)
v1+v2
v1*v2

v3 <- c('a','b','c')

c(v1,v3)

v4 <- seq(1,40,4)
v4
v4[1]
v4[1:5]
v4[c(1,2,6,7)]
v4[c(1,20)]

#Operaciones básicas
s1 <- seq(1,20)
s1
rev(s1)
s2 <- seq(0,1,.01)
s2
s3 <- seq(10,1,-1)
s3
rep(v1,5)

#Factores

dias <- c('Lu','Ma','Mi','Jue','Vi','Sa','Do')
dias
dias.f <- factor(dias)
dias.f
dias.f[1] <- 'Do'
dias.f
dias.f[2] <- 'Xx'


#Matrices

m1 <- matrix(seq(1,16),ncol=4)
m1
t(m1)
m1 %*% t(m1)
solve(matrix(c(1,2,0,2,1,1,0,1,0),ncol=3))


#Data Frames

id <- seq(1,10)
nombre <- c('Jaime','Pablo','Monica','Ximena','Saul','Maria','Octavio','Ruben','Sandra','Arely')
saldo <- runif(10,10.0,1500.5)
dias <- c('Lu','Ma','Mi','Ju','Vi','Sa','Do')
dia.sem <- factor(c('Ma','Ju','Ju','Sa'),levels=dias)
dia.sem
dia.sem <- factor(sample(dias,10,replace = T),levels=c('Lu','Ma','Mi','Ju','Vi','Sa','Do'))

clientes <- data.frame(id,nombre,saldo,dia.sem,asegurado= 'S')
clientes

str(clientes)
summary(clientes)
head(clientes)
tail(clientes)
clientes[1:2,]
clientes[,c(1,2,4)]

which(clientes$saldo>500)
clientes[which(clientes$saldo>500),]
clientes.mayor500 <- clientes[which(clientes$saldo>500),]

with(clientes.mayor500,order(id))
clientes.mayor500[with(clientes.mayor500,order(id)),]
# y con día de la semana?
clientes.mayor500[with(clientes.mayor500,order(dia.sem)),]
