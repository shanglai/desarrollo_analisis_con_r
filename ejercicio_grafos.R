
# Ejercicios - Redes (grafos)
# David López
# @5h4n6

# Instalo paquetes
install.packages('igraph')
install.packages('igraphdata')

# Cargo paquetes
library(igraph)
library(igraphdata)

# data() sirve para ver todos los datasets disponibles
data()
data(package="igraphdata")

# Cargo los datos de karate
# Datos de una clase de Karate de universidad, la cual se separó en dos grupos
# Este dataset representa las relaciones entre los miembros de estos dos grupos
data(karate)

# No es un dataset ordinario (tabla), sino que es un objeto de tipo grafo.
# Revisemos todo lo que tiene Karate.
karate

# Un grafo representa entidades (nodos) y su relación entre ellos (aristas).
# La relación puede ser dirigida (ej. A le debe $ a B) o no dirigida (ej. A y B se conocen)

# Una de las formas de entender un grafo es mediante una matriz, llamada matriz de adyacencia
get.adjacency(karate)[1:20,1:20]

# Ahora, cómo se ve
plot(karate)




