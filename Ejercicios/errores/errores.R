library(moments)
library(SuppDists)
library(ggplot2)
d <- rnorm(1000,0,1)
skewness(d)
kurtosis(d)



parms<-JohnsonFit(c(0, 1, -.8, 4), moment="use")
sJohnson(parms)


datos <- rJohnson(1000, parms)
plot(density(datos))

## 1
map <- matrix(data = 0, nrow = 41, ncol = 61)
sk <- seq(-2,2, by = 0.1)
kt <- seq(0,6, by = 0.1)
 for (s in 1:length(sk)) {
 	for (k in 1:length(kt)) {
 		tryCatch(
 			{
 			parms<-JohnsonFit(c(0, 1, sk[s], kt[k]), moment="use")
 			sJohnson(parms)
 			rJohnson(1000,parms)
 			map[s,k] <- 1
 			return(map)
 			},
 			error=function(e) {
 			}
 		)
 	}
 }


colnames(map) <- as.character(kt)
rownames(map) <- as.character(sk)
image(map)

## 2
## Normal
parms <- JohnsonFit(c(0, 1, 0, 3),moment="use")
x <- rJohnson(10000,parms)
plot(density(x))

## Whitelist
comp <- as.data.frame(which(map == 1,arr.ind = T))

## Platicurtica
parms <- JohnsonFit(c(0, 1, sk[21], kt[12]), moment="use")
x <- rJohnson(10000,parms)
plot(density(x))

## Mesocurtica

## No podemos, ya que no tenemos una combinación posible con curtosis = 0

## As positiva
min(comp$row)
comp[comp$row == 9,]
parms <- JohnsonFit(c(0, 1, sk[9], kt[57]), moment="use")
x <- rJohnson(10000,parms)
plot(density(x))

## As negativa
max(comp$row)
comp[comp$row == 33,]
parms <- JohnsonFit(c(0, 1, sk[33], kt[61]), moment="use")
x <- rJohnson(10000,parms)
plot(density(x))

## 3

## Uniforme

set.seed(1)

N=1000 # tamano de la poblacion
n = 25 # tamano de las muestras
k = 25000

# creamos una poblacion de N numeros aleatorios con media 5 y sd =2
poblacion <- runif(N, 0, 1)

# comprobamos media y sd porque al ser aleatorios no tienen que ser exactamente 5 y 2
mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)

# extraigo k muestras
# no es el codigo mas eficiente pero al menos reservo la memoria al principio
means <- vector(length=k)

for (i in 1:k){
	muestra <- sample(poblacion, n)
	means[i] <- mean(muestra)
}

# dibujamos esa distribucion de medias
# plot(density(means)) mas bonito pero menos preciso
hist(means)


# Deben coincidir las afirmaciones teoricas
m.teo <- mu.pob
sd.teo <- sd.pob/sqrt(n)

m.emp <- mean(means)
sd.emp <- sd(means)

sprintf("Media teoría: %.2f -- Media empírica : %.2f", m.teo, m.emp)

## [1] "Media teoría: 4.98 -- Media empírica : 4.98"
sprintf("Sd teoría: %.2f -- Sd empírica : %.2f", sd.teo, sd.emp)





## Chi sq


set.seed(1)

N=1000 # tamano de la poblacion
n = 25 # tamano de las muestras
k = 25000

# creamos una poblacion de N numeros aleatorios con media 5 y sd =2
poblacion <- rchisq(N, 3)

# comprobamos media y sd porque al ser aleatorios no tienen que ser exactamente 5 y 2
mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)

# extraigo k muestras
# no es el codigo mas eficiente pero al menos reservo la memoria al principio
means <- vector(length=k)

for (i in 1:k){
	muestra <- sample(poblacion, n)
	means[i] <- mean(muestra)
}

# dibujamos esa distribucion de medias
# plot(density(means)) mas bonito pero menos preciso
hist(means)


# Deben coincidir las afirmaciones teoricas
m.teo <- mu.pob
sd.teo <- sd.pob/sqrt(n)

m.emp <- mean(means)
sd.emp <- sd(means)

sprintf("Media teoría: %.2f -- Media empírica : %.2f", m.teo, m.emp)

## [1] "Media teoría: 4.98 -- Media empírica : 4.98"
sprintf("Sd teoría: %.2f -- Sd empírica : %.2f", sd.teo, sd.emp)



## 4
## Pequeña
set.seed(1)

N=1000 # tamano de la poblacion
n = 5 # tamano de las muestras
k = 25

# creamos una poblacion de N numeros aleatorios con media 5 y sd =2
poblacion <- rbinom(N, 1, 0.3)

# comprobamos media y sd porque al ser aleatorios no tienen que ser exactamente 5 y 2
mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)

# extraigo k muestras
# no es el codigo mas eficiente pero al menos reservo la memoria al principio
means <- vector(length=k)

for (i in 1:k){
	muestra <- sample(poblacion, n)
	means[i] <- mean(muestra)
}

# dibujamos esa distribucion de medias
# plot(density(means)) mas bonito pero menos preciso
hist(means)


# Deben coincidir las afirmaciones teoricas
m.teo <- mu.pob
sd.teo <- sd.pob/sqrt(n)

m.emp <- mean(means)
sd.emp <- sd(means)

sprintf("Media teoría: %.2f -- Media empírica : %.2f", m.teo, m.emp)

## [1] "Media teoría: 4.98 -- Media empírica : 4.98"
sprintf("Sd teoría: %.2f -- Sd empírica : %.2f", sd.teo, sd.emp)



## Grande
set.seed(1)

N=1000 # tamano de la poblacion
n = 25 # tamano de las muestras
k = 25

# creamos una poblacion de N numeros aleatorios con media 5 y sd =2
poblacion <- rbinom(N, 1, 0.3)

# comprobamos media y sd porque al ser aleatorios no tienen que ser exactamente 5 y 2
mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)

# extraigo k muestras
# no es el codigo mas eficiente pero al menos reservo la memoria al principio
means <- vector(length=k)

for (i in 1:k){
	muestra <- sample(poblacion, n)
	means[i] <- mean(muestra)
}

# dibujamos esa distribucion de medias
# plot(density(means)) mas bonito pero menos preciso
hist(means)


# Deben coincidir las afirmaciones teoricas
m.teo <- mu.pob
sd.teo <- sd.pob/sqrt(n)

m.emp <- mean(means)
sd.emp <- sd(means)

sprintf("Media teoría: %.2f -- Media empírica : %.2f", m.teo, m.emp)

## [1] "Media teoría: 4.98 -- Media empírica : 4.98"
sprintf("Sd teoría: %.2f -- Sd empírica : %.2f", sd.teo, sd.emp)
















