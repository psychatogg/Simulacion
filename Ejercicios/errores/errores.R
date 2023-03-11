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
x <- rJohnson(1000,parms)
plot(density(x))

## Whitelist
comp <- which(map == 1,arr.ind = T)

## Platicurtica
parms <- JohnsonFit(c(0, 1, sk[21], kt[12]), moment="use")
x <- rJohnson(1000,parms)
plot(density(x))





