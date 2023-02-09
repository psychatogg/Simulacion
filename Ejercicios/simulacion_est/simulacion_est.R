
## 1. Muestra que la función runif produce una distribución uniforme que coincide con la teórica
## 2. Muestra que rt genera distribuciones t que coinciden con la teórica
## 3. Muestra que rf genera distribuciones F que coinciden con la teórica
## 4. Muestra que rchisq genera distribuciones chisq que coinciden con la teórica
## 5. Muestra como el valor empírico y el teórico se aproximan al aumentar el n en el caso de tirar 5 dados
##	y que salga el 1.

set.seed(1)

## 1 
x <- seq(-10, 10, .1)
un_generador <- runif (10000,-10,10)


uniform <- function (x) {
	dens_vec <- c()
	for(i in 1:length(x)) {
		dens_vec[i] <- 1/(max(x)-min(x))
	}
	return(dens_vec)
}
un_eq <- uniform(x)
	
d <- density(un_generador) 
plot(d) # plot d
lines(un_eq~x, col="green")





## 5
dbinom(1,5,(1/6))




## 
