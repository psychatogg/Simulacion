
## 1. Muestra que la función runif produce una distribución uniforme que coincide con la teórica
## 2. Muestra que rt genera distribuciones t que coinciden con la teórica
## 3. Muestra que rf genera distribuciones F que coinciden con la teórica
## 4. Muestra que rchisq genera distribuciones chisq que coinciden con la teórica
## 5. Muestra como el valor empírico y el teórico se aproximan al aumentar el n en el caso de tirar 5 dados
## y que salga el 1.
## 6. Muestre el teorema central del límite con la distribución F. Como F tiene gl1 y gl2, para no hacer
## inﬁnidad de gráﬁcos, haga la demostración para gl1 = gl2, es decir, siempre los mismos grados de
## libertad en el numerador y el denominador.
## 7. Muestre el teorema central del límite con la distribución χ2.
set.seed(1)

## 1 
x <- seq(-10, 10, .1)
un_generador <- runif (1000000,-10,10)


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

## 2
x <- seq(-10, 10, .1)
t_generador <- rt (10000,99)


t <- function(x, df){
	n = ((gamma((df+1)/2))/(sqrt(df*pi)*gamma(df/2))) * (1+(x^2)/df)^((-df+1)/2)
}

t_eq <- t(x,99)

d <- density(t_generador) 
plot(d) # plot d
lines(t_eq~x, col="green")






## 5
x <- vector()
for(n in 1:4000) x[n] <- mean(rbinom(n,5,1/6))
plot(x, type="l", xlab="número de repeticiones (n)", xlim = c(3000,4000))




## 6

