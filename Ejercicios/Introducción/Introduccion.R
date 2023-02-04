## Integrantes del grupo: Juliana Quirós, Alberto (ajuliana@ucm.es); 




## 11.1

## 1
library(psych)

## 2
ej <- read.csv("E:/GDrive1/Uni/Master/Simulacion/Ejercicios/Introducción/ejemplo.csv", sep=";")

## 3
mean(ej$X)
mean(ej$Y)
var(ej$X)
var(ej$Y)

## 4
sqrt_x <- sapply(ej$X, function (x) sqrt(x))
ej <- cbind(ej,sqrt_x)


## 11.2
## 1
suma1 <- function(a,b) {
	return(a+b)
}
suma1(4,9)

## 2
suma3 <- function(x1,x2,x3) {
	if(missing(x1) & missing(x2) & missing(x3)) {
		return(0)
	}
		
	else if (missing(x1)) {
		x1 <- 0
	}
	else if(missing(x2)) {
		x2 <- 0
	}
	else if(missing(x3)) {
		x3 <- 0
	} 
	s <- sum(x1,x2,x3)
	return(s)
}
suma3(2,3)
suma3(7,3,1)

## 11.3

## 1
suma_10 <- 0
for(i in 1:10) {
	suma_10 <- suma_10 + i 
}
suma_10

## 2
v <- c(0,0,0,0,0)

for(i in 1:length(v)){
	if(i %% 2 != 0) {
		v[i] <- 1
	}
}
v

## 3

aj <- matrix(nrow = 10, ncol = 10)

for(f in 1:10){
	for (c in 1:10) {
		if((f %% 2 != 0) & (c %% 2 != 0 )) {
			aj[f,c] <- 0
		} else if ((f %% 2 != 0) & (c %% 2 == 0)) {
			aj[f,c] <- 1
			
		} else if ((f %% 2 == 0) & (c %% 2 != 0 )) {
			aj[f,c] <- 1
		} else {
			aj[f,c] <- 0
		}
		
	}
}
aj
	
## 4

crea_aj <- function (d1,d2) {
	aj <- matrix(nrow = d1, ncol = d2)
	for(f in 1:nrow(aj)){
		for (c in 1:ncol(aj)) {
			if((f %% 2 != 0) & (c %% 2 != 0 )) {
				aj[f,c] <- 0
			} else if ((f %% 2 != 0) & (c %% 2 == 0)) {
				aj[f,c] <- 1
				
			} else if ((f %% 2 == 0) & (c %% 2 != 0 )) {
				aj[f,c] <- 1
			} else {
				aj[f,c] <- 0
			}
		}
	}
	return(aj)
}

crea_aj(4,4)

crea_aj(5,3)


## 11.4

## 1

medes <- function (df) {
	for(i in 1:ncol(df)) {
		print(mean(df[,i]))
		print(sd(df[,i]))
	}
}


ej <- read.csv("E:/GDrive1/Uni/Master/Simulacion/Ejercicios/Introducción/ejemplo.csv", sep=";")
medes(ej)

## 2

prod_intern_cos <- function (v1,v2) {
	if (length(v1) == length(v2)) {
	res <- 0
	for(i in 1:length(v1)) {
		mult <- v1[i] * v2[i]
		res <- res + mult
	}
	return (cos(res))
	} else {
		warning ("Error: Los vectores han de tener mismas dimensiones ")
	}
}
v1 <- c(4,3,4)
v2 <- c(3,4,4)
prod_intern_cos(v1,v2)


v1 <- c(2,3)
v2 <- c(3,4,5)
prod_intern_cos(v1,v2)

## 3

df_medes <- function(df) {
	df_res <- data.frame(Variable = names(df))
	m_conj <- c()
	desv_conj <- c()
	for(i in 1:ncol(df)) {
		m <- mean(df[,i])
		desv <- sd(df[,i])
		m_conj <- append(m_conj,m)
		desv_conj <- append(desv_conj,desv)
	}
	df_res <- cbind(df_res,m_conj)
	df_res <- cbind(df_res,desv_conj)
	return(df_res)
}
	
ej <- read.csv("E:/GDrive1/Uni/Master/Simulacion/Ejercicios/Introducción/ejemplo.csv", sep=";")
df_medes(ej)


