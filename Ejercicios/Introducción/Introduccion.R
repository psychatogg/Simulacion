## Integrantes del grupo: Juliana Quirós, Alberto (ajuliana@ucm.es); 




## 11.1

## 1
library(psych)

## 2
ej <- read.csv("ejemplo.csv", sep=";")

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
	return(sum(a,b))
}

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



## 11.3

## 1
suma_10 <- 0
for(i in 1:10) {
	suma_10 <- suma_10 + i 
}


## 2
v <- c(0,0,0,0,0)

for(i in 1:length(v)){
	if(i %% 2 != 0) {
		v[i] <- 1
	}
}

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