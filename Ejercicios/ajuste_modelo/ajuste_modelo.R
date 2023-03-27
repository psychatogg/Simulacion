
rm(list=ls())
load("E:\\GDrive1\\Uni\\Master\\simulacion\\ejercicios\\ajuste_modelo\\auto-mpg.Rda")

## Codificación
data_train$horsepower <- as.integer(data_train$horsepower)
data_test$horsepower <- as.integer(data_test$horsepower)

miFit <- function(data,dependiente) {
	formulas <- list()
	modelos <- list()
	formulas_int <- list()
	modelos_int <- list()
	preds <- names(data[,2:7]) ## Obviamos el nombre del coche por colinealidad
	comb_preds <- list()
	
	## Genera todas las combinaciones de 1 a 5 predictores
	for(i in 1:5) {
		comb_preds[[i]] <- combn(preds,i)
	}
	
	
	## Inicializa las listas
	for (i in 1: length(comb_preds)) {
		formulas[[i]] <- vector("list", ncol(comb_preds[[i]]))
		modelos[[i]] <- vector("list", ncol(comb_preds[[i]]))
		formulas_int[[i]] <- vector("list", ncol(comb_preds[[i]]))
		modelos_int[[i]] <- vector("list", ncol(comb_preds[[i]]))
	}
	## Genera Lms con todas las combos de predictores
		for (i in 1: length(comb_preds)) { 
			for (j in 1:ncol(comb_preds[[i]])) { 
				formulas[[i]][[j]] <- formula(paste(dependiente, paste(comb_preds[[i]][,j], collapse = " + "), sep = " ~ "))
				modelos[[i]][[j]] <- lm(formulas[[i]][[j]], data)
			}
		}
	## Genera Lms de todas las combos con todas las interacciones
	for (i in 1: length(comb_preds)) { 
		for (j in 1:ncol(comb_preds[[i]])) { 
			formulas_int[[i]][[j]] <- formula(paste(dependiente, "~",
																							paste(comb_preds[[i]][,j], collapse = " + "),
																							paste0(sep="^4")))
																				
																				
			modelos_int[[i]][[j]] <- lm(formulas_int[[i]][[j]], data)
		}
	}
	
	return(modelos_int)
}

miFit(data_train,"mpg")



####### PRUEBAS #####

########### 5 PREDICTORES
miFit <- function(data,dependiente) {
	preds <- names(data[,-1])
	comb_preds <- combn(preds,5)
	formulas <- list()
	MSEs <- rep(NA, ncol(comb_preds))
	modelos <- list()
	
	
	## 5 predictores
	for (i in 1:ncol(comb_preds)) {
		formulas[[i]] <- formula(paste(dependiente, paste(comb_preds[,i], collapse = " + "), sep = " ~ "))
		modelos[[i]] <- lm(formulas[[i]], data)
	}
	return(modelos)
}
##################################

formulas <- list()
modelos <- list()
formulas_int <- list()
modelos_int <- list()
preds <- names(data_train[,2:7]) ## Obviamos el nombre del coche por colinealidad
comb_preds <- list()
dependiente<-"mpg"
## Genera todas las combinaciones de 1 a 5 predictores
for(i in 1:5) {
	comb_preds[[i]] <- combn(preds,i)
}


## Inicializa las listas
for (i in 1: length(comb_preds)) {
	formulas[[i]] <- vector("list", ncol(comb_preds[[i]]))
	modelos[[i]] <- vector("list", ncol(comb_preds[[i]]))
	formulas_int[[i]] <- vector("list", ncol(comb_preds[[i]]))
	modelos_int[[i]] <- vector("list", ncol(comb_preds[[i]]))
}
## Genera Lms con todas las combos de predictores
for (i in 1: length(comb_preds)) { 
	for (j in 1:ncol(comb_preds[[i]])) { 
		formulas[[i]][[j]] <- formula(paste(dependiente, paste(comb_preds[[i]][,j], collapse = " + "), sep = " ~ "))
		modelos[[i]][[j]] <- lm(formulas[[i]][[j]], data_train)
	}
}
## Genera Lms de todas las combos con todas las interacciones
for (i in 1: length(comb_preds)) { 
	for (j in 1:ncol(comb_preds[[i]])) { 
		formulas_int[[i]][[j]] <- formula(paste(dependiente, "~",
																						paste(comb_preds[[i]][,j], collapse = " + "),
																						paste0(sep="^4")))
		
		
		modelos_int[[i]][[j]] <- lm(formulas_int[[i]][[j]], data_train)
	}
}