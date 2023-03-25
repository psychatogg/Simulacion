
rm(list=ls())
load("E:\\GDrive1\\Uni\\Master\\simulacion\\ejercicios\\ajuste_modelo\\auto-mpg.Rda")




miFit <- function(data,dependiente) {
	formulas <- list()
	modelos <- list()
	preds <- names(data[,-1])
	comb_preds <- list()
	
	## Genera todas las combinaciones de 1 a 5 predictores
	for(i in 1:5) {
		comb_preds[[i]] <- combn(preds,i)
	}
	## Genera Lms con todas las combos de predictores
	for (k in 1:218) { ## 218 es la suma de todas las combinaciones de 1 a 5
		for (i in 1: length(comb_preds)) {
			for (j in 1:length(comb_preds[[i]])) {
				formulas[[k]] <- formula(paste(dependiente, paste(comb_preds[[i]][j], collapse = " + "), sep = " ~ "))
				modelos[[k]] <- lm(formulas[[k]], data)
			}
		}
	}
	return(modelos)
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



preds <- names(data_train[,-1])
comb_preds <- list()
for(i in 1:5) {
	comb_preds[[i]] <- combn(preds,i)
}

	




