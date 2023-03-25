
rm(list=ls())
load("E:\\GDrive1\\Uni\\Master\\simulacion\\ejercicios\\ajuste_modelo\\auto-mpg.Rda")


preds <- names(data_test[,-1])

comb_preds <- combn(preds,5)

miFit <- function(data,dependiente) {
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

miFit(data_train,"mpg")



####### PRUEBAS ######
formulas <- formula(paste("mpg", paste(comb_preds[,1], collapse = " + "), sep = " ~ "))
																					
modelo <- lm(formulas,data_test)




	




prueba <- lm(data_test$mpg~data_test$cylinders)
p.sum <- summary(prueba)
