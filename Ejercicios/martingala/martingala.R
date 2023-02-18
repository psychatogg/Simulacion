## 1

set.seed(1)

martingala <- function (bolsa, apuesta, limite, prob) {
	apuesta_actual <- apuesta
	while (apuesta_actual < bolsa & apuesta_actual < limite) {
		bolsa <- bolsa- apuesta_actual
		## print(apuesta_actual)
		resul <- rbinom(1,1,prob)
		## print(resul)
		if(resul == 0 ) { ## perder
			apuesta_actual <- apuesta_actual * 2
			## print(apuesta_actual)
			## print(bolsa)
		} else { ## ganar
			bolsa <- bolsa + apuesta_actual *2 
			apuesta_actual <- apuesta
			## print(apuesta_actual)
			## print(bolsa)
		}
	}
	return (bolsa)
}




martingala(bolsa = 100, apuesta = 10, limite = 500, prob = 18/37)






## 2
set.seed(1)
n_simulaciones <- 1000

meansim_vec <- c()
prob_vec <- c()
errmax_vec <- c()
sumprob <- 0.2
for(i in 1:41){
	sim <- replicate(n_simulaciones,martingala(bolsa = 100, apuesta = 10, limite = 500, prob = sumprob))
	## Medias
	meansim <- mean(sim)
	meansim_vec <- append(meansim_vec,meansim)
	## ICs
	errmax <- 1.95*(sd(sim)/sqrt(1000))
	errmax_vec <- append(errmax_vec,errmax)
	## probabilidades
	prob_vec <- append(prob_vec,sumprob)
	sumprob <- sumprob + 0.01
}


require(ggplot2)
df_probmean <- data.frame(prob_vec,meansim_vec)
probmean_graph <- ggplot(df_probmean,aes(prob_vec,meansim_vec)) + 
	geom_point() + 
	geom_errorbar(ymin = meansim_vec-errmax_vec, ymax = meansim_vec+errmax_vec)
probmean_graph <- probmean_graph + expand_limits(y = c(0,800))
print(probmean_graph)


## 3

set.seed(1)
n_simulaciones <- 1000

meansim_vec <- c()
bolsa_vec <- c()
errmax_vec <- c()
sumbolsa <- 10
for(i in 1:39){
	sim <- replicate(n_simulaciones,martingala(bolsa = sumbolsa, apuesta = 10, limite = 500, prob = 18/37))
	## Medias
	meansim <- mean(sim)
	meansim_vec <- append(meansim_vec,meansim)
	## ICs
	errmax <- 1.95*(sd(sim)/sqrt(1000))
	errmax_vec <- append(errmax_vec,errmax)
	## probabilidades
	bolsa_vec <- append(bolsa_vec,sumbolsa)
	sumbolsa <- sumbolsa + 5
}


require(ggplot2)
df_bolsamean <- data.frame(bolsa_vec,meansim_vec)
bolsamean_graph <- ggplot(df_bolsamean,aes(bolsa_vec,meansim_vec)) + 
	geom_point() + 
	geom_errorbar(ymin = meansim_vec-errmax_vec, ymax = meansim_vec+errmax_vec)
bolsamean_graph <- bolsamean_graph + expand_limits(y = c(0,180))
print(bolsamean_graph)

## 4 
set.seed(1)
n_simulaciones <- 1000

meansim_vec <- c()
limit_vec <- c()
errmax_vec <- c()
sumlimit <- 50
for(i in 1:46){
	sim <- replicate(n_simulaciones,martingala(bolsa = 100, apuesta = 10, limite = sumlimit, prob = 18/37))
	## Medias
	meansim <- mean(sim)
	meansim_vec <- append(meansim_vec,meansim)
	## ICs
	errmax <- 1.95*(sd(sim)/sqrt(1000))
	errmax_vec <- append(errmax_vec,errmax)
	## probabilidades
	limit_vec <- append(limit_vec,sumlimit)
	sumlimit <- sumlimit + 10
}


require(ggplot2)
df_limitmean <- data.frame(limit_vec,meansim_vec)
limitmean_graph <- ggplot(df_limitmean,aes(limit_vec,meansim_vec)) + 
	geom_point() + 
	geom_errorbar(ymin = meansim_vec-errmax_vec, ymax = meansim_vec+errmax_vec)
limitmean_graph <- limitmean_graph + expand_limits(y = c(0,200))
print(limitmean_graph)