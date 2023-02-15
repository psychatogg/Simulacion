
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

