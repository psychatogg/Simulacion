
library(neuralnet)
source("E:\\GDrive1\\Uni\\Master\\simulacion\\ejercicios\\battleship\\game\\funciones.R")
##############Preprocess#####################

flota <- list()

flota[[1]] <- creaBarco("A", c(1,1,
															 2,1,
															 3,1))

flota[[2]] <- creaBarco("B", c(5,1,
															 5,2,
															 5,3))

flota[[3]] <- creaBarco("C", c(6,3,
															 7,3,
															 8,3))


tablero <- nuevoTablero(10, 10, flota)

jugadas <- list()
jugadas[[1]] <- c(4,1)
jugadas[[2]] <- c(6,3)
jugadas[[3]] <- c(5,1)

alcances_pred <- c(0,1,1)

## Extrae posiciones de flota de cada eje


posiciones_y<- c()
c = 1
for(a in 1:length(flota)){
		for(n in 1:nrow(flota[[a]]$pos)){
			posiciones_y[c] <- flota[[a]]$pos[n,1]
			c = c + 1
		}
	}







training_data <- data.frame(,alcances_pred)



nn <- neuralnet(alcances_pred ~ flota+jugadas,
								hidden = 5,
								data = training_data,
								algorithm = "backprop",
								learningrate = 0.05)
