## 1
datos <- read.csv("E:\\GDrive1\\Uni\\Master\\simulacion\\ejercicios\\sistemas_estacionarios\\datos.csv")
n1 <- datos$n1
n2 <- datos$n2
n3 <- datos$n3
v1 <- datos$v1
v2 <- datos$v2
v3 <- datos$v3
p1 <- datos$p1
p2 <- datos$p2
p3 <- datos$p3
y <- datos$y
beta_COG <- 1
beta_fp <- 1

sis <- function(n1,n2,n3,v1,v2,v3,p1,p2,p3,beta_COG,beta_fp) {
	fn <- 0.5*n1+0.3*n2+0.2*n3
	fv <- 0.5*v1+0.3*v2+0.2*v3
	fp <- 0.5*p1+0.3*p2+0.2*p3
	COG <- 1.2*fn+0.5*fv
	INT <- beta_COG*COG+beta_fp*fp
	y_prime <- 1/(1+exp(-INT))
	return(y_prime)
}
pred <- sis(n1,n2,n3,v1,v2,v3,p1,p2,p3,beta_COG,beta_fp)


## 2
n <- length(datos$y)
MSE <- 1/n * (sum(y-pred)^2)
print(MSE)


## 3




