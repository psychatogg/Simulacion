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

MSE <-  mean((y-pred)^2)
print(MSE)


## 3
pred_lis <- vector("list",441)
rowcol <- as.character(seq(0,2,0.1))
MSE_mat <- matrix(ncol=21,nrow=21)
colnames(MSE_mat) <- rowcol
rownames(MSE_mat) <- rowcol
a <- 1
	for (i in seq(0, 2, by=0.1)) {
		for (j in seq(0, 2, by=0.1)) {
			pred <- sis(n1,n2,n3,v1,v2,v3,p1,p2,p3,i,j)
			pred_lis[[a]] <- pred
			MSE_mat[i*10+1,j*10+1] <- mean((y-pred_lis[[a]])^2)
			a <- a+1
		}
	}


ind_min <- which(MSE_mat == min(MSE_mat), arr.ind = TRUE)

mejor_beta_COG <- rownames(MSE_mat)[ind_min[1]]
mejor_beta_fp<- colnames(MSE_mat)[ind_min[2]]
		
cat("Mejor beta_COG: ", mejor_beta_COG, "\n")
cat("Mejor beta_fp: ", mejor_beta_fp, "\n")


## 5
sis_vec <- function(n1, n2, n3, v1, v2, v3, p1, p2, p3, beta_COG, beta_fp) {
	X <- rbind(c(n1), c(n2), c(n3), c(v1), c(v2), c(v3), c(p1), c(p2), c(p3))
	B <- matrix(c(beta_COG, beta_fp), nrow = 2, ncol = 1)
	COG <- 1.2 * X[1:3,] + 0.5 * X[4:6,]
	INT <- t(B) * COG + beta_fp * X[7:9,]
  y_prime <- 1 / (1 + exp(-INT))
  return(y_prime)
}



pred <- sis(n1,n2,n3,v1,v2,v3,p1,p2,p3,beta_COG,beta_fp)



