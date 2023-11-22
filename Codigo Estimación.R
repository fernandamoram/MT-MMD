install.packages('pracma')
library('pracma') #función null: calcula nulidad
library('MASS') #función ginv: inversa generalizada

#Generación de datos
n = c(40,50,45,70) #tamaños de muestra de 4 grupos
#n=n*10
X4 = rnorm(n[4],0,1)
X3 = rnorm(n[3],0,1)
X2 = rexp(n[2],4)
B = (runif(n[1])<1/2)
X1 = B*rexp(n[1])+(1-B)*rnorm(n[1],0.1,1)
#Note que los datos no cumplen con la hipotesis nula

#Calculo de matriz de contraste
C = matrix(c(1,-0.5,-0.5,0,0,0,1,-1),ncol=4,byrow=TRUE)
V=null(C)
#C%*%V

#Paso 1: Calculo de las funciones empiricas (sin restricción)
Xtotal=c(X1,X2,X3,X4)

F1_rank=rank(Xtotal)[1:n[1]]
F1=rep(0,sum(n))
F1[F1_rank]=1/n[1]
F1=cumsum(F1)

F2_rank=rank(Xtotal)[(n[1]+1):(n[1]+n[2])]
F2=rep(0,sum(n))
F2[F2_rank]=1/n[2]
F2=cumsum(F2)

F3_rank=rank(Xtotal)[(n[1]+n[2]+1):(n[1]+n[2]+n[3])]
F3=rep(0,sum(n))
F3[F3_rank]=1/n[3]
F3=cumsum(F3)

F4_rank=rank(Xtotal)[(n[1]+n[2]+n[3]+1):(n[1]+n[2]+n[3]+n[4])]
F4=rep(0,sum(n))
F4[F4_rank]=1/n[4]
F4=cumsum(F4)

plot(sort(Xtotal),F1,main='Estimación libre (empírica)')
points(sort(Xtotal),F2,col=2)
points(sort(Xtotal),F3,col=3)
points(sort(Xtotal),F4,col=4)

#Paso 2: Estimación de F tilde (Funciones de distribución sujetas a que
#cumplen con H0: C%*%F tilde=0)
Fvec=matrix(c(F1,F2,F3,F4),byrow=TRUE,nrow=4)
Fvec_proj=V%*%ginv(V)%*%Fvec


plot(sort(Xtotal),Fvec_proj[1,],main='Estimación con restricción')
points(sort(Xtotal),Fvec_proj[2,],col=2)
points(sort(Xtotal),Fvec_proj[3,],col=3)
points(sort(Xtotal),Fvec_proj[4,],col=4)


