#Librerías necesarias
library(pracma)  #Función null: calcula nulidad
library(MASS)    #Función ginv: inversa generalizada
library(ggplot2) #Visualización de gráficos
set.seed(234) #Semilla

#Generación de datos
n = c(40, 50, 45, 70)  #Tamaños de muestra de 4 grupos
X4 = rnorm(n[4], 0, 1) #Muestra de 70 datos que distribuyen normal con media 0 y de desviación estándar 1
X3 = rnorm(n[3], 0, 1) #Muestra de 45 datos que distribuyen normal con media 0 y desviación estándar 1
X2 = rexp(n[2], 4) #Muestra de 50 datos que distribuyen exponencial con parámetro lambda
B = (runif(n[1]) < 1/2) #Muestra de 40 datos booleanos (a partir de una distribución uniforme)
X1 = B*rexp(n[1]) + (1-B)*rnorm(n[1], 0.1, 1) #Muestra de 40 datos a partir de una combinación de distribución
#exponencial con lambda=1 y distribución normal con media 0.1 y desviación estándar 1

#Cálculo de Matriz de Contraste: Se genera a partir de las hipótesis marginales
#La hipótesis gobal se puede escribir como CF = 0, F pertenece a la nulidad de C 
C = matrix(c(1, -0.5, -0.5, 0, 0, 0, 1, -1), ncol = 4, byrow = TRUE) #4 grupos y 2 restricciones (2x4)
V = null(C) #Nulidad de la matriz C, conjunto de vectores que hacen que el producto matriz-vector sea 0.
#Espacio de vectores que satisfacen las restricciones impuestas por la matriz C
R = C%*%V #Hipótesis nula global

#Paso 1: Cálculo de las Funciones Empíricas (sin restricción)
Xtotal = c(X1, X2, X3, X4) #Vector con todos los datos observados

f_empirica = function(X, m) { #Función con entrada de vector con el total de datos y vector de tamaño de muestras
  f_matrix = matrix(0, nrow = length(m), ncol = sum(m)) #Matriz (4xTotaldedatos)
  start_index = 0 #Índice de inicio de cada grupo
  for (i in seq_along(m)) { #Bucle que recorre longitud de n
    F_rank = rank(Xtotal)[start_index:(start_index + m[i])] #Asigna posición ordenada ascendente dentro del Xtotal 
    f_matrix[i, F_rank] = 1/m[i] #Asigna probabilidad a cada dato 
    f_matrix[i, ] = cumsum(f_matrix[i, ]) #Actualiza los datos con la suma acumulada 
    start_index = start_index + m[i] #Actualiza el índice para el próximo grupo
  }
  return(f_matrix) #Matriz que contiene las funciones empíricas acumulativas
}

Fvec = f_empirica(Xtotal, n) 

#Paso 2: Estimación de F tilde (Funciones de distribución sujetas a H_0: C%*%F tilde = 0)
Fvec_proj = V %*% ginv(V) %*% Fvec #Proyección de F (nulidad*inversageneralizadanulidad*Festimada)


#Visualización de Funciones Empíricas sin restricción 
plot1 = ggplot() +
  geom_point(aes(x = sort(Xtotal), y = Fvec[1, ], col = 'Grupo 1', shape = 'Grupo 1'), size = 2.5) +
  geom_point(aes(x = sort(Xtotal), y = Fvec[2, ], col = 'Grupo 2', shape = 'Grupo 2'), size = 2.5) +
  geom_point(aes(x = sort(Xtotal), y = Fvec[3, ], col = 'Grupo 3', shape = 'Grupo 3'), size = 2.5) +
  geom_point(aes(x = sort(Xtotal), y = Fvec[4, ], col = 'Grupo 4', shape = 'Grupo 4'), size = 2.5) +
  scale_color_manual(values = c('#00CD66', '#EE6AA7', '#EE2C2C', '#1874CD')) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  labs(x = 'Valores', y = 'Probabilidad') +
  theme_minimal() +
  theme(legend.box = 'vertical', legend.position = c(0.9, 0.1), legend.justification = 'center') +
  guides(color = guide_legend(title = 'Grupos', ncol = 1), shape = guide_legend(title = 'Grupos', ncol = 1, override.aes = list(size = 3)))


#Visualización de Funciones Empíricas con restricción (Bajo H_0)
plot2 = ggplot() +
  geom_point(aes(x = sort(Xtotal), y = Fvec_proj[1, ], col = 'Grupo 1', shape = 'Grupo 1'), size = 2.5) +
  geom_point(aes(x = sort(Xtotal), y = Fvec_proj[2, ], col = 'Grupo 2', shape = 'Grupo 2'), size = 2.5) +
  geom_point(aes(x = sort(Xtotal), y = Fvec_proj[3, ], col = 'Grupo 3', shape = 'Grupo 3'), size = 2.5) +
  geom_point(aes(x = sort(Xtotal), y = Fvec_proj[4, ], col = 'Grupo 4', shape = 'Grupo 4'), size = 2.5) +
  scale_color_manual(values = c('#00CD66', '#EE6AA7', '#EE2C2C', '#1874CD')) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  labs(x = 'Valores', y = 'Probabilidad') +
  theme_minimal() +
  theme(legend.box = 'vertical', legend.position = c(0.9, 0.1), legend.justification = 'center') +
  guides(color = guide_legend(title = 'Grupos', ncol = 1), shape = guide_legend(title = 'Grupos', ncol = 1, override.aes = list(size = 3)))

#Visualización de ambos gráficos
gridExtra::grid.arrange(plot1, plot2, ncol = 2)

