library(pwr)
library(Hmisc)
library(ggpubr)
library(dplyr)
# Poder estadistico -------------------------------------------------------

# Alfa nivel de significacion corresponde a la probabilidad de cometer un error de Tipo I
# Beta probabilidad de cometer error de Tipo II

# Reducir beta, aumenta alfa.

# En la practica resulta mas interesante estudiar la probabilidad de no cometer errores de Tipo II
# De lo anterior se desprende el poder estadistico que se define como la probabilidad de correctamente rechazar H0 cuando es falsa.

# Tamaño del efecto -> Cuantificacion de la diferencia entre dos grupos. Diferencia entre el valor observado y el valor nulo.

# El poder de la prueba aumenta siempre que aumenta el tamaño del efecto.
# Cuando disminuye el tamaño del efecto el poder se aproxima al nivel de significacion.
# Usar una muestra mas grande aumenta el poder de la prueba para cualquier tamaño del efecto.

# Funcion en R para calcular el poder estadistico.
#power.t.test ( n = 6 ,delta = efecto ,sd = 1 ,sig.level = 0.01 ,type = "one. sample ", alternative = " two. sided ") $ power

# Tamaño del efecto.

# D de cohen: medida estandar ampliamente empleada para el tamaño del efecto de 
# la prueba t de student,

# - d = 0,2 -> efecto pequeño
# - d = 0,5 -> efecto mediano
# - d = 0,8 -> efecto grande

# Se calcula como:

# Para la prueba t de una muestra
# d = (media-valor nulo)/sd

# Para la prueba t de diferencia de dos medias
# - Muestras > 50

# d = (media_1-media_2)/desviacion estandar agrupada

# - Muestras < 50

# d = ((media_1 - media_2)/(desviacion estandar agrupada))/((n_1+n_2-3)/(n_1+n_2-2,25))

# * desviacion estandar agrupada : sqrt((sum(x-media_2)**2 + sum(x-x_2)**2)/(n_1+n_2-2))

# Calculo teorico del poder:

# Superponer curvas de hipotesis nula y alternativa.


# Inferencia con proporciones muestrales ----------------------------------

#Metodo de wald: 

# Se deben cumplir las siguientes condiciones para decir que el estimador se
# distribuye de manera cercana a la normal:

# 1- Las observaciones de la muestra son independientes
# 2- Se cumple la condicion de exito fracaso, que establece que se espera observar
#    al menos 10 observaciones correspondientes a exito y al menos 10, correspondientes
#    a fracasos. Matematicamente, np>=10 y n(1-p)>=10.

#Metodo de wald para una proporcion:

# Pasos:
# - Comprobar condiciones
# - Calcular error estandar
# - Armar intervalo: p+-<z_critico*SE

# Podemos usar el modelo normal tambien, para ello se deben verificar ambas condiciones
# donde la segunda se verifica usando el valor nulo.

# - Error estandar: sqrt((valor_nulo(1-valor_nulo))/n)
# - Estimador Z: (estimador-valor_nulo)/SE
# - valor p: pnorm(z,lower.tail=FALSE)

# Como podemos ver existen dos formas de resolver esto, mediante intervalo de confianza
# y tambien a través de una prueba de hipotesis.


#Metodo de wald para dos proporciones:

#Lo utilizamos para estudiar la diferencia entre las proporciones

# Estimador: p = p_1 - p_2 

# Se deben verificar las condiciones para cada muestra por separado.

# Error estandar: sqrt((p_1*(1-p_1)/n_1)-(p_2*(1-p_2)/n_2))

#¿Cuando aplicariamos esta prueba?
#EJ:A modo de ejemplo, supongamos que la Facultad de Ingeniería de una prestigiosa
#universidad desea determinar si la tasa de reprobación de estudiantes que 
#rinden la asignatura de programación por primera vezes igual para hombres y mujeres. 

#p_1 y p_2 se calculan como: casos favorables / casos totales

# Pasos 
# - Verificar condiciones
# - Calcular error estandar
# - z_critico = en base al nivel de significancia qnorm(alfa/2,lower.tail=False)
# - Armar intervalo = (p_1-p_2)+-z_critico*SE

# En caso de querer hacer prueba de hipotesis

# Pasos
# - Verificar condiciones, donde la segunda condicion cuando la hipotesis nula
# supone que no hay diferencia entre las proporciones, la verificación de la 
# condición de éxito-fracaso y la estimación del error estándar se realizan 
# usando para ello la proporción agrupada

# p = ((p_1*n_1)+(p_2*n_2))/(n_1+n_2)

# Entonces p*n>=10 y n(1-p)>=10 para ambas proporciones

# - Error estandar: sqrt((p*(1-p))/n_1  +  (p*(1-p))/n_2)

# - Z: (estimador puntual - valor nulo) / SE

# - valor p: pnorm(Z,lower.tail=FALSE)

# - Verificar condiciones, donde la segunda condicion cuando contrastamos 
# hipotesis para la diferencia entre dos proporciones con un valor distinto
# de 0. La comprobacion de exito fracaso se realiza de manera independiente 
# para ambas muestras.

# - Error estandar: sqrt((p_1*(1-p_1)/n_1)-(p_2*(1-p_2)/n_2))

# - Z: (estimador puntual - valor nulo)/SE

# - valor p: pnorm(z,lower.tail)

# Metodo de wilson

# Este metodo opera del mismo modo que el de wald, aunque las formulas empleadas
# para estimar la proporcion en la muestra y el error estandar son diferentes.
# Es un metodo mas robusto y no realiza tantas simplificaciones matematicas.

# En R se realiza la prueba con: prop.test(x,n,p,alternative,conf.level)
# - x: cantidad de exitos en la muestra
# - n: tamaño de la muestra
# - p: valor nulo
# - alternative: two.sided, less, greater
# - conf.level: nivel de confianza.

# Poder estadistico
# power.prop.test(n,p1,p2,sig.level,power,alternativ)
# Recibe 4 argumentos y al quinto se le debe asignar valo NULL.

# pwr.p.test(h, n, sig.level, power, alternative): para pruebas con una única proporción
# pwr.2p.test(h, n, sig.level, power, alternative): para pruebas con dos proporciones donde
#ambas muestras son de igual tamaño
# pwr.2p2n.test(h, n1, n2, sig.level, power, alternative): para pruebas con dos proporciones
#y muestras de diferente tamaño

# h: tamaño del efecto que se calcula con ES.h(p1, p2)



























