# Podemos decir que un estadístico corresponde a un estimador puntual
# de un parámetro. El estimador tiende a mejorar a medida que aumentamos el tamaño de la muestra, por
# efecto de la ley de los grandes números.

#Error estandar
# El error estándar, denotado usualmente por SE, corresponde a la desviación
# estándar de la distribución de un estimador muestral de un parámetro.

#Intervalo de confianza -> donde podria encontrarse un determinado estimador
# media +- valor_z*error_estandar
# "se tiene x% de certeza de que el parámetro de la población
# se encuentra entre..."


# Pruebas de Hipotesis ----------------------------------------------------

#El que no se logre rechazar H0 no significa aceptarla como verdadera o como correcta sin más

#Error tipo 1: Rechazar H0 cuando es verdadera
#Error tipo 2: Rechazar Ha cuando es verdadera 

# El intervalo de confianza es de mucha ayuda para decidir si rechazar o no H0. No obstante, no aporta
# información directa acerca de cuán fuerte es la evidencia para la decisión tomada.

# Es posible obtener probabilidades casi exactas para cualquier percentil haciendo uso del valor p, que se define como
# "la probabilidad de observar datos al menos tan favorables como la muestra actual para
# la hipótesis alternativa, si la hipótesis nula es verdadera"

# Estadistico de prueba : z = (estimador puntual-valor nulo)/error estandar

# Se calcula el valor p
# p_1 <- pnorm (Z , lower.tail = TRUE )

# El valor p se puede comparar directamente con el nivel de significación
# alfa, y si p es menor que el nivel de significación se considera evidencia suficiente para rechazar la hipótesis
# nula en favor de la hipótesis alternativa.

# Valores mas pequeños de alfa aumentan la probabilidad de cometer un error de tipo 2.

# Pruebas de Inferencia con medias ----------------------------------------

# Prueba Z
# Se deben verificar 3 condiciones para poder usarla 
# 1- La muestra debe tener al menos 30 observaciones. Si la muestra tiene menos de 30 observaciones, se
#    debe conocer la varianza de la población
# 2- Las observaciones deben ser independientes, es decir que la elección de una observación para la muestra
#    no influye en la selección de las otras.
# 3- La población de donde se obtuvo la muestra sigue aproximadamente una distribución normal. (Grafico q-q)

# Esta prueba resulta adecuada si queremos asegurar o descartar que la media de la población tiene un
# cierto valor hipotético

# Pasos: 
# 1 - Verificar condiciones
# 2 - Calcular estimador Z
# 3 - Calcular valor p
# 4 - Comparar con alfa
# 5 - Concluir

#Se puede hacer la prueba de forma automatica
# z.test ( media , mu = valor_nulo , alternative = "two. sided ",
#            43 stdev = desv_est , conf.level = 1 - alfa )

#Prueba t de student para una muestra.

# En la práctica, rara vez podemos conocer la desviación estándar de la población y a menudo nos encontraremos
# con muestras pequeñas, por lo que la prueba Z no es muy utilizada.

# Para poder usar la prueba se deben verificar 2 condiciones
# 1- Las observaciones son independientes entre sí
# 2- Las observaciones provienen de una distribución cercana a la normal

# Pasos

# 1- Verificar condiciones

# 2- Estimador T
# T = (media-valor nulo)/error estandar

# 3- Intervalo
# t critico = qt( alfa , df = grados_libertad , lower . tail = FALSE )
# media +- tcritico*error estandar

# 4- Valor p
# pt(t, df = grados_libertad , lower . tail = TRUE )


# Funcion que ejecuta automaticamente el test.
# t.test(x,alternative, mu, conf.level)


# Prueba t para dos muestras pareadas 

# Para esta prueba, supongamos ahora que el ingeniero en Informática del ejemplo anterior tiene dos algoritmos
# diferentes (A y B) que, en teoría, deberían tardar lo mismo en resolver un problema. Para ello, probó ambos
# algoritmos con 35 instancias del problema (elegidas al azar) de igual tamaño y registró los tiempos de
# ejecución (en milisegundos) de ambos algoritmos bajo iguales condiciones para cada una de ellas, además de
# calcular la diferencia en los tiempos de ejecución

# Pasos:
# 1- Plantear hipotesis
# 2- Verificar condiciones -> Se comprueba normalidad con  shapiro.test ( diferencia )
# 3- Calcular estimador
# 4- Calcular valor p
# 5- Compara valor p con alfa
# 6- Concluir

#Funcion que ejecuta automaticamente el test.
#prueba _1 <- t. test ( diferencia ,alternative = " two. sided ",mu = valor _ nulo ,conf . level = 1 - alfa )

# Prueba t para dos muestras independientes

# En este caso, la prueba t se usa para comparar las medias de dos poblaciones en que las observaciones con
# que se cuenta no tienen relación con ninguna de las otras observaciones, ni influyen en su selección, ni en la
# misma ni en la otra muestra. En este caso la inferencia se hace sobre la diferencia de las medias: u1 ??? u2 = d0,
# donde d0 es un valor hipotético fijo para la diferencia.

# Se deben verificar 2 condiciones 

# 1- Cada muestra cumple las condiciones para usar la distribución t.
# 2-  Las muestras son independientes entre sí.

# Pasos
# 1-Plantear hipotesis
# 2- Verificar condiciones -> Para verificar supuesto de normalidad usar shapiro . test (muestra) para cada muestra.
# 3- Calcular estimador T = ((media1-media2)-d0)/SE(media1-media2)
# 4- Calcular valor p
# 5- Comparar valor p con alfa
# 6- Concluir 

#Funcion que realiza automaticamente el test.
# prueba <- t. test (x = vacuna _A ,
#                      y = vacuna _B ,
#                      paired = FALSE ,
#                      alternative = " greater ",
#                      mu = 0 ,
#                      conf.level = 1 - alfa )



