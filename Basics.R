#defaultW <- getOption("warn") 
#options(warn = -1) 

#Librerias importantes a importar 
library(dplyr)
library(ggpubr)
library(tidyr)


# Dataframe y medidas para variables numericas ----------------------------



#Crear dataframe
col1 = c(1,2,3,4,5,6)
col2 = c(7,8,9,10,11,12)

frame <- data.frame(col1,col2)

#Añadir nueva columna al dataframe
col3 <- c(13,14,15,16,17,18)

frame$col3 <- col3

#Añadir nueva fila
nuevaObs <- data.frame(col1=999,col2=998,col3=997)

frame <- rbind(frame,nuevaObs)

#filter() -> filtra por filas
#select() -> selecciona columnas
#arrange() -> modifica el orden de las filas

#Ejemplo de arrange
frame <- arrange(frame,desc(col1))

#Renombrar columnas
frame <- rename(frame,Columna1=col1,Columna2=col2,Columna3=col3)

datos <- mtcars

datos <- datos %>% rename ( Rendimiento = mpg , Cilindrada = cyl ,
                             Desplazamiento = disp , Potencia = hp ,
                             Eje = drat , Peso = wt , Cuarto_milla = qsec ,
                             Motor = vs , Transmision = am , Cambios = gear ,
                             Carburadores = carb )

#A la variable Motor se le da formato categorico y se renombran sus niveles
datos[["Motor"]] <- factor ( datos [["Motor"]] , levels = c(0 , 1) ,
                                labels = c("V", "Recto") )


#Para calcular medidas estadisticas
# mean(datos)
# mean(datos,na.rm) -> omite valores faltantes

#Calculo de cuantiles

#Quintiles
#quantile(datos, seq(0,1,0.2))

#Deciles
#quantile(datos,seq(0,1,0.1))

#Percentiles
#quantile(datos,seq(0,1,0.01))

#Cuartiles
#quantile(datos)

#Calculo de varias medidas con ayuda de la funcion summarise
medidas <- summarise(datos,Media=mean(Potencia),
                           Mediana=median(Potencia),
                           Varianza=var(Potencia),
                           IQR = IQR(Potencia),
                           Desviacion=sd(Potencia))
print(medidas)


# Medidas para variables categoricas --------------------------------------

#Tabla de contingencia
contingencia <- table ( datos [["Cambios"]])

#Calcula los totales por fila de una tabla de contingencia
totales <- marginSums ( contingencia )

#Calcula los totales por fila y los añade a la tabla
con_totales <- addmargins (contingencia , 1)

# Convertir a tabla de proporciones
proporciones <- prop.table ( contingencia )
proporciones <- addmargins ( proporciones , 1)

#Agrupar datos
agrupar <- group_by(datos,Cambios)

print(agrupar)













# Graficos ----------------------------------------------------------------

# Histograma para la variable Rendimiento (Variable numerica.
g1 <- gghistogram (datos ,
                   x = "Rendimiento",
                   bins = 10 ,
                   add = "mean",
                   xlab = " Rendimiento [ Millas /galón]",
                   ylab = " Frecuencia ",
                   color = " blue ",
                   fill = " blue ")
print(g1)

#Datos desviados a la izquierda = asimetria negativa
#Datos desviados a la derecha = simetrico


#Grafico de cajas
g <- ggboxplot ( datos [["Potencia"]] ,
                 color = "red",
                 fill = "pink",
                 ylab = " Potencia [hp]")

g <- g + rremove ("x.ticks")
g <- g + rremove ("x.text")
g <- g + rremove ("x.title")

print ( g )


# Crear el gráico de barras para 1 variable categorica (Se hace con tabla de contingencia.

contingencia <- as.data.frame ( xtabs (~ Cambios , data = datos ) )


g <- ggbarplot ( contingencia ,
                    x = "Cambios",
                    y = "Freq",
                    fill = c("brown", "purple", "orange") ,
                    title = "Cantidad de cambios de los automóviles",
                    xlab = "Cantidad de cambios",
                    ylab =  "Frecuencia")

print ( g )

#Para representar 1 variable categoria ademas del grafico de barras tambien sirve el grafico de pastel.

#Graficos para dos variables numericas

# Crear gráfico de dispersión -> sirven para identificar si dos variables estan relacionadas.
g <- ggscatter ( datos ,
                   x = "Rendimiento",
                   y = "Peso",
                   color = "red",
                   title = "Rendimiento v/s peso",
                   xlab = "Rendimiento [ millas /gal ón]",
                   ylab = "Peso [1000 lb]")

print ( g )

#Crear figura con multiples graficos
#g <- ggarrange ( g1 , g2 ,g3 , ncol = 3 , nrow = 1 , common . legend = TRUE )

#Graficos para dos variables categoricas -> se requiere tabla de contingencia guardada como dataframe.
tabla <- xtabs (~ Motor + Cambios , data = datos )

contingencia <- as.data.frame ( tabla )

# Crear gráfico de barras segmentadas .
g1 <- ggplot ( contingencia , aes ( fill = Motor , y = Freq , x = Cambios ) )
g1 <- g1 + geom_bar ( position = "stack", stat = "identity")
g1 <- g1 + labs ( y = " Frecuencia ") + ggtitle (" Barras apiladas ")
g1 <- g1 + theme_pubr ()

# Crear grá fico de barras agrupadas .
g2 <- ggplot ( contingencia , aes ( fill = Motor , y = Freq , x = Cambios ) )
g2 <- g2 + geom_bar ( position = "dodge", stat = "identity")
g2 <- g2 + labs ( y = " Frecuencia ") + ggtitle (" Barras agrupadas ")
g2 <- g2 + theme_pubr ()

# Crear grá fico de barras segmentadas estandarizado .
g3 <- ggplot ( contingencia , aes ( fill = Motor , y = Freq , x = Cambios ) )
g3 <- g3 + geom_bar ( position = "fill", stat = "identity")
g3 <- g3 + labs ( y = " Frecuencia ") + ggtitle (" Barras estandarizadas ")
g3 <- g3 + theme_pubr ()

g <- ggarrange ( g1 , g2 ,g3 , ncol = 3 , nrow = 1 , common.legend = TRUE )
print(g)

#Guardar el grafico en el dispositivo
ggexport (g , filename = "./Grafico Barras 2 variables categoricas.png",
            height = 480 , width = 960)

#Grafico para una variable numerica y otra categorica
g <- ggboxplot (datos, x = "Cambios",
                 y = "Rendimiento",
                 palette = c("light blue", "pink", "yellow") ,
                 fill = "Cambios",
                 title = "Rendimiento por cantidad de cambios",
                 xlab = "Cambios",
                 ylab = "Rendimiento [ millas /galón]")

print ( g )




# Variables aleatorias -----------------------------------

library(discreteRV)

resultados <- 1:6
probabilidades = c (0.25 , 0.125 , 0.125 , 0.125 , 0.125 , 0.25)
X <-  RV ( outcomes = resultados , probs = probabilidades )

#Calculo valor esperado
esperado <- E(X)

#Calculo varianza
varianza <- V(X)

#Calculo desviacion
desviacion <- SD(X)

#Experimento de lanzar 20 dados
lanzar_20 <- SofIID (X , n =20)



# Distribuciones Continuas----------------------------------------------------------

# Distribucion normal
# Simetrica - unimodal - con forma de campana
# Se puede ajustar mediante dos parametros: media y desviacion 

# Generar valores para una distribuci ón normal con media 0 y
# desviaci ón está ndar 1.
media <- 0
desv_est <- 1
x <- seq ( -15 , 35 , 0.01)
y <- dnorm (x , mean = media , sd = desv_est )
normal_1 <- data.frame (x , y )

# - pnorm(q,mean,sd,lower.tail) = probabilidad de que la variable tome valores menores o igual a un valor dado.
# - qnorm(p,mean,sd.lower.tail) = encuentra el percentil para las probabilidades dadas en p.
# - rnorm(n,mean,sd) = genera aleatoriamente n observaciones de la distribucion normal especificada.

# Regla empirica
# [u-sd,u+sd] -> 68% de las observaciones
# [u-2sd,u+2sd] -> 95% de las observaciones
# [u-3sd,u+3sd] -> 99.7% de las observaciones

# Grafico q-q para saber si una muestra se distribuye de forma cercana a la normal.

g <- ggqqplot(datos ,x = "Rendimiento",color="red")
print(g)


#Distribucion Z
# Tecnica de estandarizacion para determinar que tan inusual es un determinado valor en una escala unica.

#Valor z
# z <- (valor-media)/sd, > 0 esta por sobre la media - < 0 esta por debajo de la media

#Distribucion chi cuadrado
# Se usa para caracterizar valores siempre positivos y habitualmente desviados a la derecha
# media = grados de libertad
# sd = 2*grados de libertad

# - dchisq(x,df)
# - pchisq(q, df, lower.tail)
# - qchisq(p, df, lower.tail)
# - rchisq(n, df)


#Distribucion t de student
# A medida que los grados de libertad aumentan, esta distribución se asemeja cada vez más a la normal, aunque
# sus colas son más gruesas

# - dt(x, df)
# - pt(q, df, lower.tail)
# - qt(p, df, lower.tail).
# - rt(n, df)

#Distribucion F
#Ampliamente usada para comparar varianzas

# - df(x, df1, df2).
# - pf(q, df1, df2, lower.tail)
# - qf(p, df1, df2, lower.tail)
# - rf(n, df1, df2)

# Donde df1 como df2 corresponden a grados de libertad




# Distribuciones Discretas ------------------------------------------------

#Distribucion de Bernoulli 
#Es aquella en que cada intento individual tiene solo dos resultados
#posibles: "éxito", que ocurre con una probabilidad p y se representa habitualmente con un 1, y "fracaso", que
#ocurre con probabilidad q = 1 ??? p y suele representarse por un 0.


# - dbern(x, prob)
# - pbern(q, prob, lower.tail).
# - qbern(p, pro, lower.tail).
# - rbern(n, prob).


#Distribucion Geometrica
#La distribución geométrica describe la cantidad de intentos que debemos realizar hasta obtener un éxito
#para variables de Bernoulli independientes e idénticamente distribuidas

# - dgeom(x, prob).
# - pgeom(q, prob, lower.tail).
# - qgeom(p, prob, lower.tail).
# - rbern(n, prob).

#Distribucion Binomial
#A diferencia de la distribución geométrica, la distribución binomial describe la probabilidad de tener
#exactamente k éxitos en n intentos independientes de Bernoulli con probabilidad de éxito p


# Se deben verificar 4 condiciones:
#   - Los intentos son independientes
#   - La cantidad de intentos (n) es fija
#   - El resultado de cada intento puede ser clasificado como éxito o fracaso.
#   - La probabilidad de éxito (p) es la misma para cada intento

# - dbinom(x, size, prob)
# - pbinom(x, size, prob)
# - qbinom(p, size, prob)
# - rbinom(n, size, prob)


#Distribución binomial negativa
#La distribución binomial negativa es algo más general que la binomial, pues describe la probabilidad de
#encontrar el k-ésimo éxito al n-ésimo intento. 

#Debemos verficiar 4 condiciones para poder usarla:

#   - Los intentos son independientes.
#   - El resultado de cada intento puede ser clasificado como éxito o fracaso.
#   - La probabilidad de éxito (p) es la misma para cada intento.
#   - El último intento debe ser un éxito.

# - dnbinom(x, size, prob).
# - pnbinom(q, size, prob, lower.tail).
# - qnbinom(p, size, prob, lower.tail).
# - rnbinom(n, size, prob).

#Distribución de Poisson
#Útil para estimar la cantidad de eventos en una población grande en un lapso de tiempo dado, por ejemplo,
#la cantidad de contagios de influenza entre los habitantes de Santiago en una semana

# - dpois(x, lambda).
# - ppois(q, lambda, lower.tail).
# - qqpois(p, lambda, lower.tail).
# - rpois(n, lambda).

