
#semanas que tarda en andar un niño de media, muestra aleatoria con semilla de reproducibilidad 1
#se supone que por encima de 60 semanas se considera motivo para consultar a un especialista.
set.seed(1)
semanas_andar_m1=rnorm(10000, 54 , 3) #varones

# igual, con semilla de reproducibilidad 2 para la muestra segunda
set.seed(2)
semanas_andar_m2=rnorm(10000,54,3) #mujeres

#guardamos los datasets para verificar calculos cuando se necesite
setwd("C:/Users/rafah/OneDrive/Estudios/UNIR/MasterVisualAnalytics/Analisis Interpretacion Datos/lab 1") 
write.csv(semanas_andar_m1, file="semanas_andar_1.csv")
write.csv(semanas_andar_m2, file="semanas_andar_2.csv")
#se trata de una muestra aleatoria simple.
#histograma de la muestra 1
hist(semanas_andar_m1)

#histograma de la muestra 2
hist(semanas_andar_m2)

#grafica de cuantiles teóricos y reales de las muestras
qqnorm( semanas_andar_m1 ) # la nube de puntos 
qqline( semanas_andar_m1 ) # la recta

qqnorm( semanas_andar_m2 ) # la nube de puntos 
qqline( semanas_andar_m2 ) # la recta

#vemos la descriptiva
summary(semanas_andar_m1)
sd(semanas_andar_m1)  
summary(semanas_andar_m2)

#contamos el numero de valores por encima de 60 semanas en nuestra muestra 1
sum(semanas_andar_m1>60)
sum(semanas_andar_m1<100)

#contamos el numero de valores por encima de 60 semanas en nuestra muestra 2
sum(semanas_andar_m2>60)

#nuestra proporcion muestral será
p1_m = sum(semanas_andar_m1>60)/10000

#calculamos z para nuestra hipostesis inicial
z_m1 <- (p1_m - 0.02) / sqrt(0.02 * (1 - 0.02) / 10000)

# h0 p = 0.02 Para obtener el valor-P de la prueba debemos tener en cuenta el sentido en la hipótesis alternativa  
# H1 > 0.02
# por esa razón el valor-P será  
# y para obtenerlo usamos el siguiente código

#igual se puede calcular el valor mediante tabla a la izquierda de z y comprobar el resto ( para nuestro > de la hip. alternativa)
pnorm(q=z_m1, lower.tail = FALSE )  # Para obtener el valor-P, minúsculo por lo que se rechaza H0
#De hecho en la memoria se explica cómo se calculó  inicialmente con la tabla A2 del libro de Triola, Estadística. 


# Existe evidencia suficiente
# que justifica que el rechazo
# de la aseveración de que.
# la probabilidad sea menor del 2% por tanto la preocupacion de los padres esta justificada.





#ahora vamos a comprobar si los niños sufre más problemas que las niñas en proporcion
#nuestra hipótesis es que en la población de la muestra 1 
#la probabilidad de que tu hijo tarde más de 60 semanas en andar 
#es más alta que en la población de la muestra 2
# H0 p_mas = p_fem , H1 p_mas > p_fem     

#calculamos el estadístico de prueba para la proporción


#nuestra proporcion muestral será
p1_m = sum(semanas_andar_m1>60)/10000

#nuestra proporcion muestral será
p1_f = sum(semanas_andar_m2>60)/10000

p_med = (sum(semanas_andar_m1>60) + sum(semanas_andar_m2>60))/20000 #proporcion muestral agrupada
q_med = 1 - p_med

#calculamos z para nuestra hipostesis inicial H0 p_mas = p_fem (0.02)
z_mf <- (p1_m - p1_f)-(0.02 - 0.02) / sqrt((p_med * q_med / 10000) + (p_med * q_med / 10000))
#da un z de 0.00049 que corresponde a un P de 0.50 mucho mayor que nuestro nivel de significanza 0.05 (alfa)
# por lo que concluimos que no se rechaza H0, es decir, no hay evidencias para no determinar que afecta igual a niños que a niñas. 

#calculo computacional de p-value para la prueba de contraste de hipotesis con dos poblaciones:
pnorm(q=z_mf, lower.tail = FALSE )


