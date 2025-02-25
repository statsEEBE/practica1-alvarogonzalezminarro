#Codigo para problema 2
mis_dades <- iris
mis_dades
#esto nos sirve para que nos lo muestre 
#nos muestra características de cada flor
# para ver media de los cepalos de la longitud hacemos:
mean(mis_dades$Sepal.Length)
#cual es la dispersion
#variabilidad respecto la media, desviación tipica haremos:
sd(mis_dades$Sepal.Length)
dim(mis_dades)
#150 flores 5 variables
names(mis_dades)
#vemos cada característica
hist((mis_dades$Sepal.Length))
#para hacer grafica
#recta de regresión por minimos cuadrados
#petallenght es x, mas grande el petalo mas grande el cepalo
x<-mis_dades$Petal.Length
#y es el cepalo
y<-mis_dades$Sepal.Length
plot(x,y)
#para ver los valores de x e y
#teniendo valor de x puedo predecir el de y? NO
# me creo recta que mas se aproxime mejor a los puntos, la media
# y'(valor predicho) = mx + b
#Q(distancia)=sumatorio desde i igual a 1 hasta n de(y-y')^2)
#recta que minimiza Q
# Q(m,b) = smatorio desde i igual a 1 hasta n de (y-(mx + b))^2)
# minimizo la función haciendo la derivada = a 0
# dQ/dm  =  0
# dQ/db  =  0
# m = (sumatorio de ((x-media)(y-ymedia))))/ (sumatorio de (x-xmedia)^2))
#trabajamos de adentro hacia afuera, x es un vector
# media de x
mean(x)
#hago x menos la media y lo multiplico por y menos la media de y
sum( (x-mean(x))*(y-mean(y)) )/sum( (x-mean(x))^2)
m<-sum( (x-mean(x))*(y-mean(y)) )/sum( (x-mean(x))^2)
b<- mean(y)-m*mean(x)
# pongo b y m abajo para que me lo muestre 
#funcion sum para sumar los elementos del vector con parentesis
# prediccion para longitudes de 1.5
m*1.5+b
#para dibujar la recta, diferentes predicciones a diferentes puntos
#funcion lm para predecir y en terminos de x, alt gr +4 para el gusanito mas espacio
lm(y~x)
#mod es el objeto que sale de un lm
mod <- lm(y~x)
summary(mod)
# le puedo decir que haga una prediccion sobre el modelo los valores de x
xpred <- data.frame(x=1.5)
# data frame es base de datos de una fila y una columna 
predict(mod, xpred)
# ahora predecimos un rango de valores
xpred <-data.frame(x=1:7)
predict(mod,xpred)
# que pasa si llamo prediccion de x, valor predicho para cada uno de los valores de x
xpred <- data.frame(x=x)
xpred
ypred<-predict(mod,xpred)
ypred
plot(x,y)
