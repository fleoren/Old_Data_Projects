require(tseries)
require(forecast)
require(fUnitRoots)
data(USeconomic)
x=log(M1); x2=diff(x)
tsdisplay(x)
adfTest(x,type='c') #c media distinta de cero t tendencia creciente nc ninguna
#la nula de dickey-fuller es que s?? es un paseo aleatorio.
tsdisplay(x2)
model1<-arma(x2,c(1,0))
tsdisplay(model1$residuals)

model2<-arma(x2,c(0,4)) #m4 muy poco significativo
tsdisplay(model2$residuals)
summary(model2)

model3<-arma(x2,c(0,3)) #veamos si se puede quitar m3
tsdisplay(model3$residuals)
summary(model3)

model4<-arma(x2,c(0,2)) #sale palito en dependencia orden 3. si hay que poner ese parametro
tsdisplay(model4$residuals)
summary(model4)

#diferencias y ajustar un promedio movil de orden 3 u autorregresivo orden 1 ARIMA(0,1,3) o (0,1,0)

w<-WWWusage
tsdisplay(w)
automodelw<-auto.arima(w,max.p=5,max.q=5)
summary(automodelw)
tsdisplay(automodelw$residuals)
adfTest(w,type='c') #porque no parece tener tendencia

w2=diff(w)
tsdisplay(w2)
modeloamano<-arma(w2,c(3,0))
tsdisplay(modeloamano$residuals)
summary(modeloamano)

#en terminos predictivos, arma(1,1)
#en terminos explicativos, ar(3)

data(NelPlo)
tsdisplay(emp)
adfTest(emp,type='ct')
kpss.test(emp,'Trend') #invierte la hipotesis nula
#rechazamos, entonces ya podemos diferenciar
tsdisplay(diff(emp))
modelo5=arima(emp,c(1,1,1))
summary(modelo5)

modelo6=arima(emp,c(0,1,0)) #no nos ayuda verlo como ruido blanco

modelo7=arima(emp,c(0,1,1)) #modelo AR(1)
tsdisplay(modelo7$residuals)

modelo8=arima(emp,c(1,1,0)) #modelo MA(1)
tsdisplay(modelo8$residuals)

summary(modelo8)

dm.test(modelo7$residuals, modelo8$residuals,'two.sided',h=3) #compara dos modelos, "predice" de mentiritas
#la nula es que los dos tienen el mismo poder de prediccion.
#tenemos dos modelos distintos que tienen el mismo poder
#para explicar, conviene el modelo AR(1)

automodelemp<-auto.arima(w,max.p=5,max.q=5)
summary(automodelemp)
tsdisplay(automodelemp$residuals)
