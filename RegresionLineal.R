setwd("~/Documents/MaestriaDatos/GLM/ProyectoFinal")
setwd("~/Google Drive/proyectoFinalConcreto")
tabla_original<-read.csv("Concrete_Data.csv",header=FALSE)
names(tabla_original)<-c("cement",
                         "BlastFurnaceSlag",
                         "FlyAsh",
                         "Water",
                         "Superplasticizer",
                         "CoarseAggregate",
                         "FineAggregate",
                         "Age",
                         "ConcreteCompressiveStrength")
df<-data.frame(tabla_original)
df2<-df
df2$Age<-log(df2$Age)
df2<-data.frame(scale(df))

regresion.lineal<-lm(df2$ConcreteCompressiveStrength~.,data=df2)
regresion.lineal.predichas<-regresion.lineal$fitted.values
or<-order(df2$ConcreteCompressiveStrength)
plot(df2$ConcreteCompressiveStrength[or])
plot(df2$ConcreteCompressiveStrength,regresion.lineal.predichas)

ridge.cv<-cv.glmnet(x=as.matrix(df2[,-9]),y=df2[,9],alpha=0,standardize=FALSE)
ridge.optimo<-glmnet(x=as.matrix(df2[,-9]),y=df2[,9],lambda= ridge.cv$lambda.min,alpha=0,standardize=FALSE)
prediccion.ridge<-predict(ridge.optimo,newx=as.matrix(df2[,-9]))
plot(df2$ConcreteCompressiveStrength,prediccion.ridge)

lasso.cv<-cv.glmnet(x=as.matrix(df2[,-9]),y=df2[,9],alpha=1)
lasso.optimo<-glmnet(x=as.matrix(df2[,-9]),y=df2[,9],lambda= lasso.cv$lambda.min,alpha=1,standardize=FALSE)
prediccion.lasso<-predict(lasso.optimo,newx=as.matrix(df2[,-9]))
plot(df2$ConcreteCompressiveStrength,prediccion.lasso)

par(mfrow=c(3,1)) 
plot(df$ConcreteCompressiveStrength,regresion.lineal$fitted.values)




